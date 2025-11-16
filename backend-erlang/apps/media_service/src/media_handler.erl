%%%-------------------------------------------------------------------
%%% @doc Media Service HTTP Handler
%%% Handles file upload and retrieval
%%% @end
%%%-------------------------------------------------------------------
-module(media_handler).

-export([init/2]).

init(Req0, #{action := upload} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            %% Read multipart data
            {ok, Headers, Req1} = cowboy_req:read_part(Req0),
            {ok, Data, Req2} = cowboy_req:read_part_body(Req1),

            %% Extract filename and content type
            {_, Disposition} = lists:keyfind(<<"content-disposition">>, 1, Headers),
            FileName = extract_filename(Disposition),
            ContentType = case lists:keyfind(<<"content-type">>, 1, Headers) of
                {_, CT} -> CT;
                false -> <<"application/octet-stream">>
            end,

            %% Generate media ID
            MediaId = generate_media_id(),

            %% Store file metadata in Cassandra
            {ok, _} = tolkflip_cassandra:save_media(
                MediaId,
                binary_to_list(FileName),
                binary_to_list(ContentType),
                byte_size(Data)
            ),

            %% In production, upload to MinIO/S3
            %% For now, store URL reference
            MediaUrl = <<"https://storage.tolkflip.com/", MediaId/binary>>,

            %% TODO: Implement actual MinIO upload
            %% upload_to_minio(MediaId, Data, ContentType),

            Reply = jsx:encode(#{
                <<"success">> => true,
                <<"mediaId">> => MediaId,
                <<"url">> => MediaUrl,
                <<"fileName">> => FileName,
                <<"fileSize">> => byte_size(Data)
            }),
            Req3 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req2),
            {ok, Req3, State};
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := get_media} = State) ->
    MediaId = cowboy_req:binding(media_id, Req),

    case tolkflip_cassandra:get_media(binary_to_list(MediaId)) of
        {ok, MediaInfo} ->
            %% In production, fetch from MinIO/S3
            MediaUrl = <<"https://storage.tolkflip.com/", MediaId/binary>>,

            Reply = jsx:encode(#{
                <<"mediaId">> => MediaId,
                <<"url">> => MediaUrl,
                <<"fileName">> => maps:get(<<"file_name">>, MediaInfo),
                <<"contentType">> => maps:get(<<"content_type">>, MediaInfo),
                <<"fileSize">> => maps:get(<<"file_size">>, MediaInfo)
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {error, not_found} ->
            Req2 = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Media not found">>}), Req),
            {ok, Req2, State};
        {error, Reason} ->
            lager:error("Failed to get media ~s: ~p", [MediaId, Reason]),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Internal server error">>}), Req),
            {ok, Req2, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"media-service">>}),
        Req),
    {ok, Req2, State}.

%% Helper functions
generate_media_id() ->
    UUID = uuid:get_v4(),
    uuid:uuid_to_string(UUID, binary_standard).

extract_filename(Disposition) ->
    case binary:split(Disposition, <<"filename=">>, [global]) of
        [_, FileNamePart | _] ->
            %% Remove quotes and whitespace
            FileName = binary:replace(FileNamePart, <<"\"">>, <<>>, [global]),
            hd(binary:split(FileName, <<";">>));
        _ ->
            <<"unknown">>
    end.
