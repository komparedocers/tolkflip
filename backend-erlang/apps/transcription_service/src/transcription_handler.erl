%%%-------------------------------------------------------------------
%%% @doc Transcription Service HTTP Handler
%%% Handles audio transcription requests
%%% @end
%%%-------------------------------------------------------------------
-module(transcription_handler).

-export([init/2]).

init(Req0, #{action := transcribe} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            {ok, Body, Req} = cowboy_req:read_body(Req0),
            Data = jsx:decode(Body, [return_maps]),

            AudioUrl = maps:get(<<"audioUrl">>, Data),
            Language = maps:get(<<"language">>, Data, <<"en">>),

            %% Check cache first
            CacheKey = crypto:hash(md5, AudioUrl),
            CacheKeyStr = binary:encode_hex(CacheKey),

            case tolkflip_redis:get(<<"transcription:", CacheKeyStr/binary>>) of
                {ok, CachedTranscription} when CachedTranscription =/= undefined ->
                    %% Return cached transcription
                    Reply = jsx:encode(#{
                        <<"text">> => CachedTranscription,
                        <<"language">> => Language,
                        <<"cached">> => true
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State};
                _ ->
                    %% Perform transcription (mock implementation)
                    TranscribedText = mock_transcribe(AudioUrl, Language),

                    %% Cache result (24 hour TTL)
                    tolkflip_redis:setex(
                        <<"transcription:", CacheKeyStr/binary>>,
                        86400,
                        TranscribedText
                    ),

                    %% Save to database
                    {ok, TranscriptionId} = tolkflip_cassandra:save_transcription(
                        binary_to_list(AudioUrl),
                        binary_to_list(TranscribedText),
                        binary_to_list(Language)
                    ),

                    Reply = jsx:encode(#{
                        <<"id">> => list_to_binary(TranscriptionId),
                        <<"text">> => TranscribedText,
                        <<"language">> => Language,
                        <<"cached">> => false
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"transcription-service">>}),
        Req),
    {ok, Req2, State}.

%% Mock transcription function - replace with real API integration
mock_transcribe(_AudioUrl, _Language) ->
    %% TODO: Integrate with Google Speech-to-Text, Whisper, or similar
    <<"This is a transcribed audio message">>.
