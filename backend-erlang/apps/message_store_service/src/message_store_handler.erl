%%%-------------------------------------------------------------------
%%% @doc Message Store Service HTTP Handler
%%% Handles message persistence operations
%%% @end
%%%-------------------------------------------------------------------
-module(message_store_handler).

-export([init/2]).

init(Req0, #{action := save_message} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            {ok, Body, Req} = cowboy_req:read_body(Req0),
            Data = jsx:decode(Body, [return_maps]),

            ThreadId = maps:get(<<"threadId">>, Data),
            SenderId = maps:get(<<"senderId">>, Data),
            ReceiverId = maps:get(<<"receiverId">>, Data),
            Content = maps:get(<<"content">>, Data),
            MessageType = maps:get(<<"messageType">>, Data, <<"text">>),
            OriginalLanguage = maps:get(<<"originalLanguage">>, Data, <<"en">>),
            IsGroup = maps:get(<<"isGroup">>, Data, false),

            SaveResult = tolkflip_cassandra:save_message(
                binary_to_list(ThreadId),
                binary_to_list(SenderId),
                binary_to_list(ReceiverId),
                binary_to_list(MessageType),
                binary_to_list(Content),
                binary_to_list(OriginalLanguage),
                "sent",
                IsGroup
            ),

            case SaveResult of
                {ok, MessageId} ->
                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"messageId">> => list_to_binary(MessageId)
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    lager:error("Failed to save message: ~p", [Reason]),
                    Req2 = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{<<"error">> => <<"Failed to save message">>}), Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := get_messages} = State) ->
    ThreadId = cowboy_req:binding(thread_id, Req),
    QsVals = cowboy_req:parse_qs(Req),
    Limit = case proplists:get_value(<<"limit">>, QsVals) of
        undefined -> 50;
        LimitBin -> binary_to_integer(LimitBin)
    end,

    case tolkflip_cassandra:get_messages(binary_to_list(ThreadId), Limit) of
        {ok, Messages} ->
            Reply = jsx:encode(#{<<"messages">> => Messages}),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {error, Reason} ->
            lager:error("Failed to get messages for thread ~s: ~p", [ThreadId, Reason]),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Failed to retrieve messages">>}), Req),
            {ok, Req2, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"message-store-service">>}),
        Req),
    {ok, Req2, State}.
