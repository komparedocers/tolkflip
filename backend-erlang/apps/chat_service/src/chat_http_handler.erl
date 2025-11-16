%%%-------------------------------------------------------------------
%%% @doc HTTP Handler for Chat Service
%%% @end
%%%-------------------------------------------------------------------
-module(chat_http_handler).

-export([init/2]).

init(Req, #{action := health} = State) ->
    Reply = jsx:encode(#{
        <<"status">> => <<"ok">>,
        <<"service">> => <<"chat-service">>
    }),

    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Reply,
        Req
    ),

    {ok, Req2, State}.
