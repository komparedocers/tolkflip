%%%-------------------------------------------------------------------
%%% @doc WebRTC HTTP Handler
%%% Handles HTTP requests for WebRTC service
%%% @end
%%%-------------------------------------------------------------------
-module(webrtc_http_handler).

-export([init/2]).

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"webrtc-service">>}),
        Req),
    {ok, Req2, State}.
