%%%-------------------------------------------------------------------
%%% @doc WebRTC Service Application
%%% WebSocket-based signaling for voice/video calls
%%% @end
%%%-------------------------------------------------------------------
-module(webrtc_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting WebRTC Service"),

    {ok, Port} = application:get_env(webrtc_service, port),
    {ok, NumAcceptors} = application:get_env(webrtc_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", webrtc_ws_handler, []},
            {"/health", webrtc_http_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        webrtc_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("WebRTC Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    webrtc_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(webrtc_http_listener),
    ok.
