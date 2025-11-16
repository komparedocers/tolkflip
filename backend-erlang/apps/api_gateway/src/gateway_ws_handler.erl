%%%-------------------------------------------------------------------
%%% @doc API Gateway WebSocket Handler
%%% Proxies WebSocket connections to backend services
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_ws_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
    service :: atom(),
    backend_pid :: pid() | undefined
}).

-define(SERVICE_PORTS, #{
    chat => 3003,
    webrtc => 3010
}).

init(Req, Opts) ->
    Service = maps:get(service, Opts),
    {cowboy_websocket, Req, #state{service = Service}}.

websocket_init(#state{service = Service} = State) ->
    %% In production, establish WebSocket connection to backend service
    %% For now, log connection
    lager:info("Gateway WebSocket established for service: ~p", [Service]),
    {ok, State}.

websocket_handle(Frame, State) ->
    %% Forward frame to backend service
    %% In production, implement actual WebSocket proxying
    lager:debug("Gateway WebSocket received frame: ~p", [Frame]),
    {ok, State}.

websocket_info(Info, State) ->
    %% Handle info from backend connection
    lager:debug("Gateway WebSocket info: ~p", [Info]),
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
