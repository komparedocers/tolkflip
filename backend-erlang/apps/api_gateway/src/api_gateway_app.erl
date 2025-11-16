%%%-------------------------------------------------------------------
%%% @doc API Gateway Application
%%% Routes and load balances requests to backend services
%%% @end
%%%-------------------------------------------------------------------
-module(api_gateway_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting API Gateway"),

    {ok, Port} = application:get_env(api_gateway, port),
    {ok, NumAcceptors} = application:get_env(api_gateway, num_acceptors),

    %% Define routes - proxy to backend services
    Dispatch = cowboy_router:compile([
        {'_', [
            %% Auth Service (3001)
            {"/request-code", gateway_handler, #{service => auth, path => "/request-code"}},
            {"/verify-code", gateway_handler, #{service => auth, path => "/verify-code"}},
            {"/register", gateway_handler, #{service => auth, path => "/register"}},
            {"/login", gateway_handler, #{service => auth, path => "/login"}},
            {"/refresh", gateway_handler, #{service => auth, path => "/refresh"}},

            %% User Service (3002)
            {"/api/users/:user_id", gateway_handler, #{service => user, path_pattern => "/api/users/:user_id"}},
            {"/api/users/:user_id/profile", gateway_handler, #{service => user, path_pattern => "/api/users/:user_id/profile"}},
            {"/api/users/:user_id/languages", gateway_handler, #{service => user, path_pattern => "/api/users/:user_id/languages"}},

            %% Chat Service (3003) - WebSocket
            {"/chat", gateway_ws_handler, #{service => chat}},

            %% Translation Service (3004)
            {"/api/translate", gateway_handler, #{service => translation, path => "/api/translate"}},

            %% Transcription Service (3005)
            {"/api/transcribe", gateway_handler, #{service => transcription, path => "/api/transcribe"}},

            %% Media Service (3006)
            {"/api/media/upload", gateway_handler, #{service => media, path => "/api/media/upload"}},
            {"/api/media/:media_id", gateway_handler, #{service => media, path_pattern => "/api/media/:media_id"}},

            %% Presence Service (3007)
            {"/api/presence/:user_id", gateway_handler, #{service => presence, path_pattern => "/api/presence/:user_id"}},
            {"/api/presence/:user_id/status", gateway_handler, #{service => presence, path_pattern => "/api/presence/:user_id/status"}},

            %% Message Store Service (3008)
            {"/api/messages", gateway_handler, #{service => message_store, path => "/api/messages"}},
            {"/api/messages/:thread_id", gateway_handler, #{service => message_store, path_pattern => "/api/messages/:thread_id"}},

            %% Notification Service (3009)
            {"/api/notifications/send", gateway_handler, #{service => notification, path => "/api/notifications/send"}},

            %% WebRTC Service (3010) - WebSocket
            {"/webrtc", gateway_ws_handler, #{service => webrtc}},

            %% Group Service (3011)
            {"/api/groups", gateway_handler, #{service => group, path => "/api/groups"}},
            {"/api/groups/:group_id", gateway_handler, #{service => group, path_pattern => "/api/groups/:group_id"}},
            {"/api/groups/:group_id/members", gateway_handler, #{service => group, path_pattern => "/api/groups/:group_id/members"}},
            {"/api/groups/:group_id/members/:user_id", gateway_handler, #{service => group, path_pattern => "/api/groups/:group_id/members/:user_id"}},

            %% Health check
            {"/health", gateway_health_handler, []}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        gateway_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("API Gateway listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    api_gateway_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(gateway_http_listener),
    ok.
