%%%-------------------------------------------------------------------
%%% @doc Chat Service Application
%%% High-performance WebSocket chat with massive concurrency
%%% @end
%%%-------------------------------------------------------------------
-module(chat_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting Chat Service"),

    {ok, Port} = application:get_env(chat_service, port),
    {ok, NumAcceptors} = application:get_env(chat_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", chat_ws_handler, []},
            {"/health", chat_http_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP/WebSocket listener
    {ok, _} = cowboy:start_clear(
        chat_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{
            env => #{dispatch => Dispatch},
            idle_timeout => infinity,  % Keep WebSocket connections alive
            max_connections => infinity
        }
    ),

    lager:info("Chat Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    chat_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(chat_http_listener),
    ok.
