%%%-------------------------------------------------------------------
%%% @doc Auth Service Application
%%% High-performance authentication service using Cowboy
%%% @end
%%%-------------------------------------------------------------------
-module(auth_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:info("Starting Auth Service"),

    {ok, Port} = application:get_env(auth_service, port),
    {ok, NumAcceptors} = application:get_env(auth_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/request-code", auth_handler, #{action => request_code}},
            {"/verify-code", auth_handler, #{action => verify_code}},
            {"/register", auth_handler, #{action => register}},
            {"/login", auth_handler, #{action => login}},
            {"/refresh", auth_handler, #{action => refresh}},
            {"/health", auth_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        auth_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Auth Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    auth_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(auth_http_listener),
    ok.
