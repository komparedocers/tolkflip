%%%-------------------------------------------------------------------
%%% @doc User Service Application
%%% User profile management service
%%% @end
%%%-------------------------------------------------------------------
-module(user_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting User Service"),

    {ok, Port} = application:get_env(user_service, port),
    {ok, NumAcceptors} = application:get_env(user_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/users/:user_id", user_handler, #{action => get_user}},
            {"/api/users/:user_id/profile", user_handler, #{action => update_profile}},
            {"/api/users/:user_id/languages", user_handler, #{action => update_languages}},
            {"/health", user_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        user_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("User Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    user_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(user_http_listener),
    ok.
