%%%-------------------------------------------------------------------
%%% @doc Presence Service Application
%%% Real-time user presence tracking service
%%% @end
%%%-------------------------------------------------------------------
-module(presence_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Presence Service"),

    {ok, Port} = application:get_env(presence_service, port),
    {ok, NumAcceptors} = application:get_env(presence_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/presence/:user_id", presence_handler, #{action => get_presence}},
            {"/api/presence/:user_id/status", presence_handler, #{action => update_status}},
            {"/health", presence_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        presence_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Presence Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    presence_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(presence_http_listener),
    ok.
