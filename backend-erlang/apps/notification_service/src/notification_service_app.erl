%%%-------------------------------------------------------------------
%%% @doc Notification Service Application
%%% Push notification service
%%% @end
%%%-------------------------------------------------------------------
-module(notification_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Notification Service"),

    {ok, Port} = application:get_env(notification_service, port),
    {ok, NumAcceptors} = application:get_env(notification_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/notifications/send", notification_handler, #{action => send}},
            {"/health", notification_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        notification_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Notification Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    notification_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(notification_http_listener),
    ok.
