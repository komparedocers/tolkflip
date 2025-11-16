%%%-------------------------------------------------------------------
%%% @doc Group Service Application
%%% Group chat management service
%%% @end
%%%-------------------------------------------------------------------
-module(group_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Group Service"),

    {ok, Port} = application:get_env(group_service, port),
    {ok, NumAcceptors} = application:get_env(group_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/groups", group_handler, #{action => create_group}},
            {"/api/groups/:group_id", group_handler, #{action => get_group}},
            {"/api/groups/:group_id/members", group_handler, #{action => add_member}},
            {"/api/groups/:group_id/members/:user_id", group_handler, #{action => remove_member}},
            {"/health", group_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        group_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Group Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    group_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(group_http_listener),
    ok.
