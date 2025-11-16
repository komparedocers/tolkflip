%%%-------------------------------------------------------------------
%%% @doc Message Store Service Application
%%% Message persistence and retrieval service
%%% @end
%%%-------------------------------------------------------------------
-module(message_store_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Message Store Service"),

    {ok, Port} = application:get_env(message_store_service, port),
    {ok, NumAcceptors} = application:get_env(message_store_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/messages", message_store_handler, #{action => save_message}},
            {"/api/messages/:thread_id", message_store_handler, #{action => get_messages}},
            {"/health", message_store_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        message_store_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Message Store Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    message_store_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(message_store_http_listener),
    ok.
