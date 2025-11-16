%%%-------------------------------------------------------------------
%%% @doc Translation Service Application
%%% Real-time message translation service
%%% @end
%%%-------------------------------------------------------------------
-module(translation_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Translation Service"),

    {ok, Port} = application:get_env(translation_service, port),
    {ok, NumAcceptors} = application:get_env(translation_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/translate", translation_handler, #{action => translate}},
            {"/health", translation_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        translation_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Translation Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    translation_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(translation_http_listener),
    ok.
