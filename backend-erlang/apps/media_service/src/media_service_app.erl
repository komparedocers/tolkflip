%%%-------------------------------------------------------------------
%%% @doc Media Service Application
%%% File upload and storage service
%%% @end
%%%-------------------------------------------------------------------
-module(media_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Media Service"),

    {ok, Port} = application:get_env(media_service, port),
    {ok, NumAcceptors} = application:get_env(media_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/media/upload", media_handler, #{action => upload}},
            {"/api/media/:media_id", media_handler, #{action => get_media}},
            {"/health", media_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        media_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Media Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    media_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(media_http_listener),
    ok.
