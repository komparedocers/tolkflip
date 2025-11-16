%%%-------------------------------------------------------------------
%%% @doc Transcription Service Application
%%% Audio-to-text transcription service
%%% @end
%%%-------------------------------------------------------------------
-module(transcription_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    lager:info("Starting Transcription Service"),

    {ok, Port} = application:get_env(transcription_service, port),
    {ok, NumAcceptors} = application:get_env(transcription_service, num_acceptors),

    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/transcribe", transcription_handler, #{action => transcribe}},
            {"/health", transcription_handler, #{action => health}}
        ]}
    ]),

    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(
        transcription_http_listener,
        [{port, Port}, {num_acceptors, NumAcceptors}],
        #{env => #{dispatch => Dispatch}}
    ),

    lager:info("Transcription Service listening on port ~p with ~p acceptors", [Port, NumAcceptors]),

    transcription_service_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(transcription_http_listener),
    ok.
