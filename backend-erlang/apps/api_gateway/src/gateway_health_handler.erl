%%%-------------------------------------------------------------------
%%% @doc API Gateway Health Check Handler
%%% @end
%%%-------------------------------------------------------------------
-module(gateway_health_handler).

-export([init/2]).

init(Req, State) ->
    %% Check health of all backend services
    Services = [auth, user, chat, translation, transcription, media,
                presence, message_store, notification, webrtc, group],

    HealthChecks = lists:map(fun(Service) ->
        Port = maps:get(Service, gateway_handler:service_ports()),
        Url = io_lib:format("http://localhost:~p/health", [Port]),

        Status = case httpc:request(get, {Url, []}, [{timeout, 5000}], []) of
            {ok, {{_, 200, _}, _, _}} -> <<"healthy">>;
            _ -> <<"unhealthy">>
        end,

        {atom_to_binary(Service, utf8), Status}
    end, Services),

    AllHealthy = lists:all(fun({_, Status}) -> Status =:= <<"healthy">> end, HealthChecks),

    Response = jsx:encode(#{
        <<"status">> => if AllHealthy -> <<"ok">>; true -> <<"degraded">> end,
        <<"service">> => <<"api-gateway">>,
        <<"backends">> => maps:from_list(HealthChecks)
    }),

    Req2 = cowboy_req:reply(if AllHealthy -> 200; true -> 503 end,
        #{<<"content-type">> => <<"application/json">>},
        Response, Req),

    {ok, Req2, State}.
