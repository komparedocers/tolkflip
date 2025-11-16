%%%-------------------------------------------------------------------
%%% @doc Presence Service HTTP Handler
%%% Handles user presence operations
%%% @end
%%%-------------------------------------------------------------------
-module(presence_handler).

-export([init/2]).

init(Req, #{action := get_presence} = State) ->
    UserId = cowboy_req:binding(user_id, Req),

    case tolkflip_redis:hget(<<"presence">>, UserId) of
        {ok, Status} when Status =/= undefined ->
            Reply = jsx:encode(#{
                <<"userId">> => UserId,
                <<"status">> => Status
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {ok, undefined} ->
            Reply = jsx:encode(#{
                <<"userId">> => UserId,
                <<"status">> => <<"offline">>
            }),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {error, Reason} ->
            lager:error("Failed to get presence for ~s: ~p", [UserId, Reason]),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Failed to get presence">>}), Req),
            {ok, Req2, State}
    end;

init(Req0, #{action := update_status} = State) ->
    UserId = cowboy_req:binding(user_id, Req0),
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    Data = jsx:decode(Body, [return_maps]),
    Status = maps:get(<<"status">>, Data),

    case tolkflip_redis:hset(<<"presence">>, UserId, Status) of
        ok ->
            %% Set TTL on presence key (15 minutes)
            tolkflip_redis:expire(<<"presence">>, 900),

            Reply = jsx:encode(#{<<"success">> => true}),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {error, Reason} ->
            lager:error("Failed to update presence for ~s: ~p", [UserId, Reason]),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Failed to update presence">>}), Req),
            {ok, Req2, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"presence-service">>}),
        Req),
    {ok, Req2, State}.
