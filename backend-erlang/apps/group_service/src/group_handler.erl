%%%-------------------------------------------------------------------
%%% @doc Group Service HTTP Handler
%%% Handles group chat operations
%%% @end
%%%-------------------------------------------------------------------
-module(group_handler).

-export([init/2]).

init(Req0, #{action := create_group} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            {ok, Body, Req} = cowboy_req:read_body(Req0),
            Data = jsx:decode(Body, [return_maps]),

            Name = maps:get(<<"name">>, Data),
            Description = maps:get(<<"description">>, Data, <<>>),
            CreatorId = maps:get(<<"creatorId">>, Data),
            MemberIds = maps:get(<<"memberIds">>, Data, []),

            CreateResult = tolkflip_cassandra:create_group(
                binary_to_list(Name),
                binary_to_list(Description),
                binary_to_list(CreatorId),
                [binary_to_list(M) || M <- MemberIds]
            ),

            case CreateResult of
                {ok, GroupId} ->
                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"groupId">> => list_to_binary(GroupId)
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    lager:error("Failed to create group: ~p", [Reason]),
                    Req2 = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{<<"error">> => <<"Failed to create group">>}), Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := get_group} = State) ->
    GroupId = cowboy_req:binding(group_id, Req),

    case tolkflip_cassandra:get_group(binary_to_list(GroupId)) of
        {ok, Group} ->
            Reply = jsx:encode(#{<<"group">> => Group}),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {error, not_found} ->
            Req2 = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Group not found">>}), Req),
            {ok, Req2, State};
        {error, Reason} ->
            lager:error("Failed to get group ~s: ~p", [GroupId, Reason]),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Internal server error">>}), Req),
            {ok, Req2, State}
    end;

init(Req0, #{action := add_member} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            GroupId = cowboy_req:binding(group_id, Req0),
            {ok, Body, Req} = cowboy_req:read_body(Req0),
            Data = jsx:decode(Body, [return_maps]),

            UserId = maps:get(<<"userId">>, Data),
            Role = maps:get(<<"role">>, Data, <<"member">>),

            AddResult = tolkflip_cassandra:add_group_member(
                binary_to_list(GroupId),
                binary_to_list(UserId),
                binary_to_list(Role)
            ),

            case AddResult of
                ok ->
                    Reply = jsx:encode(#{<<"success">> => true}),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State};
                {error, Reason} ->
                    lager:error("Failed to add member to group ~s: ~p", [GroupId, Reason]),
                    Req2 = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{<<"error">> => <<"Failed to add member">>}), Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req0, #{action := remove_member} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"DELETE">> ->
            GroupId = cowboy_req:binding(group_id, Req0),
            UserId = cowboy_req:binding(user_id, Req0),

            RemoveResult = tolkflip_cassandra:remove_group_member(
                binary_to_list(GroupId),
                binary_to_list(UserId)
            ),

            case RemoveResult of
                ok ->
                    Reply = jsx:encode(#{<<"success">> => true}),
                    Req = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req0),
                    {ok, Req, State};
                {error, Reason} ->
                    lager:error("Failed to remove member from group ~s: ~p", [GroupId, Reason]),
                    Req = cowboy_req:reply(500,
                        #{<<"content-type">> => <<"application/json">>},
                        jsx:encode(#{<<"error">> => <<"Failed to remove member">>}), Req0),
                    {ok, Req, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"group-service">>}),
        Req),
    {ok, Req2, State}.
