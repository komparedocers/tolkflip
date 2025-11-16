%%%-------------------------------------------------------------------
%%% @doc User Service HTTP Handler
%%% Handles user profile operations
%%% @end
%%%-------------------------------------------------------------------
-module(user_handler).

-export([init/2]).

init(Req, #{action := get_user} = State) ->
    UserId = cowboy_req:binding(user_id, Req),

    case tolkflip_cassandra:get_user_by_id(binary_to_list(UserId)) of
        {ok, User} ->
            Reply = jsx:encode(#{<<"user">> => User}),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req),
            {ok, Req2, State};
        {error, not_found} ->
            Req2 = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"User not found">>}), Req),
            {ok, Req2, State};
        {error, Reason} ->
            lager:error("Failed to get user ~s: ~p", [UserId, Reason]),
            Req2 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Internal server error">>}), Req),
            {ok, Req2, State}
    end;

init(Req, #{action := update_profile} = State) ->
    UserId = cowboy_req:binding(user_id, Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),

    DisplayName = maps:get(<<"display_name">>, Data, undefined),
    ProfilePicture = maps:get(<<"profile_picture">>, Data, undefined),
    Bio = maps:get(<<"bio">>, Data, undefined),

    UpdateResult = tolkflip_cassandra:update_user_profile(
        binary_to_list(UserId),
        case DisplayName of undefined -> undefined; _ -> binary_to_list(DisplayName) end,
        case ProfilePicture of undefined -> undefined; _ -> binary_to_list(ProfilePicture) end,
        case Bio of undefined -> undefined; _ -> binary_to_list(Bio) end
    ),

    case UpdateResult of
        ok ->
            Reply = jsx:encode(#{<<"success">> => true}),
            Req3 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req2),
            {ok, Req3, State};
        {error, Reason} ->
            lager:error("Failed to update profile for ~s: ~p", [UserId, Reason]),
            Req3 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Failed to update profile">>}), Req2),
            {ok, Req3, State}
    end;

init(Req, #{action := update_languages} = State) ->
    UserId = cowboy_req:binding(user_id, Req),
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),

    NativeLanguage = maps:get(<<"native_language">>, Data, undefined),
    LearningLanguages = maps:get(<<"learning_languages">>, Data, []),

    UpdateResult = tolkflip_cassandra:update_user_languages(
        binary_to_list(UserId),
        case NativeLanguage of undefined -> undefined; _ -> binary_to_list(NativeLanguage) end,
        [binary_to_list(L) || L <- LearningLanguages]
    ),

    case UpdateResult of
        ok ->
            Reply = jsx:encode(#{<<"success">> => true}),
            Req3 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req2),
            {ok, Req3, State};
        {error, Reason} ->
            lager:error("Failed to update languages for ~s: ~p", [UserId, Reason]),
            Req3 = cowboy_req:reply(500,
                #{<<"content-type">> => <<"application/json">>},
                jsx:encode(#{<<"error">> => <<"Failed to update languages">>}), Req2),
            {ok, Req3, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"user-service">>}),
        Req),
    {ok, Req2, State}.
