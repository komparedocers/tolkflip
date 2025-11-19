%%%-------------------------------------------------------------------
%%% @doc Auth Service HTTP Handler
%%% Handles authentication requests for phone verification and JWT
%%% @end
%%%-------------------------------------------------------------------
-module(auth_handler).

-export([init/2]).

init(Req, #{action := request_code} = State) ->
    Method = cowboy_req:method(Req),

    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Data = jsx:decode(Body, [return_maps]),

            PhoneNumber = maps:get(<<"phone_number">>, Data),

            %% Generate 6-digit code
            Code = generate_verification_code(),

            %% Store in Redis with 10 minute expiry
            Key = <<"verification:", PhoneNumber/binary>>,
            tolkflip_redis:setex(binary_to_list(Key), 600, Code),

            %% TODO: Send SMS via Twilio/SNS
            lager:info("Verification code for ~s: ~s", [PhoneNumber, Code]),

            Reply = jsx:encode(#{
                <<"success">> => true,
                <<"message">> => <<"Verification code sent">>,
                <<"code">> => Code  %% Remove in production
            }),
            Req3 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                Reply, Req2),
            {ok, Req3, State};
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end;

init(Req, #{action := verify_code} = State) ->
    Method = cowboy_req:method(Req),

    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Data = jsx:decode(Body, [return_maps]),

            PhoneNumber = maps:get(<<"phone_number">>, Data),
            Code = maps:get(<<"code">>, Data),

            %% Verify code from Redis
            Key = <<"verification:", PhoneNumber/binary>>,
            case tolkflip_redis:get(binary_to_list(Key)) of
                {ok, StoredCode} when StoredCode =:= Code ->
                    %% Code is valid
                    tolkflip_redis:del(binary_to_list(Key)),

                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"message">> => <<"Code verified">>
                    }),
                    Req3 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State};
                _ ->
                    Reply = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Invalid or expired code">>
                    }),
                    Req3 = cowboy_req:reply(400,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end;

init(Req, #{action := register} = State) ->
    Method = cowboy_req:method(Req),

    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Data = jsx:decode(Body, [return_maps]),

            PhoneNumber = maps:get(<<"phone_number">>, Data),
            DisplayName = maps:get(<<"display_name">>, Data),
            PrimaryLanguage = maps:get(<<"primary_language">>, Data, <<"en">>),
            AdditionalLanguages = maps:get(<<"additional_languages">>, Data, []),

            %% Check if user exists
            case tolkflip_cassandra:get_user_by_phone(binary_to_list(PhoneNumber)) of
                {ok, _ExistingUser} ->
                    Reply = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"User already exists">>
                    }),
                    Req3 = cowboy_req:reply(409,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State};
                {error, not_found} ->
                    %% Create new user
                    UserId = uuid:get_v4(),
                    UserIdStr = uuid:uuid_to_string(UserId, binary_standard),

                    tolkflip_cassandra:create_user(
                        binary_to_list(UserIdStr),
                        binary_to_list(PhoneNumber),
                        binary_to_list(DisplayName),
                        binary_to_list(PrimaryLanguage),
                        [binary_to_list(L) || L <- AdditionalLanguages]
                    ),

                    %% Generate tokens
                    AccessToken = tolkflip_jwt:generate_access_token(
                        binary_to_list(UserIdStr),
                        binary_to_list(PhoneNumber)
                    ),
                    RefreshToken = tolkflip_jwt:generate_refresh_token(
                        binary_to_list(UserIdStr)
                    ),

                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"user_id">> => UserIdStr,
                        <<"access_token">> => list_to_binary(AccessToken),
                        <<"refresh_token">> => list_to_binary(RefreshToken)
                    }),
                    Req3 = cowboy_req:reply(201,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end;

init(Req, #{action := login} = State) ->
    Method = cowboy_req:method(Req),

    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Data = jsx:decode(Body, [return_maps]),

            PhoneNumber = maps:get(<<"phone_number">>, Data),

            case tolkflip_cassandra:get_user_by_phone(binary_to_list(PhoneNumber)) of
                {ok, User} ->
                    UserId = maps:get(<<"user_id">>, User),

                    %% Generate tokens
                    AccessToken = tolkflip_jwt:generate_access_token(
                        binary_to_list(UserId),
                        binary_to_list(PhoneNumber)
                    ),
                    RefreshToken = tolkflip_jwt:generate_refresh_token(
                        binary_to_list(UserId)
                    ),

                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"user_id">> => UserId,
                        <<"access_token">> => list_to_binary(AccessToken),
                        <<"refresh_token">> => list_to_binary(RefreshToken)
                    }),
                    Req3 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State};
                {error, not_found} ->
                    Reply = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"User not found">>
                    }),
                    Req3 = cowboy_req:reply(404,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end;

init(Req, #{action := refresh} = State) ->
    Method = cowboy_req:method(Req),

    case Method of
        <<"POST">> ->
            {ok, Body, Req2} = cowboy_req:read_body(Req),
            Data = jsx:decode(Body, [return_maps]),

            RefreshToken = maps:get(<<"refresh_token">>, Data),

            case tolkflip_jwt:verify_refresh_token(binary_to_list(RefreshToken)) of
                {ok, Claims} ->
                    UserId = maps:get(<<"userId">>, Claims),
                    PhoneNumber = maps:get(<<"phoneNumber">>, Claims, ""),

                    %% Generate new access token
                    NewAccessToken = tolkflip_jwt:generate_access_token(
                        UserId,
                        PhoneNumber
                    ),

                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"access_token">> => list_to_binary(NewAccessToken)
                    }),
                    Req3 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State};
                {error, _Reason} ->
                    Reply = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"Invalid refresh token">>
                    }),
                    Req3 = cowboy_req:reply(401,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req2),
                    {ok, Req3, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(405, Req),
            {ok, Req2, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"auth-service">>}),
        Req),
    {ok, Req2, State}.

%% Helper Functions
generate_verification_code() ->
    Code = rand:uniform(900000) + 100000,
    integer_to_binary(Code).
