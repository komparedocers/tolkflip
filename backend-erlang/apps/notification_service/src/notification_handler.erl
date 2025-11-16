%%%-------------------------------------------------------------------
%%% @doc Notification Service HTTP Handler
%%% Handles push notifications
%%% @end
%%%-------------------------------------------------------------------
-module(notification_handler).

-export([init/2]).

init(Req0, #{action := send} = State) ->
    Method = cowboy_req:method(Req0),

    case Method of
        <<"POST">> ->
            {ok, Body, Req} = cowboy_req:read_body(Req0),
            Data = jsx:decode(Body, [return_maps]),

            UserId = maps:get(<<"userId">>, Data),
            Title = maps:get(<<"title">>, Data),
            Message = maps:get(<<"message">>, Data),
            NotificationType = maps:get(<<"type">>, Data, <<"message">>),
            AdditionalData = maps:get(<<"data">>, Data, #{}),

            %% Get user's FCM token from Redis
            case tolkflip_redis:hget(<<"fcm_tokens">>, UserId) of
                {ok, FcmToken} when FcmToken =/= undefined ->
                    %% Send FCM notification
                    NotificationId = send_fcm_notification(
                        FcmToken,
                        Title,
                        Message,
                        NotificationType,
                        AdditionalData
                    ),

                    %% Store notification in database
                    {ok, _} = tolkflip_cassandra:save_notification(
                        binary_to_list(UserId),
                        binary_to_list(NotificationId),
                        binary_to_list(Title),
                        binary_to_list(Message),
                        binary_to_list(NotificationType)
                    ),

                    Reply = jsx:encode(#{
                        <<"success">> => true,
                        <<"notificationId">> => NotificationId
                    }),
                    Req2 = cowboy_req:reply(200,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State};
                _ ->
                    lager:warning("No FCM token for user ~s", [UserId]),
                    Reply = jsx:encode(#{
                        <<"success">> => false,
                        <<"error">> => <<"No FCM token registered">>
                    }),
                    Req2 = cowboy_req:reply(400,
                        #{<<"content-type">> => <<"application/json">>},
                        Reply, Req),
                    {ok, Req2, State}
            end;
        _ ->
            Req = cowboy_req:reply(405, Req0),
            {ok, Req, State}
    end;

init(Req, #{action := health} = State) ->
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{<<"status">> => <<"ok">>, <<"service">> => <<"notification-service">>}),
        Req),
    {ok, Req2, State}.

%% Helper function - mock FCM implementation
send_fcm_notification(FcmToken, Title, Message, Type, Data) ->
    %% TODO: Implement actual FCM/APNS integration
    lager:info("Sending notification to ~s: ~s - ~s", [FcmToken, Title, Message]),

    %% Generate notification ID
    NotificationId = generate_notification_id(),

    %% In production, send via FCM API:
    %% Payload = #{
    %%     <<"to">> => FcmToken,
    %%     <<"notification">> => #{
    %%         <<"title">> => Title,
    %%         <<"body">> => Message
    %%     },
    %%     <<"data">> => Data#{<<"type">> => Type}
    %% },
    %% send_fcm_request(Payload),

    NotificationId.

generate_notification_id() ->
    UUID = uuid:get_v4(),
    uuid:uuid_to_string(UUID, binary_standard).
