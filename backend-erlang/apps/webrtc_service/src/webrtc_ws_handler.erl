%%%-------------------------------------------------------------------
%%% @doc WebRTC WebSocket Handler
%%% Handles WebSocket connections for WebRTC signaling
%%% @end
%%%-------------------------------------------------------------------
-module(webrtc_ws_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
    user_id :: binary() | undefined,
    authenticated = false :: boolean()
}).

init(Req, _Opts) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    lager:info("WebRTC WebSocket connection established"),
    {ok, State}.

%% Handle authentication
websocket_handle({text, Msg}, #state{authenticated = false} = State) ->
    Data = jsx:decode(Msg, [return_maps]),

    case maps:get(<<"action">>, Data, undefined) of
        <<"authenticate">> ->
            Token = maps:get(<<"token">>, Data),
            case tolkflip_jwt:verify_token(binary_to_list(Token)) of
                {ok, Claims} ->
                    UserId = list_to_binary(maps:get(<<"userId">>, Claims)),
                    lager:info("WebRTC WebSocket authenticated: ~s", [UserId]),

                    %% Join user's WebRTC room
                    gproc:reg({p, l, {webrtc_user, UserId}}),

                    Response = jsx:encode(#{
                        <<"type">> => <<"authenticated">>,
                        <<"userId">> => UserId
                    }),
                    {reply, {text, Response}, State#state{user_id = UserId, authenticated = true}};
                {error, _Reason} ->
                    Response = jsx:encode(#{
                        <<"type">> => <<"error">>,
                        <<"message">> => <<"Invalid token">>
                    }),
                    {reply, {text, Response}, State}
            end;
        _ ->
            Response = jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"message">> => <<"Authentication required">>
            }),
            {reply, {text, Response}, State}
    end;

%% Handle signaling messages
websocket_handle({text, Msg}, #state{authenticated = true} = State) ->
    Data = jsx:decode(Msg, [return_maps]),
    handle_message(Data, State);

websocket_handle(_Frame, State) ->
    {ok, State}.

%% Handle call initiation
handle_message(#{<<"action">> := <<"call:initiate">>} = Data,
               #state{user_id = CallerId} = State) ->
    CalleeId = maps:get(<<"calleeId">>, Data),
    CallType = maps:get(<<"callType">>, Data, <<"audio">>),
    ThreadId = maps:get(<<"threadId">>, Data, <<>>),

    %% Generate call ID
    CallId = generate_call_id(CallerId),

    %% Store call info in Redis
    CallData = #{
        <<"callId">> => CallId,
        <<"caller">> => CallerId,
        <<"callee">> => CalleeId,
        <<"callType">> => CallType,
        <<"threadId">> => ThreadId,
        <<"status">> => <<"ringing">>,
        <<"timestamp">> => erlang:system_time(millisecond)
    },

    tolkflip_redis:setex(
        <<"webrtc:call:", CallId/binary>>,
        3600,
        jsx:encode(CallData)
    ),

    %% Send to callee
    send_to_user(CalleeId, #{
        <<"type">> => <<"call:incoming">>,
        <<"callId">> => CallId,
        <<"callerId">> => CallerId,
        <<"callType">> => CallType,
        <<"threadId">> => ThreadId
    }),

    %% Echo back to caller
    Response = jsx:encode(#{
        <<"type">> => <<"call:initiated">>,
        <<"callId">> => CallId
    }),
    {reply, {text, Response}, State};

%% Handle call answer
handle_message(#{<<"action">> := <<"call:answer">>} = Data, State) ->
    CallId = maps:get(<<"callId">>, Data),

    %% Update call status in Redis
    case tolkflip_redis:get(<<"webrtc:call:", CallId/binary>>) of
        {ok, CallDataJson} when CallDataJson =/= undefined ->
            CallData = jsx:decode(CallDataJson, [return_maps]),
            CallerId = maps:get(<<"caller">>, CallData),

            UpdatedCallData = maps:put(<<"status">>, <<"active">>, CallData),
            tolkflip_redis:setex(
                <<"webrtc:call:", CallId/binary>>,
                3600,
                jsx:encode(UpdatedCallData)
            ),

            %% Notify caller
            send_to_user(CallerId, #{
                <<"type">> => <<"call:answered">>,
                <<"callId">> => CallId
            }),

            {ok, State};
        _ ->
            Response = jsx:encode(#{
                <<"type">> => <<"error">>,
                <<"message">> => <<"Call not found">>
            }),
            {reply, {text, Response}, State}
    end;

%% Handle call rejection
handle_message(#{<<"action">> := <<"call:reject">>} = Data, State) ->
    CallId = maps:get(<<"callId">>, Data),
    Reason = maps:get(<<"reason">>, Data, <<"rejected">>),

    case tolkflip_redis:get(<<"webrtc:call:", CallId/binary>>) of
        {ok, CallDataJson} when CallDataJson =/= undefined ->
            CallData = jsx:decode(CallDataJson, [return_maps]),
            CallerId = maps:get(<<"caller">>, CallData),

            %% Delete call data
            tolkflip_redis:del(<<"webrtc:call:", CallId/binary>>),

            %% Notify caller
            send_to_user(CallerId, #{
                <<"type">> => <<"call:rejected">>,
                <<"callId">> => CallId,
                <<"reason">> => Reason
            }),

            {ok, State};
        _ ->
            {ok, State}
    end;

%% Handle call end
handle_message(#{<<"action">> := <<"call:end">>} = Data,
               #state{user_id = UserId} = State) ->
    CallId = maps:get(<<"callId">>, Data),

    case tolkflip_redis:get(<<"webrtc:call:", CallId/binary>>) of
        {ok, CallDataJson} when CallDataJson =/= undefined ->
            CallData = jsx:decode(CallDataJson, [return_maps]),
            CallerId = maps:get(<<"caller">>, CallData),
            CalleeId = maps:get(<<"callee">>, CallData),

            %% Delete call data
            tolkflip_redis:del(<<"webrtc:call:", CallId/binary>>),

            %% Notify other participant
            OtherUserId = if
                UserId =:= CallerId -> CalleeId;
                true -> CallerId
            end,

            send_to_user(OtherUserId, #{
                <<"type">> => <<"call:ended">>,
                <<"callId">> => CallId
            }),

            {ok, State};
        _ ->
            {ok, State}
    end;

%% Handle WebRTC offer
handle_message(#{<<"action">> := <<"webrtc:offer">>} = Data, State) ->
    CallId = maps:get(<<"callId">>, Data),
    Offer = maps:get(<<"offer">>, Data),

    case tolkflip_redis:get(<<"webrtc:call:", CallId/binary>>) of
        {ok, CallDataJson} when CallDataJson =/= undefined ->
            CallData = jsx:decode(CallDataJson, [return_maps]),
            CalleeId = maps:get(<<"callee">>, CallData),

            send_to_user(CalleeId, #{
                <<"type">> => <<"webrtc:offer">>,
                <<"callId">> => CallId,
                <<"offer">> => Offer
            }),

            {ok, State};
        _ ->
            {ok, State}
    end;

%% Handle WebRTC answer
handle_message(#{<<"action">> := <<"webrtc:answer">>} = Data, State) ->
    CallId = maps:get(<<"callId">>, Data),
    Answer = maps:get(<<"answer">>, Data),

    case tolkflip_redis:get(<<"webrtc:call:", CallId/binary>>) of
        {ok, CallDataJson} when CallDataJson =/= undefined ->
            CallData = jsx:decode(CallDataJson, [return_maps]),
            CallerId = maps:get(<<"caller">>, CallData),

            send_to_user(CallerId, #{
                <<"type">> => <<"webrtc:answer">>,
                <<"callId">> => CallId,
                <<"answer">> => Answer
            }),

            {ok, State};
        _ ->
            {ok, State}
    end;

%% Handle ICE candidates
handle_message(#{<<"action">> := <<"webrtc:ice-candidate">>} = Data,
               #state{user_id = UserId} = State) ->
    CallId = maps:get(<<"callId">>, Data),
    Candidate = maps:get(<<"candidate">>, Data),

    case tolkflip_redis:get(<<"webrtc:call:", CallId/binary>>) of
        {ok, CallDataJson} when CallDataJson =/= undefined ->
            CallData = jsx:decode(CallDataJson, [return_maps]),
            CallerId = maps:get(<<"caller">>, CallData),
            CalleeId = maps:get(<<"callee">>, CallData),

            %% Send to other participant
            TargetUserId = if
                UserId =:= CallerId -> CalleeId;
                true -> CallerId
            end,

            send_to_user(TargetUserId, #{
                <<"type">> => <<"webrtc:ice-candidate">>,
                <<"callId">> => CallId,
                <<"candidate">> => Candidate
            }),

            {ok, State};
        _ ->
            {ok, State}
    end;

handle_message(_Data, State) ->
    {ok, State}.

%% Handle messages from other processes
websocket_info({send_message, Message}, State) ->
    Response = jsx:encode(Message),
    {reply, {text, Response}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{user_id = UserId, authenticated = true}) ->
    lager:info("WebRTC WebSocket disconnected: ~s", [UserId]),
    gproc:unregister({p, l, {webrtc_user, UserId}}),
    ok;

terminate(_Reason, _Req, _State) ->
    ok.

%% Helper functions
generate_call_id(UserId) ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    <<Timestamp/binary, ":", UserId/binary>>.

send_to_user(UserId, Message) ->
    case gproc:lookup_pids({p, l, {webrtc_user, UserId}}) of
        [Pid | _] ->
            Pid ! {send_message, Message},
            ok;
        [] ->
            lager:warning("User ~s not connected to WebRTC service", [UserId]),
            {error, not_connected}
    end.
