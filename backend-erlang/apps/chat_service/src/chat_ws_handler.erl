%%%-------------------------------------------------------------------
%%% @doc WebSocket Handler for Real-time Chat
%%% Handles millions of concurrent WebSocket connections
%%% @end
%%%-------------------------------------------------------------------
-module(chat_ws_handler).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {
    user_id :: binary(),
    authenticated = false :: boolean()
}).

%%====================================================================
%% Cowboy WebSocket Callbacks
%%====================================================================

init(Req, _State) ->
    %% Upgrade to WebSocket
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    lager:info("WebSocket connection initialized"),
    {ok, State}.

%%====================================================================
%% Handle incoming WebSocket messages
%%====================================================================

websocket_handle({text, Msg}, State) ->
    try
        Data = jsx:decode(Msg, [return_maps]),
        handle_message(Data, State)
    catch
        _:Error ->
            lager:error("Error parsing message: ~p", [Error]),
            ErrorReply = jsx:encode(#{
                <<"error">> => <<"Invalid JSON">>
            }),
            {reply, {text, ErrorReply}, State}
    end;

websocket_handle(_Data, State) ->
    {ok, State}.

%%====================================================================
%% Handle Erlang messages sent to this process
%%====================================================================

websocket_info({send_message, Message}, State) ->
    %% Send message to client
    Json = jsx:encode(Message),
    {reply, {text, Json}, State};

websocket_info({typing_indicator, Data}, State) ->
    Json = jsx:encode(Data),
    {reply, {text, Json}, State};

websocket_info({read_receipt, Data}, State) ->
    Json = jsx:encode(Data),
    {reply, {text, Json}, State};

websocket_info(Info, State) ->
    lager:warning("Unexpected info: ~p", [Info]),
    {ok, State}.

%%====================================================================
%% Message Handlers
%%====================================================================

handle_message(#{<<"action">> := <<"authenticate">>, <<"token">> := Token}, State) ->
    case tolkflip_jwt:validate_token(binary_to_list(Token)) of
        {ok, Claims} ->
            UserId = maps:get(<<"userId">>, Claims),

            %% Register this WebSocket process for the user
            register_connection(UserId),

            lager:info("User authenticated: ~p", [UserId]),

            Reply = jsx:encode(#{
                <<"type">> => <<"authenticated">>,
                <<"success">> => true
            }),

            {reply, {text, Reply}, State#state{
                user_id = UserId,
                authenticated = true
            }};

        {error, Reason} ->
            lager:warning("Authentication failed: ~p", [Reason]),

            Reply = jsx:encode(#{
                <<"type">> => <<"auth_error">>,
                <<"error">> => atom_to_binary(Reason, utf8)
            }),

            {reply, {text, Reply}, State}
    end;

handle_message(#{<<"action">> := <<"send_message">>} = Data, #state{authenticated = true, user_id = UserId} = State) ->
    ThreadId = maps:get(<<"threadId">>, Data),
    ReceiverId = maps:get(<<"receiverId">>, Data),
    Content = maps:get(<<"content">>, Data),
    MessageType = maps:get(<<"messageType">>, Data, <<"text">>),
    OriginalLanguage = maps:get(<<"originalLanguage">>, Data, <<"en">>),

    %% Save message to Cassandra
    {ok, MessageId} = tolkflip_cassandra:save_message(
        binary_to_list(ThreadId),
        binary_to_list(UserId),
        binary_to_list(ReceiverId),
        binary_to_list(MessageType),
        binary_to_list(Content),
        binary_to_list(OriginalLanguage),
        "sent",
        false
    ),

    %% Prepare message for recipients
    MessageData = #{
        <<"type">> => <<"new_message">>,
        <<"messageId">> => list_to_binary(MessageId),
        <<"threadId">> => ThreadId,
        <<"senderId">> => UserId,
        <<"content">> => Content,
        <<"messageType">> => MessageType,
        <<"originalLanguage">> => OriginalLanguage,
        <<"timestamp">> => os:system_time(millisecond)
    },

    %% Send to receiver if online
    send_to_user(ReceiverId, {send_message, MessageData}),

    %% Echo back to sender
    Reply = jsx:encode(#{
        <<"type">> => <<"message_sent">>,
        <<"messageId">> => list_to_binary(MessageId),
        <<"status">> => <<"sent">>
    }),

    {reply, {text, Reply}, State};

handle_message(#{<<"action">> := <<"typing">>} = Data, #state{authenticated = true, user_id = UserId} = State) ->
    ReceiverId = maps:get(<<"receiverId">>, Data),
    ThreadId = maps:get(<<"threadId">>, Data),

    %% Send typing indicator to receiver
    TypingData = #{
        <<"type">> => <<"typing">>,
        <<"senderId">> => UserId,
        <<"threadId">> => ThreadId
    },

    send_to_user(ReceiverId, {typing_indicator, TypingData}),

    {ok, State};

handle_message(#{<<"action">> := <<"read_receipt">>} = Data, #state{authenticated = true, user_id = UserId} = State) ->
    MessageId = maps:get(<<"messageId">>, Data),
    ThreadId = maps:get(<<"threadId">>, Data),
    SenderId = maps:get(<<"senderId">>, Data),

    %% Update message status
    tolkflip_cassandra:update_message_status(
        binary_to_list(ThreadId),
        binary_to_list(MessageId),
        "read"
    ),

    %% Notify sender
    ReadData = #{
        <<"type">> => <<"read_receipt">>,
        <<"messageId">> => MessageId,
        <<"threadId">> => ThreadId,
        <<"readBy">> => UserId
    },

    send_to_user(SenderId, {read_receipt, ReadData}),

    {ok, State};

handle_message(#{<<"action">> := Action}, #state{authenticated = false} = State) ->
    lager:warning("Unauthenticated action attempted: ~p", [Action]),

    Reply = jsx:encode(#{
        <<"error">> => <<"Not authenticated">>
    }),

    {reply, {text, Reply}, State};

handle_message(Data, State) ->
    lager:warning("Unknown message: ~p", [Data]),
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

register_connection(UserId) ->
    %% Register this process as the handler for this user
    %% Using process registry (gproc or pg2 in production)
    ProcessName = {chat_connection, UserId},
    try
        register(binary_to_atom(ProcessName, utf8), self())
    catch
        error:badarg ->
            %% Already registered, unregister old and register new
            case whereis(binary_to_atom(ProcessName, utf8)) of
                undefined -> ok;
                OldPid ->
                    unregister(binary_to_atom(ProcessName, utf8)),
                    exit(OldPid, replaced)
            end,
            register(binary_to_atom(ProcessName, utf8), self())
    end,

    %% Update presence in Redis
    tolkflip_redis:hset(<<"presence">>, UserId, <<"online">>).

send_to_user(UserId, Message) ->
    ProcessName = binary_to_atom({chat_connection, UserId}, utf8),
    case whereis(ProcessName) of
        undefined ->
            %% User not connected, message will be delivered when they connect
            lager:info("User ~p not connected, message queued", [UserId]),
            ok;
        Pid ->
            Pid ! Message,
            ok
    end.
