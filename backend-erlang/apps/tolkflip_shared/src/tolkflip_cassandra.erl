%%%-------------------------------------------------------------------
%%% @doc High-performance Cassandra client
%%% Async operations for massive concurrency
%%% @end
%%%-------------------------------------------------------------------
-module(tolkflip_cassandra).

%% User operations
-export([create_user/5, get_user_by_phone/1, get_user_by_id/1]).

%% Message operations
-export([save_message/8, get_messages/2, update_message_status/3]).

%% Thread operations
-export([create_or_update_thread/7, get_threads_for_user/2]).

%% Group operations
-export([create_group/5, add_group_member/4, get_group_members/2]).

%% Translation cache
-export([cache_translation/5, get_cached_translation/3]).

%%====================================================================
%% User Operations
%%====================================================================

create_user(UserId, PhoneNumber, DisplayName, PrimaryLanguage, AdditionalLanguages) ->
    Query = <<"INSERT INTO users (user_id, phone_number, display_name, "
              "primary_language, additional_languages, created_at, last_active) "
              "VALUES (?, ?, ?, ?, ?, toTimestamp(now()), toTimestamp(now()))">>,

    Params = [
        {uuid, UserId},
        {text, PhoneNumber},
        {text, DisplayName},
        {text, PrimaryLanguage},
        {list, text, AdditionalLanguages}
    ],

    execute_query(Query, Params).

get_user_by_phone(PhoneNumber) ->
    Query = <<"SELECT * FROM users WHERE phone_number = ? LIMIT 1">>,
    Params = [{text, PhoneNumber}],

    case execute_query(Query, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

get_user_by_id(UserId) ->
    Query = <<"SELECT * FROM users WHERE user_id = ?">>,
    Params = [{uuid, UserId}],

    case execute_query(Query, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

%%====================================================================
%% Message Operations
%%====================================================================

save_message(ThreadId, SenderId, ReceiverId, MessageType, Content,
             OriginalLanguage, Status, IsGroup) ->
    MessageId = uuid:get_v1(uuid:new(self())),

    Query = <<"INSERT INTO messages (thread_id, message_id, sender_id, receiver_id, "
              "message_type, content, original_language, timestamp, status, is_group) "
              "VALUES (?, now(), ?, ?, ?, ?, ?, toTimestamp(now()), ?, ?)">>,

    Params = [
        {uuid, ThreadId},
        {uuid, SenderId},
        {uuid, ReceiverId},
        {text, MessageType},
        {text, Content},
        {text, OriginalLanguage},
        {text, Status},
        {boolean, IsGroup}
    ],

    case execute_query(Query, Params) of
        ok -> {ok, MessageId};
        Error -> Error
    end.

get_messages(ThreadId, Limit) ->
    Query = <<"SELECT * FROM messages WHERE thread_id = ? LIMIT ?">>,
    Params = [{uuid, ThreadId}, {int, Limit}],
    execute_query(Query, Params).

update_message_status(ThreadId, MessageId, Status) ->
    Query = <<"UPDATE messages SET status = ? WHERE thread_id = ? AND message_id = ?">>,
    Params = [{text, Status}, {uuid, ThreadId}, {uuid, MessageId}],
    execute_query(Query, Params).

%%====================================================================
%% Thread Operations
%%====================================================================

create_or_update_thread(ThreadId, ParticipantId, OtherParticipantId,
                        ThreadType, LastMessageTime, LastMessagePreview, UnreadCount) ->
    Query = <<"INSERT INTO chat_threads (thread_id, participant_id, other_participant_id, "
              "thread_type, created_at, last_message_time, last_message_preview, "
              "unread_count, is_archived) "
              "VALUES (?, ?, ?, ?, toTimestamp(now()), ?, ?, ?, false)">>,

    Params = [
        {uuid, ThreadId},
        {uuid, ParticipantId},
        {uuid, OtherParticipantId},
        {text, ThreadType},
        {bigint, LastMessageTime},
        {text, LastMessagePreview},
        {int, UnreadCount}
    ],

    execute_query(Query, Params).

get_threads_for_user(UserId, Limit) ->
    Query = <<"SELECT * FROM chat_threads WHERE participant_id = ? LIMIT ?">>,
    Params = [{uuid, UserId}, {int, Limit}],
    execute_query(Query, Params).

%%====================================================================
%% Group Operations
%%====================================================================

create_group(GroupId, GroupName, CreatedBy, Description, MemberCount) ->
    Query = <<"INSERT INTO group_chats (group_id, group_name, created_by, "
              "created_at, description, member_count) "
              "VALUES (?, ?, ?, toTimestamp(now()), ?, ?)">>,

    Params = [
        {uuid, GroupId},
        {text, GroupName},
        {uuid, CreatedBy},
        {text, Description},
        {int, MemberCount}
    ],

    execute_query(Query, Params).

add_group_member(GroupId, UserId, Role, PreferredLanguage) ->
    Query = <<"INSERT INTO group_members (group_id, user_id, joined_at, role, preferred_language) "
              "VALUES (?, ?, toTimestamp(now()), ?, ?)">>,

    Params = [
        {uuid, GroupId},
        {uuid, UserId},
        {text, Role},
        {text, PreferredLanguage}
    ],

    execute_query(Query, Params).

get_group_members(GroupId, Limit) ->
    Query = <<"SELECT * FROM group_members WHERE group_id = ? LIMIT ?">>,
    Params = [{uuid, GroupId}, {int, Limit}],
    execute_query(Query, Params).

%%====================================================================
%% Translation Cache
%%====================================================================

cache_translation(SourceText, SourceLang, TargetLang, TranslatedText, Confidence) ->
    Query = <<"INSERT INTO translation_cache (source_text, source_lang, target_lang, "
              "translated_text, confidence, cached_at) "
              "VALUES (?, ?, ?, ?, ?, toTimestamp(now())) USING TTL 2592000">>,

    Params = [
        {text, SourceText},
        {text, SourceLang},
        {text, TargetLang},
        {text, TranslatedText},
        {float, Confidence}
    ],

    execute_query(Query, Params).

get_cached_translation(SourceText, SourceLang, TargetLang) ->
    Query = <<"SELECT * FROM translation_cache "
              "WHERE source_text = ? AND source_lang = ? AND target_lang = ?">>,

    Params = [
        {text, SourceText},
        {text, SourceLang},
        {text, TargetLang}
    ],

    case execute_query(Query, Params) of
        {ok, [Row]} -> {ok, Row};
        {ok, []} -> {error, not_found};
        Error -> Error
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

execute_query(Query, Params) ->
    PreparedQuery = erlcass:create_statement(Query, Params),

    case erlcass:execute(PreparedQuery) of
        {ok, Rows} when is_list(Rows) ->
            {ok, parse_rows(Rows)};
        {ok, _} ->
            ok;
        {error, Reason} ->
            lager:error("Cassandra query failed: ~p", [Reason]),
            {error, Reason}
    end.

parse_rows(Rows) ->
    [parse_row(Row) || Row <- Rows].

parse_row(Row) ->
    %% Convert Cassandra row to Erlang map
    maps:from_list(Row).
