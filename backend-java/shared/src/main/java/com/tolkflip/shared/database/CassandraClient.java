package com.tolkflip.shared.database;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.CqlSessionBuilder;
import com.datastax.oss.driver.api.core.cql.*;
import com.datastax.oss.driver.api.core.uuid.Uuids;
import com.datastax.oss.driver.api.querybuilder.QueryBuilder;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.Vertx;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetSocketAddress;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.CompletionStage;

/**
 * High-performance Cassandra client with async operations
 * Optimized for millions of concurrent users
 */
public class CassandraClient {
    private static final Logger logger = LoggerFactory.getLogger(CassandraClient.class);

    private final Vertx vertx;
    private CqlSession session;
    private final String keyspace;

    // Prepared statements cache for performance
    private final Map<String, PreparedStatement> preparedStatementsCache = new HashMap<>();

    public CassandraClient(Vertx vertx, List<String> contactPoints, String keyspace) {
        this.vertx = vertx;
        this.keyspace = keyspace;

        // Build session with optimized settings
        CqlSessionBuilder builder = CqlSession.builder();

        for (String contactPoint : contactPoints) {
            builder.addContactPoint(new InetSocketAddress(contactPoint, 9042));
        }

        builder.withLocalDatacenter("datacenter1")
               .withKeyspace(keyspace);

        this.session = builder.build();

        logger.info("Connected to Cassandra cluster, keyspace: {}", keyspace);
    }

    /**
     * Convenience constructor that reads from config JsonObject
     */
    public CassandraClient(Vertx vertx, io.vertx.core.json.JsonObject config) {
        this(vertx,
             config.getJsonObject("cassandra", new io.vertx.core.json.JsonObject())
                   .getJsonArray("contact_points", new io.vertx.core.json.JsonArray().add("127.0.0.1"))
                   .getList(),
             config.getJsonObject("cassandra", new io.vertx.core.json.JsonObject())
                   .getString("keyspace", "tolkflip"));
    }

    /**
     * Execute async query and return Future
     */
    public Future<ResultSet> executeAsync(String query, Object... params) {
        Promise<ResultSet> promise = Promise.promise();

        vertx.executeBlocking(blockingPromise -> {
            try {
                PreparedStatement ps = preparedStatementsCache.computeIfAbsent(
                    query,
                    q -> session.prepare(q)
                );

                BoundStatement bound = ps.bind(params);
                CompletionStage<AsyncResultSet> future = session.executeAsync(bound);

                future.whenComplete((result, error) -> {
                    if (error != null) {
                        blockingPromise.fail(error);
                    } else {
                        blockingPromise.complete(result);
                    }
                });
            } catch (Exception e) {
                blockingPromise.fail(e);
            }
        }, false, promise);

        return promise.future();
    }

    /**
     * Execute simple query without parameters
     */
    public Future<ResultSet> execute(SimpleStatement statement) {
        Promise<ResultSet> promise = Promise.promise();

        vertx.executeBlocking(blockingPromise -> {
            CompletionStage<AsyncResultSet> future = session.executeAsync(statement);

            future.whenComplete((result, error) -> {
                if (error != null) {
                    blockingPromise.fail(error);
                } else {
                    blockingPromise.complete(result);
                }
            });
        }, false, promise);

        return promise.future();
    }

    /**
     * Create a user
     */
    public Future<Void> createUser(UUID userId, String phoneNumber, String displayName,
                                    String primaryLanguage, List<String> additionalLanguages) {
        String query = "INSERT INTO users (user_id, phone_number, display_name, " +
                      "primary_language, additional_languages, created_at, last_active) " +
                      "VALUES (?, ?, ?, ?, ?, ?, ?)";

        Instant now = Instant.now();

        return executeAsync(query, userId, phoneNumber, displayName, primaryLanguage,
                          additionalLanguages, now, now)
            .mapEmpty();
    }

    /**
     * Get user by phone number
     */
    public Future<Row> getUserByPhone(String phoneNumber) {
        String query = "SELECT * FROM users WHERE phone_number = ? LIMIT 1";

        return executeAsync(query, phoneNumber)
            .map(rs -> rs.one());
    }

    /**
     * Get user by ID
     */
    public Future<Row> getUserById(UUID userId) {
        String query = "SELECT * FROM users WHERE user_id = ?";

        return executeAsync(query, userId)
            .map(rs -> rs.one());
    }

    /**
     * Save a message
     */
    public Future<UUID> saveMessage(UUID threadId, UUID senderId, UUID receiverId,
                                     String messageType, String content, String originalLanguage,
                                     String status, boolean isGroup) {
        String query = "INSERT INTO messages (thread_id, message_id, sender_id, receiver_id, " +
                      "message_type, content, original_language, timestamp, status, is_group) " +
                      "VALUES (?, now(), ?, ?, ?, ?, ?, ?, ?, ?)";

        UUID messageId = Uuids.timeBased();
        Instant now = Instant.now();

        return executeAsync(query, threadId, senderId, receiverId, messageType, content,
                          originalLanguage, now, status, isGroup)
            .map(rs -> messageId);
    }

    /**
     * Get messages for a thread with pagination
     */
    public Future<List<Row>> getMessages(UUID threadId, int limit) {
        String query = "SELECT * FROM messages WHERE thread_id = ? LIMIT ?";

        return executeAsync(query, threadId, limit)
            .map(rs -> {
                List<Row> rows = new ArrayList<>();
                rs.forEach(rows::add);
                return rows;
            });
    }

    /**
     * Create or update a chat thread
     */
    public Future<Void> createOrUpdateThread(UUID threadId, UUID participantId, UUID otherParticipantId,
                                              String threadType, Instant lastMessageTime,
                                              String lastMessagePreview, int unreadCount) {
        String query = "INSERT INTO chat_threads (thread_id, participant_id, other_participant_id, " +
                      "thread_type, created_at, last_message_time, last_message_preview, " +
                      "unread_count, is_archived) " +
                      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, false)";

        Instant now = Instant.now();

        return executeAsync(query, threadId, participantId, otherParticipantId, threadType,
                          now, lastMessageTime, lastMessagePreview, unreadCount)
            .mapEmpty();
    }

    /**
     * Get threads for a user
     */
    public Future<List<Row>> getThreadsForUser(UUID userId, int limit) {
        String query = "SELECT * FROM chat_threads WHERE participant_id = ? LIMIT ?";

        return executeAsync(query, userId, limit)
            .map(rs -> {
                List<Row> rows = new ArrayList<>();
                rs.forEach(rows::add);
                return rows;
            });
    }

    /**
     * Create a group
     */
    public Future<Void> createGroup(UUID groupId, String groupName, UUID createdBy,
                                     String description, int memberCount) {
        String query = "INSERT INTO group_chats (group_id, group_name, created_by, " +
                      "created_at, description, member_count) " +
                      "VALUES (?, ?, ?, ?, ?, ?)";

        Instant now = Instant.now();

        return executeAsync(query, groupId, groupName, createdBy, now, description, memberCount)
            .mapEmpty();
    }

    /**
     * Add a member to a group
     */
    public Future<Void> addGroupMember(UUID groupId, UUID userId, String role, String preferredLanguage) {
        String query = "INSERT INTO group_members (group_id, user_id, joined_at, role, preferred_language) " +
                      "VALUES (?, ?, ?, ?, ?)";

        Instant now = Instant.now();

        return executeAsync(query, groupId, userId, now, role, preferredLanguage)
            .mapEmpty();
    }

    /**
     * Get group members
     */
    public Future<List<Row>> getGroupMembers(UUID groupId, int limit) {
        String query = "SELECT * FROM group_members WHERE group_id = ? LIMIT ?";

        return executeAsync(query, groupId, limit)
            .map(rs -> {
                List<Row> rows = new ArrayList<>();
                rs.forEach(rows::add);
                return rows;
            });
    }

    /**
     * Cache a translation
     */
    public Future<Void> cacheTranslation(String sourceText, String sourceLang, String targetLang,
                                          String translatedText, float confidence) {
        String query = "INSERT INTO translation_cache (source_text, source_lang, target_lang, " +
                      "translated_text, confidence, cached_at) " +
                      "VALUES (?, ?, ?, ?, ?, ?) USING TTL 2592000";

        Instant now = Instant.now();

        return executeAsync(query, sourceText, sourceLang, targetLang, translatedText, confidence, now)
            .mapEmpty();
    }

    /**
     * Get cached translation
     */
    public Future<Row> getCachedTranslation(String sourceText, String sourceLang, String targetLang) {
        String query = "SELECT * FROM translation_cache " +
                      "WHERE source_text = ? AND source_lang = ? AND target_lang = ?";

        return executeAsync(query, sourceText, sourceLang, targetLang)
            .map(rs -> rs.one());
    }

    /**
     * Update message status
     */
    public Future<Void> updateMessageStatus(UUID threadId, UUID messageId, String status) {
        String query = "UPDATE messages SET status = ? WHERE thread_id = ? AND message_id = ?";

        return executeAsync(query, status, threadId, messageId)
            .mapEmpty();
    }

    // Overloaded methods with String parameters for convenience

    public Future<io.vertx.core.json.JsonObject> getUserById(String userId) {
        return getUserById(UUID.fromString(userId))
            .map(row -> row != null ? rowToJson(row) : null);
    }

    public Future<String> saveMessage(String threadId, String senderId, String receiverId,
                                       String messageType, String content, String originalLanguage,
                                       String status, boolean isGroup) {
        return saveMessage(
            UUID.fromString(threadId),
            UUID.fromString(senderId),
            UUID.fromString(receiverId),
            messageType, content, originalLanguage, status, isGroup
        ).map(UUID::toString);
    }

    public Future<io.vertx.core.json.JsonArray> getMessages(String threadId, int limit) {
        return getMessages(UUID.fromString(threadId), limit)
            .map(this::rowsToJsonArray);
    }

    public Future<Void> updateMessageStatus(String messageId, String status) {
        // Since we don't have thread_id, this is a simplified version
        // In production, you'd need to query for thread_id first or use a different table
        String query = "UPDATE messages SET status = ? WHERE message_id = ?";
        return executeAsync(query, status, UUID.fromString(messageId)).mapEmpty();
    }

    public Future<Void> updateUserProfile(String userId, String displayName,
                                          String profilePicture, String bio) {
        String query = "UPDATE users SET display_name = ?, profile_picture = ?, bio = ? WHERE user_id = ?";
        return executeAsync(query, displayName, profilePicture, bio, UUID.fromString(userId))
            .mapEmpty();
    }

    public Future<Void> updateUserLanguages(String userId, String nativeLanguage,
                                           List<String> learningLanguages) {
        String query = "UPDATE users SET primary_language = ?, additional_languages = ? WHERE user_id = ?";
        return executeAsync(query, nativeLanguage, learningLanguages, UUID.fromString(userId))
            .mapEmpty();
    }

    public Future<String> createGroup(String name, String description, String creatorId,
                                      List<String> memberIds) {
        UUID groupId = UUID.randomUUID();
        String query = "INSERT INTO group_chats (group_id, group_name, created_by, " +
                      "created_at, description, member_count) " +
                      "VALUES (?, ?, ?, ?, ?, ?)";

        Instant now = Instant.now();
        int memberCount = memberIds.size() + 1; // Including creator

        return executeAsync(query, groupId, name, UUID.fromString(creatorId),
                          now, description, memberCount)
            .compose(v -> {
                // Add creator as admin
                return addGroupMember(groupId.toString(), creatorId, "admin");
            })
            .compose(v -> {
                // Add other members
                List<Future> memberFutures = new ArrayList<>();
                for (String memberId : memberIds) {
                    memberFutures.add(addGroupMember(groupId.toString(), memberId, "member"));
                }
                return Future.all(memberFutures);
            })
            .map(v -> groupId.toString());
    }

    public Future<io.vertx.core.json.JsonObject> getGroup(String groupId) {
        String query = "SELECT * FROM group_chats WHERE group_id = ?";
        return executeAsync(query, UUID.fromString(groupId))
            .map(rs -> {
                Row row = rs.one();
                return row != null ? rowToJson(row) : null;
            });
    }

    public Future<Void> addGroupMember(String groupId, String userId, String role) {
        return addGroupMember(UUID.fromString(groupId), UUID.fromString(userId),
                            role, "en")
            .mapEmpty();
    }

    public Future<Void> removeGroupMember(String groupId, String userId) {
        String query = "DELETE FROM group_members WHERE group_id = ? AND user_id = ?";
        return executeAsync(query, UUID.fromString(groupId), UUID.fromString(userId))
            .mapEmpty();
    }

    public Future<Void> saveMedia(String mediaId, String fileName, String contentType, long fileSize) {
        String query = "INSERT INTO media_files (media_id, file_name, content_type, file_size, uploaded_at) " +
                      "VALUES (?, ?, ?, ?, ?)";
        Instant now = Instant.now();
        return executeAsync(query, UUID.fromString(mediaId), fileName, contentType, fileSize, now)
            .mapEmpty();
    }

    public Future<io.vertx.core.json.JsonObject> getMedia(String mediaId) {
        String query = "SELECT * FROM media_files WHERE media_id = ?";
        return executeAsync(query, UUID.fromString(mediaId))
            .map(rs -> {
                Row row = rs.one();
                return row != null ? rowToJson(row) : null;
            });
    }

    public Future<Void> saveNotification(String userId, String notificationId, String title,
                                         String message, String type) {
        String query = "INSERT INTO notifications (user_id, notification_id, title, message, " +
                      "type, created_at, is_read) " +
                      "VALUES (?, ?, ?, ?, ?, ?, false)";
        Instant now = Instant.now();
        return executeAsync(query, UUID.fromString(userId), UUID.fromString(notificationId),
                          title, message, type, now)
            .mapEmpty();
    }

    public Future<String> saveTranscription(String audioUrl, String transcribedText, String language) {
        UUID transcriptionId = UUID.randomUUID();
        String query = "INSERT INTO transcriptions (transcription_id, audio_url, transcribed_text, " +
                      "language, created_at) " +
                      "VALUES (?, ?, ?, ?, ?)";
        Instant now = Instant.now();
        return executeAsync(query, transcriptionId, audioUrl, transcribedText, language, now)
            .map(v -> transcriptionId.toString());
    }

    // Helper methods to convert Row to JSON
    private io.vertx.core.json.JsonObject rowToJson(Row row) {
        io.vertx.core.json.JsonObject json = new io.vertx.core.json.JsonObject();
        row.getColumnDefinitions().forEach(def -> {
            String name = def.getName().asInternal();
            Object value = row.getObject(name);
            if (value != null) {
                if (value instanceof UUID) {
                    json.put(name, value.toString());
                } else if (value instanceof Instant) {
                    json.put(name, ((Instant) value).toEpochMilli());
                } else if (value instanceof List) {
                    json.put(name, new io.vertx.core.json.JsonArray((List) value));
                } else {
                    json.put(name, value);
                }
            }
        });
        return json;
    }

    private io.vertx.core.json.JsonArray rowsToJsonArray(List<Row> rows) {
        io.vertx.core.json.JsonArray array = new io.vertx.core.json.JsonArray();
        for (Row row : rows) {
            array.add(rowToJson(row));
        }
        return array;
    }

    /**
     * Close the session
     */
    public void close() {
        if (session != null && !session.isClosed()) {
            session.close();
            logger.info("Cassandra session closed");
        }
    }
}
