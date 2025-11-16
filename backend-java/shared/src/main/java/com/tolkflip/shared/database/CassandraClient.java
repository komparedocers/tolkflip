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
