package com.tolkflip.chat.data.local.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.tolkflip.chat.data.local.entity.MessageEntity;

import java.util.List;

/**
 * Data Access Object for Messages
 */
@Dao
public interface MessageDao {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(MessageEntity message);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<MessageEntity> messages);

    @Update
    void update(MessageEntity message);

    @Delete
    void delete(MessageEntity message);

    @Query("DELETE FROM messages")
    void deleteAll();

    @Query("DELETE FROM messages WHERE thread_id = :threadId")
    void deleteThreadMessages(String threadId);

    @Query("SELECT * FROM messages WHERE message_id = :messageId LIMIT 1")
    MessageEntity getMessageById(String messageId);

    @Query("SELECT * FROM messages WHERE thread_id = :threadId ORDER BY timestamp ASC")
    LiveData<List<MessageEntity>> getThreadMessages(String threadId);

    @Query("SELECT * FROM messages WHERE thread_id = :threadId ORDER BY timestamp ASC LIMIT :limit")
    LiveData<List<MessageEntity>> getThreadMessagesLimited(String threadId, int limit);

    @Query("SELECT * FROM messages WHERE thread_id = :threadId ORDER BY timestamp DESC LIMIT :limit")
    List<MessageEntity> getRecentMessages(String threadId, int limit);

    @Query("SELECT * FROM messages WHERE thread_id = :threadId AND status = 'failed'")
    LiveData<List<MessageEntity>> getFailedMessages(String threadId);

    @Query("SELECT * FROM messages WHERE status = 'sending' OR status = 'failed'")
    List<MessageEntity> getPendingMessages();

    @Query("UPDATE messages SET status = :status WHERE message_id = :messageId")
    void updateMessageStatus(String messageId, String status);

    @Query("UPDATE messages SET status = 'read' WHERE thread_id = :threadId AND sender_id != :currentUserId AND status != 'read'")
    void markThreadAsRead(String threadId, String currentUserId);

    @Query("SELECT COUNT(*) FROM messages WHERE thread_id = :threadId AND sender_id != :currentUserId AND status != 'read'")
    int getUnreadCount(String threadId, String currentUserId);

    @Query("SELECT * FROM messages WHERE thread_id = :threadId ORDER BY timestamp DESC LIMIT 1")
    MessageEntity getLastMessage(String threadId);

    @Query("SELECT COUNT(*) FROM messages WHERE thread_id = :threadId")
    int getMessageCount(String threadId);

    @Query("DELETE FROM messages WHERE message_id IN (SELECT message_id FROM messages WHERE thread_id = :threadId ORDER BY timestamp ASC LIMIT (SELECT COUNT(*) - :keepCount FROM messages WHERE thread_id = :threadId))")
    void pruneOldMessages(String threadId, int keepCount);

    @Query("SELECT * FROM messages WHERE content LIKE '%' || :query || '%' ORDER BY timestamp DESC LIMIT :limit")
    List<MessageEntity> searchMessages(String query, int limit);
}
