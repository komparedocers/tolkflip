package com.tolkflip.chat.data.local.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.tolkflip.chat.data.local.entity.ThreadEntity;

import java.util.List;

/**
 * Data Access Object for Threads
 */
@Dao
public interface ThreadDao {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(ThreadEntity thread);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<ThreadEntity> threads);

    @Update
    void update(ThreadEntity thread);

    @Delete
    void delete(ThreadEntity thread);

    @Query("DELETE FROM threads")
    void deleteAll();

    @Query("SELECT * FROM threads WHERE thread_id = :threadId LIMIT 1")
    LiveData<ThreadEntity> getThreadById(String threadId);

    @Query("SELECT * FROM threads WHERE thread_id = :threadId LIMIT 1")
    ThreadEntity getThreadByIdSync(String threadId);

    @Query("SELECT * FROM threads WHERE participant_id = :userId AND is_archived = 0 ORDER BY is_pinned DESC, last_message_time DESC")
    LiveData<List<ThreadEntity>> getAllThreads(String userId);

    @Query("SELECT * FROM threads WHERE participant_id = :userId AND is_archived = 1 ORDER BY last_message_time DESC")
    LiveData<List<ThreadEntity>> getArchivedThreads(String userId);

    @Query("SELECT * FROM threads WHERE participant_id = :userId AND is_pinned = 1 ORDER BY last_message_time DESC")
    LiveData<List<ThreadEntity>> getPinnedThreads(String userId);

    @Query("SELECT * FROM threads WHERE participant_id = :userId AND unread_count > 0 ORDER BY last_message_time DESC")
    LiveData<List<ThreadEntity>> getUnreadThreads(String userId);

    @Query("SELECT SUM(unread_count) FROM threads WHERE participant_id = :userId")
    LiveData<Integer> getTotalUnreadCount(String userId);

    @Query("UPDATE threads SET last_message_preview = :preview, last_message_time = :timestamp WHERE thread_id = :threadId")
    void updateLastMessage(String threadId, String preview, long timestamp);

    @Query("UPDATE threads SET unread_count = :count WHERE thread_id = :threadId")
    void updateUnreadCount(String threadId, int count);

    @Query("UPDATE threads SET unread_count = 0 WHERE thread_id = :threadId")
    void markAsRead(String threadId);

    @Query("UPDATE threads SET is_archived = :archived WHERE thread_id = :threadId")
    void setArchived(String threadId, boolean archived);

    @Query("UPDATE threads SET is_muted = :muted WHERE thread_id = :threadId")
    void setMuted(String threadId, boolean muted);

    @Query("UPDATE threads SET is_pinned = :pinned WHERE thread_id = :threadId")
    void setPinned(String threadId, boolean pinned);

    @Query("SELECT COUNT(*) FROM threads WHERE participant_id = :userId AND is_archived = 0")
    int getThreadCount(String userId);

    @Query("SELECT * FROM threads WHERE participant_id = :userId AND (name LIKE '%' || :query || '%' OR last_message_preview LIKE '%' || :query || '%') ORDER BY last_message_time DESC")
    LiveData<List<ThreadEntity>> searchThreads(String userId, String query);
}
