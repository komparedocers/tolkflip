package com.tolkflip.chat.data.local.entity;

import androidx.annotation.NonNull;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.Index;
import androidx.room.PrimaryKey;

/**
 * Thread entity for Room Database
 */
@Entity(
    tableName = "threads",
    indices = {
        @Index(value = "last_message_time", order = Index.Order.DESC)
    }
)
public class ThreadEntity {

    @PrimaryKey
    @NonNull
    @ColumnInfo(name = "thread_id")
    private String threadId;

    @ColumnInfo(name = "participant_id")
    private String participantId;

    @ColumnInfo(name = "other_participant_id")
    private String otherParticipantId;

    @ColumnInfo(name = "thread_type")
    private String threadType; // direct, group

    @ColumnInfo(name = "name")
    private String name;

    @ColumnInfo(name = "avatar_url")
    private String avatarUrl;

    @ColumnInfo(name = "last_message_preview")
    private String lastMessagePreview;

    @ColumnInfo(name = "last_message_time")
    private long lastMessageTime;

    @ColumnInfo(name = "unread_count")
    private int unreadCount;

    @ColumnInfo(name = "is_archived")
    private boolean isArchived;

    @ColumnInfo(name = "is_muted")
    private boolean isMuted;

    @ColumnInfo(name = "is_pinned")
    private boolean isPinned;

    @ColumnInfo(name = "created_at")
    private long createdAt;

    // Constructors
    public ThreadEntity() {}

    public ThreadEntity(@NonNull String threadId, String participantId,
                       String otherParticipantId, String threadType, String name) {
        this.threadId = threadId;
        this.participantId = participantId;
        this.otherParticipantId = otherParticipantId;
        this.threadType = threadType;
        this.name = name;
        this.createdAt = System.currentTimeMillis();
        this.lastMessageTime = System.currentTimeMillis();
        this.unreadCount = 0;
        this.isArchived = false;
        this.isMuted = false;
        this.isPinned = false;
    }

    // Getters and Setters
    @NonNull
    public String getThreadId() { return threadId; }
    public void setThreadId(@NonNull String threadId) { this.threadId = threadId; }

    public String getParticipantId() { return participantId; }
    public void setParticipantId(String participantId) { this.participantId = participantId; }

    public String getOtherParticipantId() { return otherParticipantId; }
    public void setOtherParticipantId(String otherParticipantId) {
        this.otherParticipantId = otherParticipantId;
    }

    public String getThreadType() { return threadType; }
    public void setThreadType(String threadType) { this.threadType = threadType; }

    public String getName() { return name; }
    public void setName(String name) { this.name = name; }

    public String getAvatarUrl() { return avatarUrl; }
    public void setAvatarUrl(String avatarUrl) { this.avatarUrl = avatarUrl; }

    public String getLastMessagePreview() { return lastMessagePreview; }
    public void setLastMessagePreview(String lastMessagePreview) {
        this.lastMessagePreview = lastMessagePreview;
    }

    public long getLastMessageTime() { return lastMessageTime; }
    public void setLastMessageTime(long lastMessageTime) {
        this.lastMessageTime = lastMessageTime;
    }

    public int getUnreadCount() { return unreadCount; }
    public void setUnreadCount(int unreadCount) { this.unreadCount = unreadCount; }

    public boolean isArchived() { return isArchived; }
    public void setArchived(boolean archived) { isArchived = archived; }

    public boolean isMuted() { return isMuted; }
    public void setMuted(boolean muted) { isMuted = muted; }

    public boolean isPinned() { return isPinned; }
    public void setPinned(boolean pinned) { isPinned = pinned; }

    public long getCreatedAt() { return createdAt; }
    public void setCreatedAt(long createdAt) { this.createdAt = createdAt; }
}
