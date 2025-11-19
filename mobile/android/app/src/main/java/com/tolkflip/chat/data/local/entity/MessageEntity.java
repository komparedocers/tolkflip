package com.tolkflip.chat.data.local.entity;

import androidx.annotation.NonNull;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.ForeignKey;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import java.util.Map;

/**
 * Message entity for Room Database
 */
@Entity(
    tableName = "messages",
    foreignKeys = {
        @ForeignKey(
            entity = ThreadEntity.class,
            parentColumns = "thread_id",
            childColumns = "thread_id",
            onDelete = ForeignKey.CASCADE
        )
    },
    indices = {
        @Index(value = "thread_id"),
        @Index(value = "timestamp")
    }
)
public class MessageEntity {

    @PrimaryKey
    @NonNull
    @ColumnInfo(name = "message_id")
    private String messageId;

    @NonNull
    @ColumnInfo(name = "thread_id")
    private String threadId;

    @ColumnInfo(name = "sender_id")
    private String senderId;

    @ColumnInfo(name = "receiver_id")
    private String receiverId;

    @ColumnInfo(name = "content")
    private String content;

    @ColumnInfo(name = "message_type")
    private String messageType; // text, image, video, audio, file

    @ColumnInfo(name = "original_language")
    private String originalLanguage;

    @ColumnInfo(name = "translations")
    private Map<String, String> translations;

    @ColumnInfo(name = "timestamp")
    private long timestamp;

    @ColumnInfo(name = "status")
    private String status; // sending, sent, delivered, read, failed

    @ColumnInfo(name = "is_encrypted")
    private boolean isEncrypted;

    @ColumnInfo(name = "is_group")
    private boolean isGroup;

    @ColumnInfo(name = "media_url")
    private String mediaUrl;

    @ColumnInfo(name = "media_thumbnail")
    private String mediaThumbnail;

    @ColumnInfo(name = "duration")
    private int duration; // For audio/video messages (seconds)

    // Constructors
    public MessageEntity() {}

    public MessageEntity(@NonNull String messageId, @NonNull String threadId,
                        String senderId, String receiverId, String content,
                        String messageType, String originalLanguage) {
        this.messageId = messageId;
        this.threadId = threadId;
        this.senderId = senderId;
        this.receiverId = receiverId;
        this.content = content;
        this.messageType = messageType;
        this.originalLanguage = originalLanguage;
        this.timestamp = System.currentTimeMillis();
        this.status = "sending";
        this.isEncrypted = false;
        this.isGroup = false;
    }

    // Getters and Setters
    @NonNull
    public String getMessageId() { return messageId; }
    public void setMessageId(@NonNull String messageId) { this.messageId = messageId; }

    @NonNull
    public String getThreadId() { return threadId; }
    public void setThreadId(@NonNull String threadId) { this.threadId = threadId; }

    public String getSenderId() { return senderId; }
    public void setSenderId(String senderId) { this.senderId = senderId; }

    public String getReceiverId() { return receiverId; }
    public void setReceiverId(String receiverId) { this.receiverId = receiverId; }

    public String getContent() { return content; }
    public void setContent(String content) { this.content = content; }

    public String getMessageType() { return messageType; }
    public void setMessageType(String messageType) { this.messageType = messageType; }

    public String getOriginalLanguage() { return originalLanguage; }
    public void setOriginalLanguage(String originalLanguage) {
        this.originalLanguage = originalLanguage;
    }

    public Map<String, String> getTranslations() { return translations; }
    public void setTranslations(Map<String, String> translations) {
        this.translations = translations;
    }

    public long getTimestamp() { return timestamp; }
    public void setTimestamp(long timestamp) { this.timestamp = timestamp; }

    public String getStatus() { return status; }
    public void setStatus(String status) { this.status = status; }

    public boolean isEncrypted() { return isEncrypted; }
    public void setEncrypted(boolean encrypted) { isEncrypted = encrypted; }

    public boolean isGroup() { return isGroup; }
    public void setGroup(boolean group) { isGroup = group; }

    public String getMediaUrl() { return mediaUrl; }
    public void setMediaUrl(String mediaUrl) { this.mediaUrl = mediaUrl; }

    public String getMediaThumbnail() { return mediaThumbnail; }
    public void setMediaThumbnail(String mediaThumbnail) { this.mediaThumbnail = mediaThumbnail; }

    public int getDuration() { return duration; }
    public void setDuration(int duration) { this.duration = duration; }
}
