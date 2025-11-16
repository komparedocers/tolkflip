package com.tolkflip.models;

import androidx.room.Entity;
import androidx.room.PrimaryKey;
import androidx.room.TypeConverters;

import com.tolkflip.utils.Converters;

import java.util.List;
import java.util.Map;

@Entity(tableName = "messages")
@TypeConverters(Converters.class)
public class Message {
    @PrimaryKey(autoGenerate = false)
    private String messageId;
    private String threadId;
    private String senderId;
    private String receiverId;
    private String content;
    private String translatedContent;
    private String originalLanguage;
    private String targetLanguage;
    private MessageType messageType;
    private MessageStatus status;
    private long timestamp;
    private boolean isEncrypted;
    private byte[] encryptedContent;
    private List<String> mediaUrls;
    private Map<String, String> metadata;
    private String emotion;
    private boolean showOriginal;

    public enum MessageType {
        TEXT, IMAGE, VIDEO, AUDIO, FILE, VOICE_NOTE
    }

    public enum MessageStatus {
        SENDING, SENT, DELIVERED, READ, FAILED
    }

    public Message() {
        this.timestamp = System.currentTimeMillis();
        this.status = MessageStatus.SENDING;
        this.showOriginal = false;
    }

    // Getters and Setters
    public String getMessageId() {
        return messageId;
    }

    public void setMessageId(String messageId) {
        this.messageId = messageId;
    }

    public String getThreadId() {
        return threadId;
    }

    public void setThreadId(String threadId) {
        this.threadId = threadId;
    }

    public String getSenderId() {
        return senderId;
    }

    public void setSenderId(String senderId) {
        this.senderId = senderId;
    }

    public String getReceiverId() {
        return receiverId;
    }

    public void setReceiverId(String receiverId) {
        this.receiverId = receiverId;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public String getTranslatedContent() {
        return translatedContent;
    }

    public void setTranslatedContent(String translatedContent) {
        this.translatedContent = translatedContent;
    }

    public String getOriginalLanguage() {
        return originalLanguage;
    }

    public void setOriginalLanguage(String originalLanguage) {
        this.originalLanguage = originalLanguage;
    }

    public String getTargetLanguage() {
        return targetLanguage;
    }

    public void setTargetLanguage(String targetLanguage) {
        this.targetLanguage = targetLanguage;
    }

    public MessageType getMessageType() {
        return messageType;
    }

    public void setMessageType(MessageType messageType) {
        this.messageType = messageType;
    }

    public MessageStatus getStatus() {
        return status;
    }

    public void setStatus(MessageStatus status) {
        this.status = status;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    public boolean isEncrypted() {
        return isEncrypted;
    }

    public void setEncrypted(boolean encrypted) {
        isEncrypted = encrypted;
    }

    public byte[] getEncryptedContent() {
        return encryptedContent;
    }

    public void setEncryptedContent(byte[] encryptedContent) {
        this.encryptedContent = encryptedContent;
    }

    public List<String> getMediaUrls() {
        return mediaUrls;
    }

    public void setMediaUrls(List<String> mediaUrls) {
        this.mediaUrls = mediaUrls;
    }

    public Map<String, String> getMetadata() {
        return metadata;
    }

    public void setMetadata(Map<String, String> metadata) {
        this.metadata = metadata;
    }

    public String getEmotion() {
        return emotion;
    }

    public void setEmotion(String emotion) {
        this.emotion = emotion;
    }

    public boolean isShowOriginal() {
        return showOriginal;
    }

    public void setShowOriginal(boolean showOriginal) {
        this.showOriginal = showOriginal;
    }

    public String getDisplayContent() {
        if (showOriginal || translatedContent == null || translatedContent.isEmpty()) {
            return content;
        }
        return translatedContent;
    }
}
