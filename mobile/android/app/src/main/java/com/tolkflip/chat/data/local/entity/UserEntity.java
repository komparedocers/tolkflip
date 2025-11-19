package com.tolkflip.chat.data.local.entity;

import androidx.annotation.NonNull;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import java.util.List;

/**
 * User entity for Room Database
 */
@Entity(tableName = "users")
public class UserEntity {

    @PrimaryKey
    @NonNull
    @ColumnInfo(name = "user_id")
    private String userId;

    @ColumnInfo(name = "phone_number")
    private String phoneNumber;

    @ColumnInfo(name = "display_name")
    private String displayName;

    @ColumnInfo(name = "avatar_url")
    private String avatarUrl;

    @ColumnInfo(name = "bio")
    private String bio;

    @ColumnInfo(name = "primary_language")
    private String primaryLanguage;

    @ColumnInfo(name = "additional_languages")
    private List<String> additionalLanguages;

    @ColumnInfo(name = "created_at")
    private long createdAt;

    @ColumnInfo(name = "last_active")
    private long lastActive;

    @ColumnInfo(name = "is_online")
    private boolean isOnline;

    // Constructors
    public UserEntity() {}

    public UserEntity(@NonNull String userId, String phoneNumber, String displayName,
                     String primaryLanguage, List<String> additionalLanguages) {
        this.userId = userId;
        this.phoneNumber = phoneNumber;
        this.displayName = displayName;
        this.primaryLanguage = primaryLanguage;
        this.additionalLanguages = additionalLanguages;
        this.createdAt = System.currentTimeMillis();
        this.lastActive = System.currentTimeMillis();
        this.isOnline = false;
    }

    // Getters and Setters
    @NonNull
    public String getUserId() { return userId; }
    public void setUserId(@NonNull String userId) { this.userId = userId; }

    public String getPhoneNumber() { return phoneNumber; }
    public void setPhoneNumber(String phoneNumber) { this.phoneNumber = phoneNumber; }

    public String getDisplayName() { return displayName; }
    public void setDisplayName(String displayName) { this.displayName = displayName; }

    public String getAvatarUrl() { return avatarUrl; }
    public void setAvatarUrl(String avatarUrl) { this.avatarUrl = avatarUrl; }

    public String getBio() { return bio; }
    public void setBio(String bio) { this.bio = bio; }

    public String getPrimaryLanguage() { return primaryLanguage; }
    public void setPrimaryLanguage(String primaryLanguage) { this.primaryLanguage = primaryLanguage; }

    public List<String> getAdditionalLanguages() { return additionalLanguages; }
    public void setAdditionalLanguages(List<String> additionalLanguages) {
        this.additionalLanguages = additionalLanguages;
    }

    public long getCreatedAt() { return createdAt; }
    public void setCreatedAt(long createdAt) { this.createdAt = createdAt; }

    public long getLastActive() { return lastActive; }
    public void setLastActive(long lastActive) { this.lastActive = lastActive; }

    public boolean isOnline() { return isOnline; }
    public void setOnline(boolean online) { isOnline = online; }
}
