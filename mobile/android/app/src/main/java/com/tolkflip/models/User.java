package com.tolkflip.models;

import java.util.List;

public class User {
    private String userId;
    private String phoneNumber;
    private String displayName;
    private String avatarUrl;
    private String primaryLanguage;
    private List<String> additionalLanguages;
    private long lastActive;

    public User() {}

    public User(String userId, String phoneNumber, String displayName,
                String avatarUrl, String primaryLanguage,
                List<String> additionalLanguages) {
        this.userId = userId;
        this.phoneNumber = phoneNumber;
        this.displayName = displayName;
        this.avatarUrl = avatarUrl;
        this.primaryLanguage = primaryLanguage;
        this.additionalLanguages = additionalLanguages;
    }

    // Getters and Setters
    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getPhoneNumber() {
        return phoneNumber;
    }

    public void setPhoneNumber(String phoneNumber) {
        this.phoneNumber = phoneNumber;
    }

    public String getDisplayName() {
        return displayName;
    }

    public void setDisplayName(String displayName) {
        this.displayName = displayName;
    }

    public String getAvatarUrl() {
        return avatarUrl;
    }

    public void setAvatarUrl(String avatarUrl) {
        this.avatarUrl = avatarUrl;
    }

    public String getPrimaryLanguage() {
        return primaryLanguage;
    }

    public void setPrimaryLanguage(String primaryLanguage) {
        this.primaryLanguage = primaryLanguage;
    }

    public List<String> getAdditionalLanguages() {
        return additionalLanguages;
    }

    public void setAdditionalLanguages(List<String> additionalLanguages) {
        this.additionalLanguages = additionalLanguages;
    }

    public long getLastActive() {
        return lastActive;
    }

    public void setLastActive(long lastActive) {
        this.lastActive = lastActive;
    }
}
