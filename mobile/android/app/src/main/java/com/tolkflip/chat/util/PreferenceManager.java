package com.tolkflip.chat.util;

import android.content.Context;
import android.content.SharedPreferences;

/**
 * Utility class for managing SharedPreferences
 */
public class PreferenceManager {

    private static final String PREF_NAME = "tolkflip_preferences";

    // Keys
    private static final String KEY_USER_ID = "user_id";
    private static final String KEY_ACCESS_TOKEN = "access_token";
    private static final String KEY_REFRESH_TOKEN = "refresh_token";
    private static final String KEY_PHONE_NUMBER = "phone_number";
    private static final String KEY_DISPLAY_NAME = "display_name";
    private static final String KEY_PRIMARY_LANGUAGE = "primary_language";
    private static final String KEY_IS_LOGGED_IN = "is_logged_in";

    /**
     * Get SharedPreferences instance
     */
    private static SharedPreferences getPreferences(Context context) {
        return context.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    }

    /**
     * Save user ID
     */
    public static void saveUserId(Context context, String userId) {
        getPreferences(context).edit()
            .putString(KEY_USER_ID, userId)
            .apply();
    }

    /**
     * Get user ID
     */
    public static String getUserId(Context context) {
        return getPreferences(context).getString(KEY_USER_ID, null);
    }

    /**
     * Save auth tokens
     */
    public static void saveTokens(Context context, String accessToken, String refreshToken) {
        getPreferences(context).edit()
            .putString(KEY_ACCESS_TOKEN, accessToken)
            .putString(KEY_REFRESH_TOKEN, refreshToken)
            .apply();
    }

    /**
     * Get access token
     */
    public static String getAccessToken(Context context) {
        return getPreferences(context).getString(KEY_ACCESS_TOKEN, null);
    }

    /**
     * Get refresh token
     */
    public static String getRefreshToken(Context context) {
        return getPreferences(context).getString(KEY_REFRESH_TOKEN, null);
    }

    /**
     * Save phone number
     */
    public static void savePhoneNumber(Context context, String phoneNumber) {
        getPreferences(context).edit()
            .putString(KEY_PHONE_NUMBER, phoneNumber)
            .apply();
    }

    /**
     * Get phone number
     */
    public static String getPhoneNumber(Context context) {
        return getPreferences(context).getString(KEY_PHONE_NUMBER, null);
    }

    /**
     * Save display name
     */
    public static void saveDisplayName(Context context, String displayName) {
        getPreferences(context).edit()
            .putString(KEY_DISPLAY_NAME, displayName)
            .apply();
    }

    /**
     * Get display name
     */
    public static String getDisplayName(Context context) {
        return getPreferences(context).getString(KEY_DISPLAY_NAME, "User");
    }

    /**
     * Save primary language
     */
    public static void savePrimaryLanguage(Context context, String language) {
        getPreferences(context).edit()
            .putString(KEY_PRIMARY_LANGUAGE, language)
            .apply();
    }

    /**
     * Get primary language
     */
    public static String getPrimaryLanguage(Context context) {
        return getPreferences(context).getString(KEY_PRIMARY_LANGUAGE, "en");
    }

    /**
     * Set logged in status
     */
    public static void setLoggedIn(Context context, boolean isLoggedIn) {
        getPreferences(context).edit()
            .putBoolean(KEY_IS_LOGGED_IN, isLoggedIn)
            .apply();
    }

    /**
     * Check if user is logged in
     */
    public static boolean isLoggedIn(Context context) {
        return getPreferences(context).getBoolean(KEY_IS_LOGGED_IN, false);
    }

    /**
     * Save complete user session
     */
    public static void saveUserSession(Context context, String userId, String accessToken,
                                      String refreshToken, String phoneNumber,
                                      String displayName, String primaryLanguage) {
        getPreferences(context).edit()
            .putString(KEY_USER_ID, userId)
            .putString(KEY_ACCESS_TOKEN, accessToken)
            .putString(KEY_REFRESH_TOKEN, refreshToken)
            .putString(KEY_PHONE_NUMBER, phoneNumber)
            .putString(KEY_DISPLAY_NAME, displayName)
            .putString(KEY_PRIMARY_LANGUAGE, primaryLanguage)
            .putBoolean(KEY_IS_LOGGED_IN, true)
            .apply();
    }

    /**
     * Clear all user data (logout)
     */
    public static void clearSession(Context context) {
        getPreferences(context).edit().clear().apply();
    }
}
