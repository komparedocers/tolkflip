package com.tolkflip.network;

import com.tolkflip.models.User;
import com.tolkflip.models.Message;

import java.util.List;
import java.util.Map;

import okhttp3.MultipartBody;
import okhttp3.RequestBody;
import okhttp3.ResponseBody;
import retrofit2.Call;
import retrofit2.http.*;

public interface ApiService {

    // Authentication endpoints
    @POST("/api/auth/request-code")
    Call<Map<String, Object>> requestVerificationCode(@Body Map<String, String> body);

    @POST("/api/auth/verify")
    Call<Map<String, Object>> verifyCode(@Body Map<String, String> body);

    @POST("/api/auth/refresh")
    Call<Map<String, Object>> refreshToken(@Body Map<String, String> body);

    @POST("/api/auth/logout")
    Call<Map<String, Object>> logout(@Body Map<String, String> body);

    // User endpoints
    @GET("/api/users/profile/{userId}")
    Call<Map<String, Object>> getUserProfile(@Path("userId") String userId);

    @PUT("/api/users/profile")
    Call<Map<String, Object>> updateUserProfile(@Body Map<String, Object> body);

    @GET("/api/users/search")
    Call<Map<String, Object>> searchUser(@Query("phone") String phoneNumber);

    @GET("/api/users/contacts")
    Call<Map<String, Object>> getContacts();

    @POST("/api/users/device-token")
    Call<Map<String, Object>> updateDeviceToken(@Body Map<String, String> body);

    // Chat endpoints
    @GET("/api/chat/threads")
    Call<Map<String, Object>> getChatThreads();

    @GET("/api/chat/threads/{threadId}/messages")
    Call<Map<String, Object>> getMessages(
        @Path("threadId") String threadId,
        @Query("limit") int limit,
        @Query("pageState") String pageState
    );

    @POST("/api/chat/threads/{threadId}/settings")
    Call<Map<String, Object>> updateThreadSettings(
        @Path("threadId") String threadId,
        @Body Map<String, Object> settings
    );

    // Translation endpoints
    @POST("/api/translate")
    Call<Map<String, Object>> translateText(@Body Map<String, String> body);

    @POST("/api/translate/batch")
    Call<Map<String, Object>> translateBatch(@Body Map<String, Object> body);

    @GET("/api/translate/languages")
    Call<Map<String, Object>> getSupportedLanguages();

    @POST("/api/translate/detect-language")
    Call<Map<String, Object>> detectLanguage(@Body Map<String, String> body);

    @POST("/api/translate/emotion")
    Call<Map<String, Object>> analyzeEmotion(@Body Map<String, String> body);

    // Transcription endpoints
    @Multipart
    @POST("/api/transcribe")
    Call<Map<String, Object>> transcribeAudio(
        @Part MultipartBody.Part audio,
        @Part("target_language") RequestBody targetLanguage,
        @Part("translate_to") RequestBody translateTo
    );

    // Media endpoints
    @Multipart
    @POST("/api/media/upload")
    Call<Map<String, Object>> uploadMedia(
        @Part MultipartBody.Part file,
        @Part("thread_id") RequestBody threadId,
        @Part("message_id") RequestBody messageId
    );

    @GET("/api/media/media/{mediaId}")
    Call<Map<String, Object>> getMediaUrl(
        @Path("mediaId") String mediaId,
        @Query("thumbnail") boolean thumbnail
    );

    @DELETE("/api/media/media/{mediaId}")
    Call<Map<String, Object>> deleteMedia(@Path("mediaId") String mediaId);

    // Presence endpoints
    @POST("/api/presence/online")
    Call<Map<String, Object>> setUserOnline(@Body Map<String, String> body);

    @POST("/api/presence/offline")
    Call<Map<String, Object>> setUserOffline();

    @GET("/api/presence/user/{userId}")
    Call<Map<String, Object>> getUserPresence(@Path("userId") String userId);

    @POST("/api/presence/users/batch")
    Call<Map<String, Object>> getBatchPresence(@Body Map<String, Object> body);

    @POST("/api/presence/typing")
    Call<Map<String, Object>> updateTypingStatus(@Body Map<String, Object> body);
}
