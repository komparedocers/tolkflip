package com.tolkflip.network;

import android.util.Log;

import com.tolkflip.models.Message;
import com.tolkflip.utils.TokenManager;

import org.json.JSONException;
import org.json.JSONObject;

import java.net.URISyntaxException;

import io.socket.client.IO;
import io.socket.client.Socket;
import io.socket.emitter.Emitter;

public class WebSocketManager {
    private static final String TAG = "WebSocketManager";
    private static WebSocketManager instance;
    private Socket socket;
    private WebSocketListener listener;
    private String wsUrl;

    public interface WebSocketListener {
        void onConnected();
        void onDisconnected();
        void onNewMessage(Message message);
        void onMessageSent(String messageId, Message.MessageStatus status);
        void onMessageRead(String threadId, String messageId, String readBy);
        void onUserTyping(String userId, String threadId, boolean isTyping);
        void onPresenceUpdate(String userId, String status, long timestamp);
        void onMessageTranslated(String messageId, String translatedContent,
                                 String targetLanguage, String emotion);
        void onError(String error);
    }

    private WebSocketManager(String wsUrl) {
        this.wsUrl = wsUrl;
    }

    public static synchronized WebSocketManager getInstance(String wsUrl) {
        if (instance == null) {
            instance = new WebSocketManager(wsUrl);
        }
        return instance;
    }

    public void setListener(WebSocketListener listener) {
        this.listener = listener;
    }

    public void connect(String token) {
        try {
            IO.Options options = new IO.Options();
            options.auth = new java.util.HashMap<>();
            ((java.util.HashMap<String, String>) options.auth).put("token", token);
            options.reconnection = true;
            options.reconnectionAttempts = Integer.MAX_VALUE;
            options.reconnectionDelay = 1000;
            options.reconnectionDelayMax = 5000;
            options.timeout = 20000;

            socket = IO.socket(wsUrl, options);

            socket.on(Socket.EVENT_CONNECT, args -> {
                Log.d(TAG, "WebSocket connected");
                if (listener != null) {
                    listener.onConnected();
                }
            });

            socket.on(Socket.EVENT_DISCONNECT, args -> {
                Log.d(TAG, "WebSocket disconnected");
                if (listener != null) {
                    listener.onDisconnected();
                }
            });

            socket.on(Socket.EVENT_CONNECT_ERROR, args -> {
                Log.e(TAG, "Connection error: " + args[0]);
                if (listener != null) {
                    listener.onError("Connection error");
                }
            });

            socket.on("new_message", onNewMessage);
            socket.on("message_sent", onMessageSent);
            socket.on("message_read", onMessageRead);
            socket.on("user_typing", onUserTyping);
            socket.on("presence_update", onPresenceUpdate);
            socket.on("message_translated", onMessageTranslated);
            socket.on("message_error", onMessageError);

            socket.connect();

        } catch (URISyntaxException e) {
            Log.e(TAG, "Invalid WebSocket URL", e);
            if (listener != null) {
                listener.onError("Invalid WebSocket URL");
            }
        }
    }

    public void disconnect() {
        if (socket != null) {
            socket.disconnect();
            socket.off();
            socket = null;
        }
    }

    public boolean isConnected() {
        return socket != null && socket.connected();
    }

    public void sendMessage(Message message) {
        if (!isConnected()) {
            Log.e(TAG, "Cannot send message: not connected");
            return;
        }

        try {
            JSONObject data = new JSONObject();
            data.put("threadId", message.getThreadId());
            data.put("receiverId", message.getReceiverId());
            data.put("content", message.getContent());
            data.put("messageType", message.getMessageType().name().toLowerCase());
            data.put("originalLanguage", message.getOriginalLanguage());
            data.put("isGroup", false);

            if (message.getMediaUrls() != null && !message.getMediaUrls().isEmpty()) {
                data.put("mediaUrls", new org.json.JSONArray(message.getMediaUrls()));
            }

            if (message.getEncryptedContent() != null) {
                data.put("encryptedContent",
                    android.util.Base64.encodeToString(message.getEncryptedContent(),
                                                       android.util.Base64.DEFAULT));
            }

            socket.emit("send_message", data);

        } catch (JSONException e) {
            Log.e(TAG, "Error creating message JSON", e);
            if (listener != null) {
                listener.onError("Failed to send message");
            }
        }
    }

    public void markMessageRead(String threadId, String messageId) {
        if (!isConnected()) return;

        try {
            JSONObject data = new JSONObject();
            data.put("threadId", threadId);
            data.put("messageId", messageId);
            socket.emit("mark_read", data);
        } catch (JSONException e) {
            Log.e(TAG, "Error marking message as read", e);
        }
    }

    public void setTyping(String threadId, boolean isTyping) {
        if (!isConnected()) return;

        try {
            JSONObject data = new JSONObject();
            data.put("threadId", threadId);
            data.put("isTyping", isTyping);
            socket.emit("typing", data);
        } catch (JSONException e) {
            Log.e(TAG, "Error sending typing indicator", e);
        }
    }

    public void joinThread(String threadId) {
        if (!isConnected()) return;

        try {
            JSONObject data = new JSONObject();
            data.put("threadId", threadId);
            socket.emit("join_thread", data);
        } catch (JSONException e) {
            Log.e(TAG, "Error joining thread", e);
        }
    }

    public void leaveThread(String threadId) {
        if (!isConnected()) return;

        try {
            JSONObject data = new JSONObject();
            data.put("threadId", threadId);
            socket.emit("leave_thread", data);
        } catch (JSONException e) {
            Log.e(TAG, "Error leaving thread", e);
        }
    }

    // Event listeners
    private final Emitter.Listener onNewMessage = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            Message message = parseMessage(data);
            if (listener != null) {
                listener.onNewMessage(message);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing new message", e);
        }
    };

    private final Emitter.Listener onMessageSent = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            String messageId = data.getString("messageId");
            String status = data.getString("status");

            Message.MessageStatus msgStatus = Message.MessageStatus.valueOf(status.toUpperCase());

            if (listener != null) {
                listener.onMessageSent(messageId, msgStatus);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing message sent", e);
        }
    };

    private final Emitter.Listener onMessageRead = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            String threadId = data.getString("threadId");
            String messageId = data.getString("messageId");
            String readBy = data.getString("readBy");

            if (listener != null) {
                listener.onMessageRead(threadId, messageId, readBy);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing message read", e);
        }
    };

    private final Emitter.Listener onUserTyping = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            String userId = data.getString("userId");
            String threadId = data.getString("threadId");
            boolean isTyping = data.getBoolean("isTyping");

            if (listener != null) {
                listener.onUserTyping(userId, threadId, isTyping);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing user typing", e);
        }
    };

    private final Emitter.Listener onPresenceUpdate = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            String userId = data.getString("userId");
            String status = data.getString("status");
            long timestamp = data.getLong("timestamp");

            if (listener != null) {
                listener.onPresenceUpdate(userId, status, timestamp);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing presence update", e);
        }
    };

    private final Emitter.Listener onMessageTranslated = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            String messageId = data.getString("messageId");
            String translatedContent = data.getString("translatedContent");
            String targetLanguage = data.getString("targetLanguage");
            String emotion = data.optString("emotion", "neutral");

            if (listener != null) {
                listener.onMessageTranslated(messageId, translatedContent,
                                            targetLanguage, emotion);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing message translation", e);
        }
    };

    private final Emitter.Listener onMessageError = args -> {
        try {
            JSONObject data = (JSONObject) args[0];
            String error = data.getString("error");

            if (listener != null) {
                listener.onError(error);
            }
        } catch (JSONException e) {
            Log.e(TAG, "Error parsing message error", e);
        }
    };

    private Message parseMessage(JSONObject data) throws JSONException {
        Message message = new Message();
        message.setMessageId(data.getString("messageId"));
        message.setThreadId(data.getString("threadId"));
        message.setSenderId(data.getString("senderId"));
        message.setReceiverId(data.getString("receiverId"));
        message.setContent(data.getString("content"));
        message.setOriginalLanguage(data.getString("originalLanguage"));
        message.setTimestamp(data.getLong("timestamp"));

        String typeStr = data.getString("messageType");
        message.setMessageType(Message.MessageType.valueOf(typeStr.toUpperCase()));

        String statusStr = data.getString("status");
        message.setStatus(Message.MessageStatus.valueOf(statusStr.toUpperCase()));

        return message;
    }
}
