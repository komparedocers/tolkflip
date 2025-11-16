package com.tolkflip.chat;

import com.tolkflip.shared.database.CassandraClient;
import com.tolkflip.shared.database.RedisClient;
import com.tolkflip.shared.util.JwtUtil;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.http.ServerWebSocket;
import io.vertx.core.json.JsonObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ChatServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(ChatServiceVerticle.class);
    private static final int PORT = 3003;

    private CassandraClient cassandraClient;
    private RedisClient redisClient;
    private JwtUtil jwtUtil;

    // Store active WebSocket connections: userId -> WebSocket
    private final Map<String, ServerWebSocket> activeConnections = new ConcurrentHashMap<>();

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());
        redisClient = new RedisClient(vertx, config());
        jwtUtil = new JwtUtil(config());

        vertx.createHttpServer()
            .webSocketHandler(this::handleWebSocket)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Chat Service (WebSocket) started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void handleWebSocket(ServerWebSocket ws) {
        final String[] userId = {null};
        final boolean[] authenticated = {false};

        logger.info("WebSocket connection established from {}", ws.remoteAddress());

        ws.textMessageHandler(message -> {
            JsonObject data = new JsonObject(message);
            String action = data.getString("action");

            if (!authenticated[0] && !"authenticate".equals(action)) {
                ws.writeTextMessage(new JsonObject()
                    .put("type", "error")
                    .put("message", "Authentication required")
                    .encode());
                return;
            }

            switch (action) {
                case "authenticate":
                    handleAuthentication(ws, data, userId, authenticated);
                    break;
                case "send_message":
                    handleSendMessage(ws, data, userId[0]);
                    break;
                case "typing":
                    handleTyping(ws, data, userId[0]);
                    break;
                case "read_receipt":
                    handleReadReceipt(ws, data, userId[0]);
                    break;
                default:
                    ws.writeTextMessage(new JsonObject()
                        .put("type", "error")
                        .put("message", "Unknown action: " + action)
                        .encode());
            }
        });

        ws.closeHandler(v -> {
            if (userId[0] != null) {
                activeConnections.remove(userId[0]);
                logger.info("WebSocket disconnected: userId={}", userId[0]);
            }
        });

        ws.exceptionHandler(err -> {
            logger.error("WebSocket error", err);
            ws.close();
        });
    }

    private void handleAuthentication(ServerWebSocket ws, JsonObject data, String[] userId, boolean[] authenticated) {
        String token = data.getString("token");

        try {
            JsonObject claims = jwtUtil.verifyAccessToken(token);
            String uid = claims.getString("userId");

            userId[0] = uid;
            authenticated[0] = true;
            activeConnections.put(uid, ws);

            logger.info("WebSocket authenticated: userId={}", uid);

            ws.writeTextMessage(new JsonObject()
                .put("type", "authenticated")
                .put("userId", uid)
                .encode());

            // Set presence to online
            redisClient.hset("presence", uid, "online");

        } catch (Exception e) {
            logger.error("Authentication failed", e);
            ws.writeTextMessage(new JsonObject()
                .put("type", "error")
                .put("message", "Invalid token")
                .encode());
        }
    }

    private void handleSendMessage(ServerWebSocket ws, JsonObject data, String senderId) {
        String threadId = data.getString("threadId");
        String receiverId = data.getString("receiverId");
        String messageType = data.getString("messageType", "text");
        String content = data.getString("content");
        String originalLanguage = data.getString("originalLanguage", "en");
        boolean isGroup = data.getBoolean("isGroup", false);

        // Save message to Cassandra
        cassandraClient.saveMessage(
            threadId, senderId, receiverId, messageType,
            content, originalLanguage, "sent", isGroup
        ).onSuccess(messageId -> {
            JsonObject messageData = new JsonObject()
                .put("type", "message")
                .put("messageId", messageId)
                .put("threadId", threadId)
                .put("senderId", senderId)
                .put("receiverId", receiverId)
                .put("messageType", messageType)
                .put("content", content)
                .put("timestamp", System.currentTimeMillis());

            // Echo to sender
            ws.writeTextMessage(new JsonObject()
                .put("type", "message_sent")
                .put("messageId", messageId)
                .encode());

            // Send to receiver if online
            ServerWebSocket receiverWs = activeConnections.get(receiverId);
            if (receiverWs != null) {
                receiverWs.writeTextMessage(messageData.encode());
            }
        }).onFailure(err -> {
            logger.error("Failed to save message", err);
            ws.writeTextMessage(new JsonObject()
                .put("type", "error")
                .put("message", "Failed to send message")
                .encode());
        });
    }

    private void handleTyping(ServerWebSocket ws, JsonObject data, String userId) {
        String receiverId = data.getString("receiverId");
        boolean isTyping = data.getBoolean("isTyping", true);

        ServerWebSocket receiverWs = activeConnections.get(receiverId);
        if (receiverWs != null) {
            receiverWs.writeTextMessage(new JsonObject()
                .put("type", "typing")
                .put("userId", userId)
                .put("isTyping", isTyping)
                .encode());
        }
    }

    private void handleReadReceipt(ServerWebSocket ws, JsonObject data, String userId) {
        String messageId = data.getString("messageId");
        String senderId = data.getString("senderId");

        // Update message status in database
        cassandraClient.updateMessageStatus(messageId, "read")
            .onSuccess(v -> {
                // Notify sender
                ServerWebSocket senderWs = activeConnections.get(senderId);
                if (senderWs != null) {
                    senderWs.writeTextMessage(new JsonObject()
                        .put("type", "read_receipt")
                        .put("messageId", messageId)
                        .put("readBy", userId)
                        .encode());
                }
            });
    }

    @Override
    public void stop() {
        activeConnections.clear();
        if (cassandraClient != null) {
            cassandraClient.close();
        }
        if (redisClient != null) {
            redisClient.close();
        }
    }
}
