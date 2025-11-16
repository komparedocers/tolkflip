package com.tolkflip.webrtc;

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

public class WebRTCServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(WebRTCServiceVerticle.class);
    private static final int PORT = 3010;

    private RedisClient redisClient;
    private JwtUtil jwtUtil;

    // Store active WebSocket connections: userId -> WebSocket
    private final Map<String, ServerWebSocket> activeConnections = new ConcurrentHashMap<>();

    @Override
    public void start(Promise<Void> startPromise) {
        redisClient = new RedisClient(vertx, config());
        jwtUtil = new JwtUtil(config());

        vertx.createHttpServer()
            .webSocketHandler(this::handleWebSocket)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("WebRTC Service (WebSocket) started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void handleWebSocket(ServerWebSocket ws) {
        final String[] userId = {null};
        final boolean[] authenticated = {false};

        logger.info("WebRTC WebSocket connection established from {}", ws.remoteAddress());

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
                case "call:initiate":
                    handleCallInitiate(ws, data, userId[0]);
                    break;
                case "call:answer":
                    handleCallAnswer(ws, data, userId[0]);
                    break;
                case "call:reject":
                    handleCallReject(ws, data, userId[0]);
                    break;
                case "call:end":
                    handleCallEnd(ws, data, userId[0]);
                    break;
                case "webrtc:offer":
                    handleOffer(ws, data, userId[0]);
                    break;
                case "webrtc:answer":
                    handleAnswer(ws, data, userId[0]);
                    break;
                case "webrtc:ice-candidate":
                    handleIceCandidate(ws, data, userId[0]);
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
                logger.info("WebRTC WebSocket disconnected: userId={}", userId[0]);
            }
        });

        ws.exceptionHandler(err -> {
            logger.error("WebRTC WebSocket error", err);
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

            logger.info("WebRTC WebSocket authenticated: userId={}", uid);

            ws.writeTextMessage(new JsonObject()
                .put("type", "authenticated")
                .put("userId", uid)
                .encode());

        } catch (Exception e) {
            logger.error("WebRTC authentication failed", e);
            ws.writeTextMessage(new JsonObject()
                .put("type", "error")
                .put("message", "Invalid token")
                .encode());
        }
    }

    private void handleCallInitiate(ServerWebSocket ws, JsonObject data, String callerId) {
        String calleeId = data.getString("calleeId");
        String callType = data.getString("callType", "audio");
        String threadId = data.getString("threadId", "");

        String callId = System.currentTimeMillis() + ":" + callerId;

        JsonObject callData = new JsonObject()
            .put("callId", callId)
            .put("caller", callerId)
            .put("callee", calleeId)
            .put("callType", callType)
            .put("threadId", threadId)
            .put("status", "ringing")
            .put("timestamp", System.currentTimeMillis());

        // Store call in Redis
        redisClient.setex("webrtc:call:" + callId, 3600, callData.encode());

        // Notify callee
        ServerWebSocket calleeWs = activeConnections.get(calleeId);
        if (calleeWs != null) {
            calleeWs.writeTextMessage(new JsonObject()
                .put("type", "call:incoming")
                .put("callId", callId)
                .put("callerId", callerId)
                .put("callType", callType)
                .put("threadId", threadId)
                .encode());
        }

        // Echo to caller
        ws.writeTextMessage(new JsonObject()
            .put("type", "call:initiated")
            .put("callId", callId)
            .encode());
    }

    private void handleCallAnswer(ServerWebSocket ws, JsonObject data, String userId) {
        String callId = data.getString("callId");

        redisClient.get("webrtc:call:" + callId).onSuccess(callDataStr -> {
            if (callDataStr != null && !callDataStr.isEmpty()) {
                JsonObject callData = new JsonObject(callDataStr);
                String callerId = callData.getString("caller");

                callData.put("status", "active");
                redisClient.setex("webrtc:call:" + callId, 3600, callData.encode());

                // Notify caller
                ServerWebSocket callerWs = activeConnections.get(callerId);
                if (callerWs != null) {
                    callerWs.writeTextMessage(new JsonObject()
                        .put("type", "call:answered")
                        .put("callId", callId)
                        .encode());
                }
            }
        });
    }

    private void handleCallReject(ServerWebSocket ws, JsonObject data, String userId) {
        String callId = data.getString("callId");
        String reason = data.getString("reason", "rejected");

        redisClient.get("webrtc:call:" + callId).onSuccess(callDataStr -> {
            if (callDataStr != null && !callDataStr.isEmpty()) {
                JsonObject callData = new JsonObject(callDataStr);
                String callerId = callData.getString("caller");

                redisClient.del("webrtc:call:" + callId);

                // Notify caller
                ServerWebSocket callerWs = activeConnections.get(callerId);
                if (callerWs != null) {
                    callerWs.writeTextMessage(new JsonObject()
                        .put("type", "call:rejected")
                        .put("callId", callId)
                        .put("reason", reason)
                        .encode());
                }
            }
        });
    }

    private void handleCallEnd(ServerWebSocket ws, JsonObject data, String userId) {
        String callId = data.getString("callId");

        redisClient.get("webrtc:call:" + callId).onSuccess(callDataStr -> {
            if (callDataStr != null && !callDataStr.isEmpty()) {
                JsonObject callData = new JsonObject(callDataStr);
                String callerId = callData.getString("caller");
                String calleeId = callData.getString("callee");

                redisClient.del("webrtc:call:" + callId);

                // Notify other participant
                String otherUserId = userId.equals(callerId) ? calleeId : callerId;
                ServerWebSocket otherWs = activeConnections.get(otherUserId);
                if (otherWs != null) {
                    otherWs.writeTextMessage(new JsonObject()
                        .put("type", "call:ended")
                        .put("callId", callId)
                        .encode());
                }
            }
        });
    }

    private void handleOffer(ServerWebSocket ws, JsonObject data, String userId) {
        String callId = data.getString("callId");
        JsonObject offer = data.getJsonObject("offer");

        redisClient.get("webrtc:call:" + callId).onSuccess(callDataStr -> {
            if (callDataStr != null && !callDataStr.isEmpty()) {
                JsonObject callData = new JsonObject(callDataStr);
                String calleeId = callData.getString("callee");

                ServerWebSocket calleeWs = activeConnections.get(calleeId);
                if (calleeWs != null) {
                    calleeWs.writeTextMessage(new JsonObject()
                        .put("type", "webrtc:offer")
                        .put("callId", callId)
                        .put("offer", offer)
                        .encode());
                }
            }
        });
    }

    private void handleAnswer(ServerWebSocket ws, JsonObject data, String userId) {
        String callId = data.getString("callId");
        JsonObject answer = data.getJsonObject("answer");

        redisClient.get("webrtc:call:" + callId).onSuccess(callDataStr -> {
            if (callDataStr != null && !callDataStr.isEmpty()) {
                JsonObject callData = new JsonObject(callDataStr);
                String callerId = callData.getString("caller");

                ServerWebSocket callerWs = activeConnections.get(callerId);
                if (callerWs != null) {
                    callerWs.writeTextMessage(new JsonObject()
                        .put("type", "webrtc:answer")
                        .put("callId", callId)
                        .put("answer", answer)
                        .encode());
                }
            }
        });
    }

    private void handleIceCandidate(ServerWebSocket ws, JsonObject data, String userId) {
        String callId = data.getString("callId");
        JsonObject candidate = data.getJsonObject("candidate");

        redisClient.get("webrtc:call:" + callId).onSuccess(callDataStr -> {
            if (callDataStr != null && !callDataStr.isEmpty()) {
                JsonObject callData = new JsonObject(callDataStr);
                String callerId = callData.getString("caller");
                String calleeId = callData.getString("callee");

                // Send to other participant
                String targetUserId = userId.equals(callerId) ? calleeId : callerId;
                ServerWebSocket targetWs = activeConnections.get(targetUserId);
                if (targetWs != null) {
                    targetWs.writeTextMessage(new JsonObject()
                        .put("type", "webrtc:ice-candidate")
                        .put("callId", callId)
                        .put("candidate", candidate)
                        .encode());
                }
            }
        });
    }

    @Override
    public void stop() {
        activeConnections.clear();
        if (redisClient != null) {
            redisClient.close();
        }
    }
}
