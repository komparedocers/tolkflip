package com.tolkflip.notification;

import com.tolkflip.shared.database.CassandraClient;
import com.tolkflip.shared.database.RedisClient;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.UUID;

public class NotificationServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(NotificationServiceVerticle.class);
    private static final int PORT = 3009;

    private CassandraClient cassandraClient;
    private RedisClient redisClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());
        redisClient = new RedisClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/api/notifications/send").handler(this::sendNotification);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Notification Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void sendNotification(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();

        String userId = body.getString("userId");
        String title = body.getString("title");
        String message = body.getString("message");
        String notificationType = body.getString("type", "message");
        JsonObject additionalData = body.getJsonObject("data", new JsonObject());

        // Get user's FCM token from Redis
        redisClient.hget("fcm_tokens", userId)
            .onSuccess(fcmToken -> {
                if (fcmToken != null && !fcmToken.isEmpty()) {
                    // Send FCM notification
                    String notificationId = sendFCMNotification(
                        fcmToken, title, message, notificationType, additionalData
                    );

                    // Store notification in database
                    cassandraClient.saveNotification(userId, notificationId, title, message, notificationType)
                        .onSuccess(v -> {
                            ctx.response()
                                .putHeader("Content-Type", "application/json")
                                .end(new JsonObject()
                                    .put("success", true)
                                    .put("notificationId", notificationId)
                                    .encode());
                        })
                        .onFailure(err -> {
                            logger.error("Failed to save notification", err);
                            // Still return success since FCM was sent
                            ctx.response()
                                .putHeader("Content-Type", "application/json")
                                .end(new JsonObject()
                                    .put("success", true)
                                    .put("notificationId", notificationId)
                                    .encode());
                        });
                } else {
                    logger.warn("No FCM token for user {}", userId);
                    ctx.response()
                        .setStatusCode(400)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                            .put("success", false)
                            .put("error", "No FCM token registered")
                            .encode());
                }
            })
            .onFailure(err -> {
                logger.error("Failed to get FCM token for user {}", userId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Internal server error").encode());
            });
    }

    private String sendFCMNotification(String fcmToken, String title, String message,
                                      String type, JsonObject data) {
        // TODO: Implement actual FCM/APNS integration
        logger.info("Sending notification to {}: {} - {}", fcmToken, title, message);

        // Generate notification ID
        String notificationId = UUID.randomUUID().toString();

        // In production, send via FCM API:
        // JsonObject payload = new JsonObject()
        //     .put("to", fcmToken)
        //     .put("notification", new JsonObject()
        //         .put("title", title)
        //         .put("body", message))
        //     .put("data", data.put("type", type));
        // sendFCMRequest(payload);

        return notificationId;
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "notification-service")
                .encode());
    }

    @Override
    public void stop() {
        if (cassandraClient != null) {
            cassandraClient.close();
        }
        if (redisClient != null) {
            redisClient.close();
        }
    }
}
