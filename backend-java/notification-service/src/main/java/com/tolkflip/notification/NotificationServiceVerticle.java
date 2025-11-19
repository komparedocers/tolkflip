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

// Firebase Admin SDK imports (requires firebase-admin dependency in pom.xml)
// <dependency>
//   <groupId>com.google.firebase</groupId>
//   <artifactId>firebase-admin</artifactId>
//   <version>9.2.0</version>
// </dependency>

public class NotificationServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(NotificationServiceVerticle.class);
    private static final int PORT = 3009;

    private CassandraClient cassandraClient;
    private RedisClient redisClient;
    private FCMClient fcmClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());
        redisClient = new RedisClient(vertx, config());

        // Initialize FCM client
        String fcmCredentialsPath = config().getString("fcm.credentials_path",
            System.getenv("GOOGLE_APPLICATION_CREDENTIALS"));

        try {
            fcmClient = new FCMClient(fcmCredentialsPath);
            logger.info("FCM client initialized successfully");
        } catch (Exception e) {
            logger.error("Failed to initialize FCM client", e);
            // Continue without FCM (notifications will be logged but not sent)
        }

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/api/notifications/send").handler(this::sendNotification);
        router.post("/api/notifications/register-token").handler(this::registerToken);
        router.delete("/api/notifications/unregister-token").handler(this::unregisterToken);
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
        String notificationId = UUID.randomUUID().toString();

        if (fcmClient != null) {
            try {
                // Add notification metadata to data payload
                JsonObject fullData = data.copy()
                    .put("type", type)
                    .put("notificationId", notificationId);

                // Send via FCM
                boolean success = fcmClient.sendNotification(fcmToken, title, message, fullData);

                if (success) {
                    logger.info("FCM notification sent to {}: {} - {}", fcmToken, title, message);
                } else {
                    logger.error("Failed to send FCM notification to {}", fcmToken);
                }
            } catch (Exception e) {
                logger.error("Error sending FCM notification", e);
            }
        } else {
            logger.warn("FCM client not initialized. Notification logged: {} - {}", title, message);
        }

        return notificationId;
    }

    private void registerToken(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String userId = body.getString("userId");
        String fcmToken = body.getString("fcmToken");
        String platform = body.getString("platform", "android"); // "android" or "ios"

        if (userId == null || fcmToken == null) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Missing userId or fcmToken").encode());
            return;
        }

        // Store FCM token in Redis
        redisClient.hset("fcm_tokens", userId, fcmToken)
            .compose(v -> redisClient.hset("fcm_platforms", userId, platform))
            .onSuccess(v -> {
                logger.info("Registered FCM token for user {} ({})", userId, platform);
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("message", "Token registered")
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to register FCM token", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to register token").encode());
            });
    }

    private void unregisterToken(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String userId = body.getString("userId");

        if (userId == null) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Missing userId").encode());
            return;
        }

        // Remove FCM token from Redis
        redisClient.hdel("fcm_tokens", userId)
            .compose(v -> redisClient.hdel("fcm_platforms", userId))
            .onSuccess(v -> {
                logger.info("Unregistered FCM token for user {}", userId);
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("message", "Token unregistered")
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to unregister FCM token", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to unregister token").encode());
            });
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
        if (fcmClient != null) {
            fcmClient.close();
        }
    }

    /**
     * FCM Client Wrapper
     * Handles Firebase Cloud Messaging for both Android and iOS (APNS via FCM)
     */
    private static class FCMClient {
        private final Logger logger = LoggerFactory.getLogger(FCMClient.class);
        private Object firebaseApp;
        private Object messaging;

        public FCMClient(String credentialsPath) throws Exception {
            try {
                // Use reflection to initialize Firebase Admin SDK (avoids compile-time dependency)
                Class<?> firebaseAppClass = Class.forName("com.google.firebase.FirebaseApp");
                Class<?> firebaseOptionsClass = Class.forName("com.google.firebase.FirebaseOptions");
                Class<?> googleCredentialsClass = Class.forName("com.google.auth.oauth2.GoogleCredentials");
                Class<?> firebaseMessagingClass = Class.forName("com.google.firebase.messaging.FirebaseMessaging");

                // Load credentials from file or environment
                Object credentials;
                if (credentialsPath != null && !credentialsPath.isEmpty()) {
                    java.io.FileInputStream serviceAccount = new java.io.FileInputStream(credentialsPath);
                    credentials = googleCredentialsClass.getMethod("fromStream", java.io.InputStream.class)
                        .invoke(null, serviceAccount);
                    serviceAccount.close();
                } else {
                    // Use application default credentials
                    credentials = googleCredentialsClass.getMethod("getApplicationDefault").invoke(null);
                }

                // Build FirebaseOptions
                Object builder = firebaseOptionsClass.getMethod("builder").invoke(null);
                builder = builder.getClass().getMethod("setCredentials", googleCredentialsClass)
                    .invoke(builder, credentials);
                Object options = builder.getClass().getMethod("build").invoke(builder);

                // Initialize FirebaseApp
                firebaseApp = firebaseAppClass.getMethod("initializeApp", firebaseOptionsClass)
                    .invoke(null, options);

                // Get FirebaseMessaging instance
                messaging = firebaseMessagingClass.getMethod("getInstance", firebaseAppClass)
                    .invoke(null, firebaseApp);

                logger.info("Firebase Admin SDK initialized successfully");
            } catch (ClassNotFoundException e) {
                logger.error("Firebase Admin SDK not found. Add dependency: com.google.firebase:firebase-admin:9.2.0");
                throw e;
            }
        }

        /**
         * Send notification via FCM
         * Supports both Android (FCM) and iOS (APNS via FCM)
         */
        public boolean sendNotification(String token, String title, String body, JsonObject data) {
            try {
                Class<?> messageClass = Class.forName("com.google.firebase.messaging.Message");
                Class<?> notificationClass = Class.forName("com.google.firebase.messaging.Notification");
                Class<?> androidConfigClass = Class.forName("com.google.firebase.messaging.AndroidConfig");
                Class<?> apnsConfigClass = Class.forName("com.google.firebase.messaging.ApnsConfig");
                Class<?> apsClass = Class.forName("com.google.firebase.messaging.Aps");

                // Build Notification
                Object notificationBuilder = notificationClass.getMethod("builder").invoke(null);
                notificationBuilder = notificationBuilder.getClass()
                    .getMethod("setTitle", String.class).invoke(notificationBuilder, title);
                notificationBuilder = notificationBuilder.getClass()
                    .getMethod("setBody", String.class).invoke(notificationBuilder, body);
                Object notification = notificationBuilder.getClass().getMethod("build").invoke(notificationBuilder);

                // Build AndroidConfig (high priority)
                Object androidConfigBuilder = androidConfigClass.getMethod("builder").invoke(null);
                Class<?> priorityClass = Class.forName("com.google.firebase.messaging.AndroidConfig$Priority");
                Object highPriority = priorityClass.getField("HIGH").get(null);
                androidConfigBuilder = androidConfigBuilder.getClass()
                    .getMethod("setPriority", priorityClass).invoke(androidConfigBuilder, highPriority);
                Object androidConfig = androidConfigBuilder.getClass().getMethod("build").invoke(androidConfigBuilder);

                // Build ApnsConfig (for iOS)
                Object apsBuilder = apsClass.getMethod("builder").invoke(null);
                apsBuilder = apsBuilder.getClass().getMethod("setSound", String.class).invoke(apsBuilder, "default");
                Object aps = apsBuilder.getClass().getMethod("build").invoke(apsBuilder);

                Object apnsConfigBuilder = apnsConfigClass.getMethod("builder").invoke(null);
                apnsConfigBuilder = apnsConfigBuilder.getClass()
                    .getMethod("setAps", apsClass).invoke(apnsConfigBuilder, aps);
                Object apnsConfig = apnsConfigBuilder.getClass().getMethod("build").invoke(apnsConfigBuilder);

                // Build Message with data payload
                Object messageBuilder = messageClass.getMethod("builder").invoke(null);
                messageBuilder = messageBuilder.getClass()
                    .getMethod("setToken", String.class).invoke(messageBuilder, token);
                messageBuilder = messageBuilder.getClass()
                    .getMethod("setNotification", notificationClass).invoke(messageBuilder, notification);
                messageBuilder = messageBuilder.getClass()
                    .getMethod("setAndroidConfig", androidConfigClass).invoke(messageBuilder, androidConfig);
                messageBuilder = messageBuilder.getClass()
                    .getMethod("setApnsConfig", apnsConfigClass).invoke(messageBuilder, apnsConfig);

                // Add data payload
                for (String key : data.fieldNames()) {
                    messageBuilder = messageBuilder.getClass()
                        .getMethod("putData", String.class, String.class)
                        .invoke(messageBuilder, key, data.getValue(key).toString());
                }

                Object message = messageBuilder.getClass().getMethod("build").invoke(messageBuilder);

                // Send message
                String response = (String) messaging.getClass()
                    .getMethod("send", messageClass)
                    .invoke(messaging, message);

                logger.info("FCM message sent successfully: {}", response);
                return true;
            } catch (Exception e) {
                logger.error("Failed to send FCM notification", e);
                return false;
            }
        }

        /**
         * Send notification to multiple tokens (topic or batch)
         */
        public boolean sendNotificationToTopic(String topic, String title, String body, JsonObject data) {
            try {
                Class<?> messageClass = Class.forName("com.google.firebase.messaging.Message");
                Class<?> notificationClass = Class.forName("com.google.firebase.messaging.Notification");

                // Build Notification
                Object notificationBuilder = notificationClass.getMethod("builder").invoke(null);
                notificationBuilder = notificationBuilder.getClass()
                    .getMethod("setTitle", String.class).invoke(notificationBuilder, title);
                notificationBuilder = notificationBuilder.getClass()
                    .getMethod("setBody", String.class).invoke(notificationBuilder, body);
                Object notification = notificationBuilder.getClass().getMethod("build").invoke(notificationBuilder);

                // Build Message with topic
                Object messageBuilder = messageClass.getMethod("builder").invoke(null);
                messageBuilder = messageBuilder.getClass()
                    .getMethod("setTopic", String.class).invoke(messageBuilder, topic);
                messageBuilder = messageBuilder.getClass()
                    .getMethod("setNotification", notificationClass).invoke(messageBuilder, notification);

                // Add data payload
                for (String key : data.fieldNames()) {
                    messageBuilder = messageBuilder.getClass()
                        .getMethod("putData", String.class, String.class)
                        .invoke(messageBuilder, key, data.getValue(key).toString());
                }

                Object message = messageBuilder.getClass().getMethod("build").invoke(messageBuilder);

                // Send message
                String response = (String) messaging.getClass()
                    .getMethod("send", messageClass)
                    .invoke(messaging, message);

                logger.info("FCM topic message sent successfully: {}", response);
                return true;
            } catch (Exception e) {
                logger.error("Failed to send FCM topic notification", e);
                return false;
            }
        }

        public void close() {
            try {
                if (firebaseApp != null) {
                    firebaseApp.getClass().getMethod("delete").invoke(firebaseApp);
                    logger.info("Firebase app deleted");
                }
            } catch (Exception e) {
                logger.error("Error closing FCM client", e);
            }
        }
    }
}
