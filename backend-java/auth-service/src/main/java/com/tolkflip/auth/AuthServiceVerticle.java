package com.tolkflip.auth;

import com.datastax.oss.driver.api.core.cql.Row;
import com.tolkflip.shared.database.CassandraClient;
import com.tolkflip.shared.database.RedisClient;
import com.tolkflip.shared.util.JwtUtil;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpServer;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import io.vertx.ext.web.handler.CorsHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.security.SecureRandom;
import java.util.List;
import java.util.UUID;

/**
 * High-performance Auth Service using Vert.x
 * Handles phone verification, user registration, and JWT token generation
 */
public class AuthServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(AuthServiceVerticle.class);

    private CassandraClient cassandraClient;
    private RedisClient redisClient;
    private JwtUtil jwtUtil;
    private SecureRandom secureRandom;

    @Override
    public void start(Promise<Void> startPromise) {
        // Get configuration
        int port = config().getInteger("port", 3001);
        String cassandraHosts = config().getString("cassandra.hosts", "localhost");
        String redisHost = config().getString("redis.host", "localhost");
        int redisPort = config().getInteger("redis.port", 6379);
        String jwtSecret = config().getString("jwt.secret", "your-jwt-secret-change-in-production");

        // Initialize clients
        cassandraClient = new CassandraClient(vertx, List.of(cassandraHosts.split(",")), "tolkflip");
        redisClient = new RedisClient(vertx, redisHost, redisPort);
        jwtUtil = new JwtUtil(vertx, jwtSecret);
        secureRandom = new SecureRandom();

        // Create router
        Router router = Router.router(vertx);

        // CORS handler
        router.route().handler(CorsHandler.create("*")
            .allowedMethod(io.vertx.core.http.HttpMethod.GET)
            .allowedMethod(io.vertx.core.http.HttpMethod.POST)
            .allowedMethod(io.vertx.core.http.HttpMethod.PUT)
            .allowedMethod(io.vertx.core.http.HttpMethod.DELETE)
            .allowedHeader("Content-Type")
            .allowedHeader("Authorization"));

        // Body handler
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/request-code").handler(this::handleRequestCode);
        router.post("/verify-code").handler(this::handleVerifyCode);
        router.post("/register").handler(this::handleRegister);
        router.post("/login").handler(this::handleLogin);
        router.post("/refresh").handler(this::handleRefresh);
        router.get("/health").handler(this::handleHealth);

        // Create HTTP server
        HttpServer server = vertx.createHttpServer();

        server.requestHandler(router)
            .listen(port)
            .onSuccess(http -> {
                logger.info("Auth Service started on port {}", port);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    /**
     * Request verification code
     */
    private void handleRequestCode(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String phoneNumber = body.getString("phone_number");

        if (phoneNumber == null || phoneNumber.isEmpty()) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Phone number is required").encode());
            return;
        }

        // Generate 6-digit verification code
        String code = String.format("%06d", secureRandom.nextInt(1000000));

        // Store in Redis with 10-minute expiry
        String redisKey = "verification:" + phoneNumber;
        JsonObject verificationData = new JsonObject()
            .put("code", code)
            .put("attempts", 0);

        redisClient.setex(redisKey, 600, verificationData.encode())
            .onSuccess(v -> {
                logger.info("Verification code sent to {}", phoneNumber);

                // TODO: Send SMS via Twilio (placeholder)
                logger.info("CODE for {}: {}", phoneNumber, code);

                ctx.response()
                    .setStatusCode(200)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("message", "Verification code sent")
                        .put("expiresIn", 600)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to store verification code", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to send verification code").encode());
            });
    }

    /**
     * Verify code
     */
    private void handleVerifyCode(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String phoneNumber = body.getString("phone_number");
        String code = body.getString("code");

        if (phoneNumber == null || code == null) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Phone number and code are required").encode());
            return;
        }

        String redisKey = "verification:" + phoneNumber;

        redisClient.get(redisKey)
            .compose(data -> {
                if (data == null) {
                    return vertx.future().failedFuture("Verification code expired or not found");
                }

                JsonObject verificationData = new JsonObject(data);
                String storedCode = verificationData.getString("code");
                int attempts = verificationData.getInteger("attempts", 0);

                if (attempts >= 3) {
                    return vertx.future().failedFuture("Too many attempts");
                }

                if (!code.equals(storedCode)) {
                    verificationData.put("attempts", attempts + 1);
                    return redisClient.setex(redisKey, 600, verificationData.encode())
                        .compose(v -> vertx.future().failedFuture("Invalid verification code"));
                }

                // Code is valid, delete from Redis
                return redisClient.del(redisKey)
                    .map(v -> true);
            })
            .onSuccess(v -> {
                logger.info("Verification successful for {}", phoneNumber);
                ctx.response()
                    .setStatusCode(200)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("message", "Verification successful")
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Verification failed for {}: {}", phoneNumber, err.getMessage());
                ctx.response()
                    .setStatusCode(400)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", err.getMessage()).encode());
            });
    }

    /**
     * Register new user
     */
    private void handleRegister(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String phoneNumber = body.getString("phone_number");
        String displayName = body.getString("display_name");
        String primaryLanguage = body.getString("primary_language", "en");

        if (phoneNumber == null || displayName == null) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Phone number and display name are required").encode());
            return;
        }

        // Check if user already exists
        cassandraClient.getUserByPhone(phoneNumber)
            .compose(existingUser -> {
                if (existingUser != null) {
                    return vertx.future().failedFuture("User already exists");
                }

                // Create new user
                UUID userId = UUID.randomUUID();
                return cassandraClient.createUser(userId, phoneNumber, displayName,
                                                 primaryLanguage, List.of())
                    .map(v -> userId);
            })
            .onSuccess(userId -> {
                // Generate tokens
                String accessToken = jwtUtil.generateAccessToken(userId.toString(), phoneNumber);
                String refreshToken = jwtUtil.generateRefreshToken(userId.toString());

                logger.info("User registered: {}", userId);

                ctx.response()
                    .setStatusCode(201)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("userId", userId.toString())
                        .put("accessToken", accessToken)
                        .put("refreshToken", refreshToken)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Registration failed", err);
                ctx.response()
                    .setStatusCode(400)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", err.getMessage()).encode());
            });
    }

    /**
     * Login existing user
     */
    private void handleLogin(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String phoneNumber = body.getString("phone_number");

        if (phoneNumber == null) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Phone number is required").encode());
            return;
        }

        cassandraClient.getUserByPhone(phoneNumber)
            .onSuccess(user -> {
                if (user == null) {
                    ctx.response()
                        .setStatusCode(404)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("error", "User not found").encode());
                    return;
                }

                UUID userId = user.getUuid("user_id");

                // Generate tokens
                String accessToken = jwtUtil.generateAccessToken(userId.toString(), phoneNumber);
                String refreshToken = jwtUtil.generateRefreshToken(userId.toString());

                logger.info("User logged in: {}", userId);

                ctx.response()
                    .setStatusCode(200)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("userId", userId.toString())
                        .put("accessToken", accessToken)
                        .put("refreshToken", refreshToken)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Login failed", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Login failed").encode());
            });
    }

    /**
     * Refresh access token
     */
    private void handleRefresh(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();
        String refreshToken = body.getString("refreshToken");

        if (refreshToken == null) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Refresh token is required").encode());
            return;
        }

        // Validate refresh token
        JsonObject claims = jwtUtil.validateToken(refreshToken);

        if (claims == null || !"refresh".equals(claims.getString("type"))) {
            ctx.response()
                .setStatusCode(401)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Invalid refresh token").encode());
            return;
        }

        String userId = claims.getString("userId");

        // Get user to get phone number
        cassandraClient.getUserById(UUID.fromString(userId))
            .onSuccess(user -> {
                if (user == null) {
                    ctx.response()
                        .setStatusCode(404)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("error", "User not found").encode());
                    return;
                }

                String phoneNumber = user.getString("phone_number");

                // Generate new access token
                String accessToken = jwtUtil.generateAccessToken(userId, phoneNumber);

                ctx.response()
                    .setStatusCode(200)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("accessToken", accessToken)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Token refresh failed", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Token refresh failed").encode());
            });
    }

    /**
     * Health check
     */
    private void handleHealth(RoutingContext ctx) {
        ctx.response()
            .setStatusCode(200)
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "auth-service")
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
