package com.tolkflip.user;

import com.tolkflip.shared.database.CassandraClient;
import com.tolkflip.shared.database.RedisClient;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

public class UserServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(UserServiceVerticle.class);
    private static final int PORT = 3002;

    private CassandraClient cassandraClient;
    private RedisClient redisClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());
        redisClient = new RedisClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.get("/api/users/:userId").handler(this::getUser);
        router.put("/api/users/:userId/profile").handler(this::updateProfile);
        router.put("/api/users/:userId/languages").handler(this::updateLanguages);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("User Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void getUser(RoutingContext ctx) {
        String userId = ctx.pathParam("userId");

        cassandraClient.getUserById(userId)
            .onSuccess(user -> {
                if (user != null) {
                    ctx.response()
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("user", user).encode());
                } else {
                    ctx.response()
                        .setStatusCode(404)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("error", "User not found").encode());
                }
            })
            .onFailure(err -> {
                logger.error("Failed to get user {}", userId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Internal server error").encode());
            });
    }

    private void updateProfile(RoutingContext ctx) {
        String userId = ctx.pathParam("userId");
        JsonObject body = ctx.body().asJsonObject();

        String displayName = body.getString("display_name");
        String profilePicture = body.getString("profile_picture");
        String bio = body.getString("bio");

        cassandraClient.updateUserProfile(userId, displayName, profilePicture, bio)
            .onSuccess(v -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("success", true).encode());
            })
            .onFailure(err -> {
                logger.error("Failed to update profile for user {}", userId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to update profile").encode());
            });
    }

    private void updateLanguages(RoutingContext ctx) {
        String userId = ctx.pathParam("userId");
        JsonObject body = ctx.body().asJsonObject();

        String nativeLanguage = body.getString("native_language");
        JsonArray learningLanguagesArray = body.getJsonArray("learning_languages", new JsonArray());
        List<String> learningLanguages = new ArrayList<>();
        for (int i = 0; i < learningLanguagesArray.size(); i++) {
            learningLanguages.add(learningLanguagesArray.getString(i));
        }

        cassandraClient.updateUserLanguages(userId, nativeLanguage, learningLanguages)
            .onSuccess(v -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("success", true).encode());
            })
            .onFailure(err -> {
                logger.error("Failed to update languages for user {}", userId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to update languages").encode());
            });
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "user-service")
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
