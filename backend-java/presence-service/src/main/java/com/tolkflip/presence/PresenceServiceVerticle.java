package com.tolkflip.presence;

import com.tolkflip.shared.database.RedisClient;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PresenceServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(PresenceServiceVerticle.class);
    private static final int PORT = 3007;

    private RedisClient redisClient;

    @Override
    public void start(Promise<Void> startPromise) {
        redisClient = new RedisClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.get("/api/presence/:userId").handler(this::getPresence);
        router.post("/api/presence/:userId/status").handler(this::updateStatus);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Presence Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void getPresence(RoutingContext ctx) {
        String userId = ctx.pathParam("userId");

        redisClient.hget("presence", userId)
            .onSuccess(status -> {
                String presenceStatus = (status != null && !status.isEmpty()) ? status : "offline";
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("userId", userId)
                        .put("status", presenceStatus)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to get presence for user {}", userId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to get presence").encode());
            });
    }

    private void updateStatus(RoutingContext ctx) {
        String userId = ctx.pathParam("userId");
        JsonObject body = ctx.body().asJsonObject();
        String status = body.getString("status");

        redisClient.hset("presence", userId, status)
            .compose(v -> redisClient.expire("presence", 900)) // 15 minutes TTL
            .onSuccess(v -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("success", true).encode());
            })
            .onFailure(err -> {
                logger.error("Failed to update presence for user {}", userId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to update presence").encode());
            });
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "presence-service")
                .encode());
    }

    @Override
    public void stop() {
        if (redisClient != null) {
            redisClient.close();
        }
    }
}
