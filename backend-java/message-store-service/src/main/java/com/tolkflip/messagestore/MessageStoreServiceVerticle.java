package com.tolkflip.messagestore;

import com.tolkflip.shared.database.CassandraClient;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.UUID;

public class MessageStoreServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(MessageStoreServiceVerticle.class);
    private static final int PORT = 3008;

    private CassandraClient cassandraClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/api/messages").handler(this::saveMessage);
        router.get("/api/messages/:threadId").handler(this::getMessages);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Message Store Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void saveMessage(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();

        String threadId = body.getString("threadId");
        String senderId = body.getString("senderId");
        String receiverId = body.getString("receiverId");
        String messageType = body.getString("messageType", "text");
        String content = body.getString("content");
        String originalLanguage = body.getString("originalLanguage", "en");
        boolean isGroup = body.getBoolean("isGroup", false);

        cassandraClient.saveMessage(
            threadId, senderId, receiverId, messageType,
            content, originalLanguage, "sent", isGroup
        )
        .onSuccess(messageId -> {
            ctx.response()
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject()
                    .put("success", true)
                    .put("messageId", messageId)
                    .encode());
        })
        .onFailure(err -> {
            logger.error("Failed to save message", err);
            ctx.response()
                .setStatusCode(500)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Failed to save message").encode());
        });
    }

    private void getMessages(RoutingContext ctx) {
        String threadId = ctx.pathParam("threadId");
        String limitStr = ctx.request().getParam("limit");
        int limit = limitStr != null ? Integer.parseInt(limitStr) : 50;

        cassandraClient.getMessages(threadId, limit)
            .onSuccess(messages -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("messages", messages).encode());
            })
            .onFailure(err -> {
                logger.error("Failed to get messages for thread {}", threadId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to retrieve messages").encode());
            });
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "message-store-service")
                .encode());
    }

    @Override
    public void stop() {
        if (cassandraClient != null) {
            cassandraClient.close();
        }
    }
}
