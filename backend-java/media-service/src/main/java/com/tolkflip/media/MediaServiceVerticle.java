package com.tolkflip.media;

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

public class MediaServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(MediaServiceVerticle.class);
    private static final int PORT = 3006;

    private CassandraClient cassandraClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create().setBodyLimit(100 * 1024 * 1024)); // 100MB limit

        // Routes
        router.post("/api/media/upload").handler(this::uploadMedia);
        router.get("/api/media/:mediaId").handler(this::getMedia);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Media Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void uploadMedia(RoutingContext ctx) {
        // In production, handle multipart file upload
        // For now, expect JSON with base64 or URL

        JsonObject body = ctx.body().asJsonObject();
        String fileName = body.getString("fileName", "unknown");
        String contentType = body.getString("contentType", "application/octet-stream");
        long fileSize = body.getLong("fileSize", 0L);

        String mediaId = UUID.randomUUID().toString();

        // Store metadata in Cassandra
        cassandraClient.saveMedia(mediaId, fileName, contentType, fileSize)
            .onSuccess(v -> {
                // In production, upload to MinIO/S3
                String mediaUrl = "https://storage.tolkflip.com/" + mediaId;

                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("mediaId", mediaId)
                        .put("url", mediaUrl)
                        .put("fileName", fileName)
                        .put("fileSize", fileSize)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to save media metadata", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to upload media").encode());
            });
    }

    private void getMedia(RoutingContext ctx) {
        String mediaId = ctx.pathParam("mediaId");

        cassandraClient.getMedia(mediaId)
            .onSuccess(mediaInfo -> {
                if (mediaInfo != null) {
                    // In production, fetch from MinIO/S3
                    String mediaUrl = "https://storage.tolkflip.com/" + mediaId;

                    ctx.response()
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                            .put("mediaId", mediaId)
                            .put("url", mediaUrl)
                            .put("fileName", mediaInfo.getString("file_name"))
                            .put("contentType", mediaInfo.getString("content_type"))
                            .put("fileSize", mediaInfo.getLong("file_size"))
                            .encode());
                } else {
                    ctx.response()
                        .setStatusCode(404)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("error", "Media not found").encode());
                }
            })
            .onFailure(err -> {
                logger.error("Failed to get media {}", mediaId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Internal server error").encode());
            });
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "media-service")
                .encode());
    }

    @Override
    public void stop() {
        if (cassandraClient != null) {
            cassandraClient.close();
        }
    }
}
