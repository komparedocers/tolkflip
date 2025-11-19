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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.Base64;
import java.util.UUID;

// MinIO imports (requires minio dependency in pom.xml)
// <dependency>
//   <groupId>io.minio</groupId>
//   <artifactId>minio</artifactId>
//   <version>8.5.7</version>
// </dependency>

public class MediaServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(MediaServiceVerticle.class);
    private static final int PORT = 3006;

    private CassandraClient cassandraClient;
    private MinIOClient minioClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());

        // Initialize MinIO client
        String minioEndpoint = config().getString("minio.endpoint", "http://localhost:9000");
        String minioAccessKey = config().getString("minio.access_key", "minioadmin");
        String minioSecretKey = config().getString("minio.secret_key", "minioadmin");
        String minioBucket = config().getString("minio.bucket", "tolkflip-media");

        try {
            minioClient = new MinIOClient(minioEndpoint, minioAccessKey, minioSecretKey, minioBucket);
            logger.info("MinIO client initialized: {}", minioEndpoint);
        } catch (Exception e) {
            logger.error("Failed to initialize MinIO client", e);
        }

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create().setBodyLimit(100 * 1024 * 1024)); // 100MB limit

        // Routes
        router.post("/api/media/upload").handler(this::uploadMedia);
        router.get("/api/media/:mediaId").handler(this::getMedia);
        router.delete("/api/media/:mediaId").handler(this::deleteMedia);
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
        JsonObject body = ctx.body().asJsonObject();
        String fileName = body.getString("fileName", "unknown");
        String contentType = body.getString("contentType", "application/octet-stream");
        String base64Data = body.getString("data");

        if (base64Data == null || base64Data.isEmpty()) {
            ctx.response()
                .setStatusCode(400)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Missing file data").encode());
            return;
        }

        String mediaId = UUID.randomUUID().toString();

        vertx.executeBlocking(promise -> {
            try {
                // Decode base64 data
                byte[] fileBytes = Base64.getDecoder().decode(base64Data);
                long fileSize = fileBytes.length;
                InputStream inputStream = new ByteArrayInputStream(fileBytes);

                // Upload to MinIO
                if (minioClient != null) {
                    String objectName = generateObjectName(mediaId, fileName);
                    minioClient.uploadFile(objectName, inputStream, fileSize, contentType);
                    logger.info("Uploaded {} to MinIO: {}", fileName, objectName);
                }

                promise.complete(new JsonObject()
                    .put("mediaId", mediaId)
                    .put("fileName", fileName)
                    .put("fileSize", fileSize)
                    .put("contentType", contentType));
            } catch (Exception e) {
                logger.error("Failed to upload to MinIO", e);
                promise.fail(e);
            }
        }, false).compose(result -> {
            JsonObject uploadResult = (JsonObject) result;
            // Store metadata in Cassandra
            return cassandraClient.saveMedia(
                uploadResult.getString("mediaId"),
                uploadResult.getString("fileName"),
                uploadResult.getString("contentType"),
                uploadResult.getLong("fileSize")
            ).map(v -> uploadResult);
        }).onSuccess(result -> {
            String mediaUrl = generateMediaUrl((String) result.getValue("mediaId"), (String) result.getValue("fileName"));

            ctx.response()
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject()
                    .put("success", true)
                    .put("mediaId", result.getValue("mediaId"))
                    .put("url", mediaUrl)
                    .put("fileName", result.getValue("fileName"))
                    .put("fileSize", result.getValue("fileSize"))
                    .encode());
        }).onFailure(err -> {
            logger.error("Failed to upload media", err);
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
                    String fileName = mediaInfo.getString("file_name");
                    String mediaUrl = generateMediaUrl(mediaId, fileName);

                    // Option 1: Return metadata with URL (client downloads directly from MinIO)
                    ctx.response()
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                            .put("mediaId", mediaId)
                            .put("url", mediaUrl)
                            .put("fileName", fileName)
                            .put("contentType", mediaInfo.getString("content_type"))
                            .put("fileSize", mediaInfo.getLong("file_size"))
                            .encode());

                    // Option 2: Proxy file from MinIO (uncomment below to enable)
                    // proxyMediaFromMinIO(ctx, mediaId, fileName, mediaInfo.getString("content_type"));
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

    private void deleteMedia(RoutingContext ctx) {
        String mediaId = ctx.pathParam("mediaId");

        cassandraClient.getMedia(mediaId)
            .compose(mediaInfo -> {
                if (mediaInfo == null) {
                    return io.vertx.core.Future.failedFuture("Media not found");
                }

                // Delete from MinIO
                return vertx.executeBlocking(promise -> {
                    try {
                        if (minioClient != null) {
                            String objectName = generateObjectName(mediaId, mediaInfo.getString("file_name"));
                            minioClient.deleteFile(objectName);
                            logger.info("Deleted {} from MinIO", objectName);
                        }
                        promise.complete(mediaInfo);
                    } catch (Exception e) {
                        logger.error("Failed to delete from MinIO", e);
                        promise.fail(e);
                    }
                }, false);
            })
            .compose(v -> cassandraClient.deleteMedia(mediaId))
            .onSuccess(v -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("message", "Media deleted")
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to delete media {}", mediaId, err);
                int statusCode = err.getMessage().equals("Media not found") ? 404 : 500;
                ctx.response()
                    .setStatusCode(statusCode)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", err.getMessage()).encode());
            });
    }

    private void proxyMediaFromMinIO(RoutingContext ctx, String mediaId, String fileName, String contentType) {
        vertx.executeBlocking(promise -> {
            try {
                if (minioClient != null) {
                    String objectName = generateObjectName(mediaId, fileName);
                    InputStream stream = minioClient.downloadFile(objectName);
                    byte[] fileBytes = stream.readAllBytes();
                    promise.complete(fileBytes);
                } else {
                    promise.fail("MinIO client not available");
                }
            } catch (Exception e) {
                logger.error("Failed to download from MinIO", e);
                promise.fail(e);
            }
        }, false).onSuccess(fileBytes -> {
            ctx.response()
                .putHeader("Content-Type", contentType)
                .putHeader("Content-Disposition", "attachment; filename=\"" + fileName + "\"")
                .end(io.vertx.core.buffer.Buffer.buffer((byte[]) fileBytes));
        }).onFailure(err -> {
            ctx.response()
                .setStatusCode(500)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Failed to download media").encode());
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

    private String generateObjectName(String mediaId, String fileName) {
        String extension = "";
        int dotIndex = fileName.lastIndexOf('.');
        if (dotIndex > 0) {
            extension = fileName.substring(dotIndex);
        }
        return "media/" + mediaId + extension;
    }

    private String generateMediaUrl(String mediaId, String fileName) {
        String minioEndpoint = config().getString("minio.endpoint", "http://localhost:9000");
        String bucket = config().getString("minio.bucket", "tolkflip-media");
        String objectName = generateObjectName(mediaId, fileName);
        return minioEndpoint + "/" + bucket + "/" + objectName;
    }

    @Override
    public void stop() {
        if (cassandraClient != null) {
            cassandraClient.close();
        }
        if (minioClient != null) {
            minioClient.close();
        }
    }

    /**
     * MinIO Client Wrapper
     * Handles file upload, download, and deletion operations
     */
    private static class MinIOClient {
        private final Logger logger = LoggerFactory.getLogger(MinIOClient.class);
        private final Object client; // io.minio.MinioClient (using Object to avoid compile error)
        private final String bucketName;

        public MinIOClient(String endpoint, String accessKey, String secretKey, String bucketName) throws Exception {
            this.bucketName = bucketName;

            try {
                // Use reflection to create MinioClient (avoids compile-time dependency)
                Class<?> minioClientClass = Class.forName("io.minio.MinioClient");
                Class<?> builderClass = Class.forName("io.minio.MinioClient$Builder");

                Object builder = minioClientClass.getMethod("builder").invoke(null);
                builder = builderClass.getMethod("endpoint", String.class).invoke(builder, endpoint);
                builder = builderClass.getMethod("credentials", String.class, String.class)
                    .invoke(builder, accessKey, secretKey);
                this.client = builderClass.getMethod("build").invoke(builder);

                // Create bucket if not exists
                createBucketIfNotExists();

                logger.info("MinIO client initialized for bucket: {}", bucketName);
            } catch (ClassNotFoundException e) {
                logger.warn("MinIO library not found. Add dependency: io.minio:minio:8.5.7");
                throw e;
            }
        }

        private void createBucketIfNotExists() throws Exception {
            Class<?> bucketExistsArgsClass = Class.forName("io.minio.BucketExistsArgs");
            Class<?> makeBucketArgsClass = Class.forName("io.minio.MakeBucketArgs");

            // Build BucketExistsArgs
            Object bucketExistsBuilder = bucketExistsArgsClass.getMethod("builder").invoke(null);
            bucketExistsBuilder = bucketExistsBuilder.getClass()
                .getMethod("bucket", String.class).invoke(bucketExistsBuilder, bucketName);
            Object bucketExistsArgs = bucketExistsBuilder.getClass().getMethod("build").invoke(bucketExistsBuilder);

            // Check if bucket exists
            Boolean exists = (Boolean) client.getClass()
                .getMethod("bucketExists", bucketExistsArgsClass)
                .invoke(client, bucketExistsArgs);

            if (!exists) {
                // Build MakeBucketArgs
                Object makeBucketBuilder = makeBucketArgsClass.getMethod("builder").invoke(null);
                makeBucketBuilder = makeBucketBuilder.getClass()
                    .getMethod("bucket", String.class).invoke(makeBucketBuilder, bucketName);
                Object makeBucketArgs = makeBucketBuilder.getClass().getMethod("build").invoke(makeBucketBuilder);

                // Create bucket
                client.getClass().getMethod("makeBucket", makeBucketArgsClass).invoke(client, makeBucketArgs);
                logger.info("Created MinIO bucket: {}", bucketName);
            }
        }

        public void uploadFile(String objectName, InputStream stream, long size, String contentType) throws Exception {
            Class<?> putObjectArgsClass = Class.forName("io.minio.PutObjectArgs");

            Object builder = putObjectArgsClass.getMethod("builder").invoke(null);
            builder = builder.getClass().getMethod("bucket", String.class).invoke(builder, bucketName);
            builder = builder.getClass().getMethod("object", String.class).invoke(builder, objectName);
            builder = builder.getClass().getMethod("stream", InputStream.class, long.class, long.class)
                .invoke(builder, stream, size, -1L);
            builder = builder.getClass().getMethod("contentType", String.class).invoke(builder, contentType);

            Object putObjectArgs = builder.getClass().getMethod("build").invoke(builder);
            client.getClass().getMethod("putObject", putObjectArgsClass).invoke(client, putObjectArgs);
        }

        public InputStream downloadFile(String objectName) throws Exception {
            Class<?> getObjectArgsClass = Class.forName("io.minio.GetObjectArgs");

            Object builder = getObjectArgsClass.getMethod("builder").invoke(null);
            builder = builder.getClass().getMethod("bucket", String.class).invoke(builder, bucketName);
            builder = builder.getClass().getMethod("object", String.class).invoke(builder, objectName);

            Object getObjectArgs = builder.getClass().getMethod("build").invoke(builder);
            return (InputStream) client.getClass()
                .getMethod("getObject", getObjectArgsClass)
                .invoke(client, getObjectArgs);
        }

        public void deleteFile(String objectName) throws Exception {
            Class<?> removeObjectArgsClass = Class.forName("io.minio.RemoveObjectArgs");

            Object builder = removeObjectArgsClass.getMethod("builder").invoke(null);
            builder = builder.getClass().getMethod("bucket", String.class).invoke(builder, bucketName);
            builder = builder.getClass().getMethod("object", String.class).invoke(builder, objectName);

            Object removeObjectArgs = builder.getClass().getMethod("build").invoke(builder);
            client.getClass().getMethod("removeObject", removeObjectArgsClass).invoke(client, removeObjectArgs);
        }

        public void close() {
            // MinioClient doesn't require explicit close
            logger.info("MinIO client closed");
        }
    }
}
