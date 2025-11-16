package com.tolkflip.transcription;

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

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;

public class TranscriptionServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(TranscriptionServiceVerticle.class);
    private static final int PORT = 3005;

    private CassandraClient cassandraClient;
    private RedisClient redisClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());
        redisClient = new RedisClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/api/transcribe").handler(this::transcribe);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Transcription Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void transcribe(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();

        String audioUrl = body.getString("audioUrl");
        String language = body.getString("language", "en");

        // Generate cache key
        String cacheKey = "transcription:" + hashMD5(audioUrl);

        // Check cache first
        redisClient.get(cacheKey)
            .onSuccess(cachedTranscription -> {
                if (cachedTranscription != null && !cachedTranscription.isEmpty()) {
                    // Return cached transcription
                    ctx.response()
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                            .put("text", cachedTranscription)
                            .put("language", language)
                            .put("cached", true)
                            .encode());
                } else {
                    // Perform transcription (mock implementation)
                    String transcribedText = mockTranscribe(audioUrl, language);

                    // Cache result (24 hour TTL)
                    redisClient.setex(cacheKey, 86400, transcribedText)
                        .compose(v -> {
                            // Save to database
                            return cassandraClient.saveTranscription(audioUrl, transcribedText, language);
                        })
                        .onSuccess(transcriptionId -> {
                            ctx.response()
                                .putHeader("Content-Type", "application/json")
                                .end(new JsonObject()
                                    .put("id", transcriptionId)
                                    .put("text", transcribedText)
                                    .put("language", language)
                                    .put("cached", false)
                                    .encode());
                        })
                        .onFailure(err -> {
                            logger.error("Failed to cache/save transcription", err);
                            // Return transcription even if caching/saving fails
                            ctx.response()
                                .putHeader("Content-Type", "application/json")
                                .end(new JsonObject()
                                    .put("text", transcribedText)
                                    .put("language", language)
                                    .put("cached", false)
                                    .encode());
                        });
                }
            })
            .onFailure(err -> {
                logger.error("Failed to check transcription cache", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Internal server error").encode());
            });
    }

    private String mockTranscribe(String audioUrl, String language) {
        // TODO: Integrate with Google Speech-to-Text, Whisper, or similar
        return "This is a transcribed audio message";
    }

    private String hashMD5(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] hashBytes = md.digest(input.getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (byte b : hashBytes) {
                sb.append(String.format("%02x", b));
            }
            return sb.toString();
        } catch (Exception e) {
            logger.error("Failed to hash cache key", e);
            return input.hashCode() + "";
        }
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "transcription-service")
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
