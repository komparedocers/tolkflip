package com.tolkflip.translation;

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

public class TranslationServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(TranslationServiceVerticle.class);
    private static final int PORT = 3004;

    private RedisClient redisClient;

    @Override
    public void start(Promise<Void> startPromise) {
        redisClient = new RedisClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/api/translate").handler(this::translate);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Translation Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void translate(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();

        String text = body.getString("text");
        String sourceLang = body.getString("sourceLang");
        String targetLang = body.getString("targetLang");

        // Generate cache key
        String cacheKey = sourceLang + ":" + targetLang + ":" + text;
        String hashedKey = "translation:" + hashMD5(cacheKey);

        // Check cache first
        redisClient.get(hashedKey)
            .onSuccess(cachedTranslation -> {
                if (cachedTranslation != null && !cachedTranslation.isEmpty()) {
                    // Return cached translation
                    ctx.response()
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject()
                            .put("translatedText", cachedTranslation)
                            .put("sourceLang", sourceLang)
                            .put("targetLang", targetLang)
                            .put("cached", true)
                            .encode());
                } else {
                    // Perform translation (mock implementation - integrate with real API)
                    String translatedText = mockTranslate(text, sourceLang, targetLang);

                    // Cache result (24 hour TTL)
                    redisClient.setex(hashedKey, 86400, translatedText)
                        .onSuccess(v -> {
                            ctx.response()
                                .putHeader("Content-Type", "application/json")
                                .end(new JsonObject()
                                    .put("translatedText", translatedText)
                                    .put("sourceLang", sourceLang)
                                    .put("targetLang", targetLang)
                                    .put("cached", false)
                                    .encode());
                        })
                        .onFailure(err -> {
                            logger.error("Failed to cache translation", err);
                            // Return translation even if caching fails
                            ctx.response()
                                .putHeader("Content-Type", "application/json")
                                .end(new JsonObject()
                                    .put("translatedText", translatedText)
                                    .put("sourceLang", sourceLang)
                                    .put("targetLang", targetLang)
                                    .put("cached", false)
                                    .encode());
                        });
                }
            })
            .onFailure(err -> {
                logger.error("Failed to check translation cache", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Internal server error").encode());
            });
    }

    private String mockTranslate(String text, String sourceLang, String targetLang) {
        // TODO: Integrate with Google Translate API, DeepL, or similar
        return "[Translated to " + targetLang + ": " + text + "]";
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
                .put("service", "translation-service")
                .encode());
    }

    @Override
    public void stop() {
        if (redisClient != null) {
            redisClient.close();
        }
    }
}
