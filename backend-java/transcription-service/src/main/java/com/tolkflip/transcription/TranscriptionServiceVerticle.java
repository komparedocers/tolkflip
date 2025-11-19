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
        // Production Transcription Implementation
        String transcriptionMode = config().getString("transcription.mode", "assemblyai");

        try {
            if ("google".equals(transcriptionMode)) {
                return transcribeWithGoogle(audioUrl, language);
            } else if ("whisper".equals(transcriptionMode)) {
                return transcribeWithWhisper(audioUrl, language);
            } else {
                return transcribeWithAssemblyAI(audioUrl, language);
            }
        } catch (Exception e) {
            logger.error("Transcription failed", e);
            return "[Transcription failed]";
        }
    }

    /**
     * Transcribe using Google Cloud Speech-to-Text API
     * Requires: GOOGLE_APPLICATION_CREDENTIALS environment variable
     */
    private String transcribeWithGoogle(String audioUrl, String language) {
        String apiKey = config().getString("google.api_key", System.getenv("GOOGLE_API_KEY"));
        String apiUrl = "https://speech.googleapis.com/v1/speech:recognize?key=" + apiKey;

        JsonObject requestBody = new JsonObject()
            .put("config", new JsonObject()
                .put("encoding", "LINEAR16")
                .put("sampleRateHertz", 16000)
                .put("languageCode", mapLanguageCode(language))
                .put("enableAutomaticPunctuation", true))
            .put("audio", new JsonObject()
                .put("uri", audioUrl));

        try {
            String response = performHttpPost(apiUrl, requestBody);
            JsonObject result = new JsonObject(response);

            if (result.containsKey("results") && result.getJsonArray("results").size() > 0) {
                return result.getJsonArray("results")
                    .getJsonObject(0)
                    .getJsonArray("alternatives")
                    .getJsonObject(0)
                    .getString("transcript");
            }

            return "[No transcription available]";
        } catch (Exception e) {
            logger.error("Google Speech-to-Text failed", e);
            throw new RuntimeException("Google transcription failed", e);
        }
    }

    /**
     * Transcribe using OpenAI Whisper API
     * Requires: OPENAI_API_KEY environment variable
     */
    private String transcribeWithWhisper(String audioUrl, String language) {
        String apiKey = config().getString("openai.api_key", System.getenv("OPENAI_API_KEY"));
        String apiUrl = "https://api.openai.com/v1/audio/transcriptions";

        // Note: Whisper API requires multipart/form-data with audio file
        // For production, download audio file first, then upload to Whisper

        try {
            // Simplified implementation - in production, download audio file first
            JsonObject requestBody = new JsonObject()
                .put("model", "whisper-1")
                .put("file", audioUrl)
                .put("language", language);

            String response = performHttpPostWithAuth(apiUrl, requestBody, "Bearer " + apiKey);
            JsonObject result = new JsonObject(response);

            return result.getString("text", "[No transcription available]");
        } catch (Exception e) {
            logger.error("OpenAI Whisper failed", e);
            throw new RuntimeException("Whisper transcription failed", e);
        }
    }

    /**
     * Transcribe using AssemblyAI API
     * Requires: ASSEMBLYAI_API_KEY environment variable
     */
    private String transcribeWithAssemblyAI(String audioUrl, String language) {
        String apiKey = config().getString("assemblyai.api_key", System.getenv("ASSEMBLYAI_API_KEY"));

        // Step 1: Submit transcription job
        String submitUrl = "https://api.assemblyai.com/v2/transcript";

        JsonObject requestBody = new JsonObject()
            .put("audio_url", audioUrl)
            .put("language_code", mapLanguageCode(language))
            .put("punctuate", true)
            .put("format_text", true);

        try {
            String submitResponse = performHttpPostWithAuth(submitUrl, requestBody, apiKey);
            JsonObject submitResult = new JsonObject(submitResponse);
            String transcriptId = submitResult.getString("id");

            // Step 2: Poll for completion (simplified - in production use webhooks)
            String pollUrl = "https://api.assemblyai.com/v2/transcript/" + transcriptId;

            for (int i = 0; i < 30; i++) { // Poll for up to 30 seconds
                try {
                    Thread.sleep(1000); // Wait 1 second between polls
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }

                String pollResponse = performHttpGetWithAuth(pollUrl, apiKey);
                JsonObject pollResult = new JsonObject(pollResponse);
                String status = pollResult.getString("status");

                if ("completed".equals(status)) {
                    return pollResult.getString("text", "[No transcription available]");
                } else if ("error".equals(status)) {
                    logger.error("AssemblyAI transcription error: {}", pollResult.getString("error"));
                    return "[Transcription failed]";
                }
            }

            return "[Transcription timeout]";
        } catch (Exception e) {
            logger.error("AssemblyAI failed", e);
            throw new RuntimeException("AssemblyAI transcription failed", e);
        }
    }

    /**
     * Map language code to provider-specific format
     */
    private String mapLanguageCode(String language) {
        switch (language) {
            case "en": return "en-US";
            case "es": return "es-ES";
            case "fr": return "fr-FR";
            case "de": return "de-DE";
            case "zh": return "zh-CN";
            case "ja": return "ja-JP";
            case "ko": return "ko-KR";
            case "ar": return "ar-SA";
            case "pt": return "pt-BR";
            case "it": return "it-IT";
            default: return "en-US";
        }
    }

    /**
     * Perform HTTP POST request
     */
    private String performHttpPost(String url, JsonObject body) {
        // Simplified synchronous implementation for demonstration
        // In production, use Vert.x WebClient with async/await

        try {
            java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();
            java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder()
                .uri(java.net.URI.create(url))
                .header("Content-Type", "application/json")
                .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body.encode()))
                .build();

            java.net.http.HttpResponse<String> response = client.send(request,
                java.net.http.HttpResponse.BodyHandlers.ofString());

            return response.body();
        } catch (Exception e) {
            logger.error("HTTP POST failed", e);
            throw new RuntimeException("HTTP request failed", e);
        }
    }

    /**
     * Perform HTTP POST request with Authorization header
     */
    private String performHttpPostWithAuth(String url, JsonObject body, String authHeader) {
        try {
            java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();
            java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder()
                .uri(java.net.URI.create(url))
                .header("Content-Type", "application/json")
                .header("Authorization", authHeader)
                .POST(java.net.http.HttpRequest.BodyPublishers.ofString(body.encode()))
                .build();

            java.net.http.HttpResponse<String> response = client.send(request,
                java.net.http.HttpResponse.BodyHandlers.ofString());

            return response.body();
        } catch (Exception e) {
            logger.error("HTTP POST with auth failed", e);
            throw new RuntimeException("HTTP request failed", e);
        }
    }

    /**
     * Perform HTTP GET request with Authorization header
     */
    private String performHttpGetWithAuth(String url, String authHeader) {
        try {
            java.net.http.HttpClient client = java.net.http.HttpClient.newHttpClient();
            java.net.http.HttpRequest request = java.net.http.HttpRequest.newBuilder()
                .uri(java.net.URI.create(url))
                .header("Authorization", authHeader)
                .GET()
                .build();

            java.net.http.HttpResponse<String> response = client.send(request,
                java.net.http.HttpResponse.BodyHandlers.ofString());

            return response.body();
        } catch (Exception e) {
            logger.error("HTTP GET with auth failed", e);
            throw new RuntimeException("HTTP request failed", e);
        }
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
