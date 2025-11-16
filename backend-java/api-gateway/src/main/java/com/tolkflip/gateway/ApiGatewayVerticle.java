package com.tolkflip.gateway;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.http.HttpClient;
import io.vertx.core.http.HttpClientOptions;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import io.vertx.ext.web.proxy.handler.ProxyHandler;
import io.vertx.httpproxy.HttpProxy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;

public class ApiGatewayVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(ApiGatewayVerticle.class);
    private static final int PORT = 3000;

    private static final Map<String, Integer> SERVICE_PORTS = new HashMap<String, Integer>() {{
        put("auth", 3001);
        put("user", 3002);
        put("chat", 3003);
        put("translation", 3004);
        put("transcription", 3005);
        put("media", 3006);
        put("presence", 3007);
        put("message-store", 3008);
        put("notification", 3009);
        put("webrtc", 3010);
        put("group", 3011);
    }};

    private HttpClient httpClient;
    private Map<String, HttpProxy> proxies = new HashMap<>();

    @Override
    public void start(Promise<Void> startPromise) {
        httpClient = vertx.createHttpClient(new HttpClientOptions()
            .setKeepAlive(true)
            .setMaxPoolSize(100));

        // Create HTTP proxies for each service
        for (Map.Entry<String, Integer> entry : SERVICE_PORTS.entrySet()) {
            HttpProxy proxy = HttpProxy.reverseProxy(httpClient);
            proxy.origin(entry.getValue(), "localhost");
            proxies.put(entry.getKey(), proxy);
        }

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Auth Service routes (3001)
        router.route("/request-code").handler(ctx -> proxyRequest(ctx, "auth"));
        router.route("/verify-code").handler(ctx -> proxyRequest(ctx, "auth"));
        router.route("/register").handler(ctx -> proxyRequest(ctx, "auth"));
        router.route("/login").handler(ctx -> proxyRequest(ctx, "auth"));
        router.route("/refresh").handler(ctx -> proxyRequest(ctx, "auth"));

        // User Service routes (3002)
        router.route("/api/users/*").handler(ctx -> proxyRequest(ctx, "user"));

        // Chat Service WebSocket (3003)
        router.route("/chat").handler(ctx -> proxyWebSocket(ctx, "chat"));

        // Translation Service routes (3004)
        router.route("/api/translate").handler(ctx -> proxyRequest(ctx, "translation"));

        // Transcription Service routes (3005)
        router.route("/api/transcribe").handler(ctx -> proxyRequest(ctx, "transcription"));

        // Media Service routes (3006)
        router.route("/api/media/*").handler(ctx -> proxyRequest(ctx, "media"));

        // Presence Service routes (3007)
        router.route("/api/presence/*").handler(ctx -> proxyRequest(ctx, "presence"));

        // Message Store Service routes (3008)
        router.route("/api/messages*").handler(ctx -> proxyRequest(ctx, "message-store"));

        // Notification Service routes (3009)
        router.route("/api/notifications/*").handler(ctx -> proxyRequest(ctx, "notification"));

        // WebRTC Service WebSocket (3010)
        router.route("/webrtc").handler(ctx -> proxyWebSocket(ctx, "webrtc"));

        // Group Service routes (3011)
        router.route("/api/groups*").handler(ctx -> proxyRequest(ctx, "group"));

        // Health check
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("API Gateway started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void proxyRequest(RoutingContext ctx, String service) {
        Integer targetPort = SERVICE_PORTS.get(service);
        if (targetPort == null) {
            ctx.response()
                .setStatusCode(502)
                .putHeader("Content-Type", "application/json")
                .end(new JsonObject().put("error", "Service not found").encode());
            return;
        }

        String path = ctx.request().path();
        String query = ctx.request().query();
        String targetUrl = path + (query != null ? "?" + query : "");

        logger.debug("Proxying {} {} to service {} on port {}",
            ctx.request().method(), targetUrl, service, targetPort);

        HttpProxy proxy = proxies.get(service);
        if (proxy != null) {
            ProxyHandler.create(proxy).handle(ctx);
        } else {
            // Fallback: manual proxy
            manualProxy(ctx, targetPort, targetUrl);
        }
    }

    private void manualProxy(RoutingContext ctx, int targetPort, String path) {
        httpClient.request(
            ctx.request().method(),
            targetPort,
            "localhost",
            path
        ).onSuccess(proxyRequest -> {
            // Copy headers
            ctx.request().headers().forEach(header -> {
                if (!header.getKey().equalsIgnoreCase("host")) {
                    proxyRequest.putHeader(header.getKey(), header.getValue());
                }
            });

            // Send request body if present
            if (ctx.body() != null && ctx.body().buffer() != null) {
                proxyRequest.send(ctx.body().buffer())
                    .onSuccess(proxyResponse -> {
                        ctx.response().setStatusCode(proxyResponse.statusCode());

                        // Copy response headers
                        proxyResponse.headers().forEach(header -> {
                            ctx.response().putHeader(header.getKey(), header.getValue());
                        });

                        // Pipe response body
                        proxyResponse.body()
                            .onSuccess(body -> ctx.response().end(body))
                            .onFailure(err -> {
                                logger.error("Failed to read proxy response body", err);
                                ctx.response().setStatusCode(502).end();
                            });
                    })
                    .onFailure(err -> {
                        logger.error("Failed to send proxy request", err);
                        ctx.response().setStatusCode(502).end();
                    });
            } else {
                proxyRequest.send()
                    .onSuccess(proxyResponse -> {
                        ctx.response().setStatusCode(proxyResponse.statusCode());

                        proxyResponse.headers().forEach(header -> {
                            ctx.response().putHeader(header.getKey(), header.getValue());
                        });

                        proxyResponse.body()
                            .onSuccess(body -> ctx.response().end(body))
                            .onFailure(err -> {
                                logger.error("Failed to read proxy response body", err);
                                ctx.response().setStatusCode(502).end();
                            });
                    })
                    .onFailure(err -> {
                        logger.error("Failed to send proxy request", err);
                        ctx.response().setStatusCode(502).end();
                    });
            }
        }).onFailure(err -> {
            logger.error("Failed to create proxy request", err);
            ctx.response().setStatusCode(502).end();
        });
    }

    private void proxyWebSocket(RoutingContext ctx, String service) {
        Integer targetPort = SERVICE_PORTS.get(service);
        if (targetPort == null) {
            ctx.response()
                .setStatusCode(502)
                .end("Service not found");
            return;
        }

        // In production, implement proper WebSocket proxying
        // For now, let clients connect directly to the services
        logger.info("WebSocket proxy for {} - clients should connect to port {}",
            service, targetPort);

        ctx.response()
            .setStatusCode(503)
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("error", "WebSocket proxy not implemented")
                .put("directPort", targetPort)
                .encode());
    }

    private void healthCheck(RoutingContext ctx) {
        // Check health of all backend services
        JsonObject healthStatus = new JsonObject();
        healthStatus.put("status", "ok");
        healthStatus.put("service", "api-gateway");

        JsonObject backends = new JsonObject();

        // In production, actually check each service's health
        for (String service : SERVICE_PORTS.keySet()) {
            backends.put(service, "ok");
        }

        healthStatus.put("backends", backends);

        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(healthStatus.encode());
    }

    @Override
    public void stop() {
        if (httpClient != null) {
            httpClient.close();
        }
    }
}
