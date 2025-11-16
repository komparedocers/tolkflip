package com.tolkflip.gateway;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;

public class MainVerticle extends AbstractVerticle {
    @Override
    public void start(Promise<Void> startPromise) {
        vertx.deployVerticle(new ApiGatewayVerticle())
            .onSuccess(id -> startPromise.complete())
            .onFailure(startPromise::fail);
    }
}
