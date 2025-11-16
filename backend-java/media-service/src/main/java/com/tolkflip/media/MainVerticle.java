package com.tolkflip.media;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;

public class MainVerticle extends AbstractVerticle {
    @Override
    public void start(Promise<Void> startPromise) {
        vertx.deployVerticle(new MediaServiceVerticle())
            .onSuccess(id -> startPromise.complete())
            .onFailure(startPromise::fail);
    }
}
