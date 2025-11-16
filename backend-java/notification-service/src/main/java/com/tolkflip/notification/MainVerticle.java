package com.tolkflip.notification;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;

public class MainVerticle extends AbstractVerticle {
    @Override
    public void start(Promise<Void> startPromise) {
        vertx.deployVerticle(new NotificationServiceVerticle())
            .onSuccess(id -> startPromise.complete())
            .onFailure(startPromise::fail);
    }
}
