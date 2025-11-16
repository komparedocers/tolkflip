package com.tolkflip.transcription;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;

public class MainVerticle extends AbstractVerticle {
    @Override
    public void start(Promise<Void> startPromise) {
        vertx.deployVerticle(new TranscriptionServiceVerticle())
            .onSuccess(id -> startPromise.complete())
            .onFailure(startPromise::fail);
    }
}
