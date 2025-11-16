package com.tolkflip.shared.database;

import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.redis.client.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;

/**
 * High-performance Redis client for caching and presence
 * Uses Vert.x Redis client for non-blocking operations
 */
public class RedisClient {
    private static final Logger logger = LoggerFactory.getLogger(RedisClient.class);

    private final Redis redis;

    public RedisClient(Vertx vertx, String host, int port) {
        RedisOptions options = new RedisOptions()
            .setConnectionString("redis://" + host + ":" + port)
            .setMaxPoolSize(32)
            .setMaxPoolWaiting(128);

        this.redis = Redis.createClient(vertx, options);

        logger.info("Connected to Redis at {}:{}", host, port);
    }

    /**
     * Convenience constructor that reads from config JsonObject
     */
    public RedisClient(Vertx vertx, JsonObject config) {
        this(vertx,
             config.getJsonObject("redis", new JsonObject())
                   .getString("host", "127.0.0.1"),
             config.getJsonObject("redis", new JsonObject())
                   .getInteger("port", 6379));
    }

    /**
     * Set a key-value pair
     */
    public Future<Void> set(String key, String value) {
        return redis.send(Request.cmd(Command.SET).arg(key).arg(value))
            .mapEmpty();
    }

    /**
     * Set a key-value pair with expiry (in seconds)
     */
    public Future<Void> setex(String key, int seconds, String value) {
        return redis.send(Request.cmd(Command.SETEX).arg(key).arg(seconds).arg(value))
            .mapEmpty();
    }

    /**
     * Get value by key
     */
    public Future<String> get(String key) {
        return redis.send(Request.cmd(Command.GET).arg(key))
            .map(response -> response != null ? response.toString() : null);
    }

    /**
     * Delete a key
     */
    public Future<Void> del(String key) {
        return redis.send(Request.cmd(Command.DEL).arg(key))
            .mapEmpty();
    }

    /**
     * Check if key exists
     */
    public Future<Boolean> exists(String key) {
        return redis.send(Request.cmd(Command.EXISTS).arg(key))
            .map(response -> response != null && response.toInteger() > 0);
    }

    /**
     * Set expiration on a key (in seconds)
     */
    public Future<Void> expire(String key, int seconds) {
        return redis.send(Request.cmd(Command.EXPIRE).arg(key).arg(seconds))
            .mapEmpty();
    }

    /**
     * Set hash field
     */
    public Future<Void> hset(String key, String field, String value) {
        return redis.send(Request.cmd(Command.HSET).arg(key).arg(field).arg(value))
            .mapEmpty();
    }

    /**
     * Get hash field
     */
    public Future<String> hget(String key, String field) {
        return redis.send(Request.cmd(Command.HGET).arg(key).arg(field))
            .map(response -> response != null ? response.toString() : null);
    }

    /**
     * Delete hash field
     */
    public Future<Void> hdel(String key, String field) {
        return redis.send(Request.cmd(Command.HDEL).arg(key).arg(field))
            .mapEmpty();
    }

    /**
     * Get all hash fields and values
     */
    public Future<JsonObject> hgetall(String key) {
        return redis.send(Request.cmd(Command.HGETALL).arg(key))
            .map(response -> {
                if (response == null) {
                    return new JsonObject();
                }

                JsonObject result = new JsonObject();
                Response[] arr = response.toArray();

                for (int i = 0; i < arr.length; i += 2) {
                    if (i + 1 < arr.length) {
                        String field = arr[i].toString();
                        String value = arr[i + 1].toString();
                        result.put(field, value);
                    }
                }

                return result;
            });
    }

    /**
     * Increment a counter
     */
    public Future<Long> incr(String key) {
        return redis.send(Request.cmd(Command.INCR).arg(key))
            .map(Response::toLong);
    }

    /**
     * Decrement a counter
     */
    public Future<Long> decr(String key) {
        return redis.send(Request.cmd(Command.DECR).arg(key))
            .map(Response::toLong);
    }

    /**
     * Set presence (online status)
     */
    public Future<Void> setPresence(String userId, String status) {
        return hset("presence", userId, status);
    }

    /**
     * Get presence (online status)
     */
    public Future<String> getPresence(String userId) {
        return hget("presence", userId);
    }

    /**
     * Remove presence
     */
    public Future<Void> removePresence(String userId) {
        return hdel("presence", userId);
    }

    /**
     * Cache object as JSON with expiry
     */
    public Future<Void> cacheObject(String key, JsonObject object, int expirySeconds) {
        return setex(key, expirySeconds, object.encode());
    }

    /**
     * Get cached object
     */
    public Future<JsonObject> getCachedObject(String key) {
        return get(key)
            .map(value -> value != null ? new JsonObject(value) : null);
    }

    /**
     * Publish a message to a channel
     */
    public Future<Void> publish(String channel, String message) {
        return redis.send(Request.cmd(Command.PUBLISH).arg(channel).arg(message))
            .mapEmpty();
    }

    /**
     * Add to a list (push)
     */
    public Future<Void> lpush(String key, String value) {
        return redis.send(Request.cmd(Command.LPUSH).arg(key).arg(value))
            .mapEmpty();
    }

    /**
     * Get from list (pop)
     */
    public Future<String> lpop(String key) {
        return redis.send(Request.cmd(Command.LPOP).arg(key))
            .map(response -> response != null ? response.toString() : null);
    }

    /**
     * Get list range
     */
    public Future<List<String>> lrange(String key, int start, int stop) {
        return redis.send(Request.cmd(Command.LRANGE).arg(key).arg(start).arg(stop))
            .map(response -> {
                if (response == null) {
                    return List.of();
                }

                return response.stream()
                    .map(Response::toString)
                    .toList();
            });
    }

    /**
     * Close the connection
     */
    public void close() {
        if (redis != null) {
            redis.close();
            logger.info("Redis connection closed");
        }
    }
}
