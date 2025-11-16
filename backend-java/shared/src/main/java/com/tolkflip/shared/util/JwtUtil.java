package com.tolkflip.shared.util;

import io.vertx.core.json.JsonObject;
import io.vertx.ext.auth.JWTOptions;
import io.vertx.ext.auth.PubSecKeyOptions;
import io.vertx.ext.auth.jwt.JWTAuth;
import io.vertx.ext.auth.jwt.JWTAuthOptions;
import io.vertx.core.Vertx;

/**
 * JWT utility for token generation and validation
 */
public class JwtUtil {
    private final JWTAuth jwtAuth;
    private static final int ACCESS_TOKEN_EXPIRY = 3600; // 1 hour
    private static final int REFRESH_TOKEN_EXPIRY = 2592000; // 30 days

    public JwtUtil(Vertx vertx, String secret) {
        this.jwtAuth = JWTAuth.create(vertx, new JWTAuthOptions()
            .addPubSecKey(new PubSecKeyOptions()
                .setAlgorithm("HS256")
                .setBuffer(secret)));
    }

    /**
     * Generate access token
     */
    public String generateAccessToken(String userId, String phoneNumber) {
        JsonObject claims = new JsonObject()
            .put("userId", userId)
            .put("phoneNumber", phoneNumber)
            .put("type", "access");

        return jwtAuth.generateToken(claims, new JWTOptions()
            .setExpiresInSeconds(ACCESS_TOKEN_EXPIRY));
    }

    /**
     * Generate refresh token
     */
    public String generateRefreshToken(String userId) {
        JsonObject claims = new JsonObject()
            .put("userId", userId)
            .put("type", "refresh");

        return jwtAuth.generateToken(claims, new JWTOptions()
            .setExpiresInSeconds(REFRESH_TOKEN_EXPIRY));
    }

    /**
     * Validate token and extract claims
     */
    public JsonObject validateToken(String token) {
        try {
            return jwtAuth.authenticate(new JsonObject().put("jwt", token))
                .result()
                .principal();
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Get JWT auth provider
     */
    public JWTAuth getJwtAuth() {
        return jwtAuth;
    }
}
