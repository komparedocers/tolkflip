package com.tolkflip.group;

import com.tolkflip.shared.database.CassandraClient;
import io.vertx.core.AbstractVerticle;
import io.vertx.core.Promise;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.Router;
import io.vertx.ext.web.RoutingContext;
import io.vertx.ext.web.handler.BodyHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.List;

public class GroupServiceVerticle extends AbstractVerticle {
    private static final Logger logger = LoggerFactory.getLogger(GroupServiceVerticle.class);
    private static final int PORT = 3011;

    private CassandraClient cassandraClient;

    @Override
    public void start(Promise<Void> startPromise) {
        cassandraClient = new CassandraClient(vertx, config());

        Router router = Router.router(vertx);
        router.route().handler(BodyHandler.create());

        // Routes
        router.post("/api/groups").handler(this::createGroup);
        router.get("/api/groups/:groupId").handler(this::getGroup);
        router.post("/api/groups/:groupId/members").handler(this::addMember);
        router.delete("/api/groups/:groupId/members/:userId").handler(this::removeMember);
        router.get("/health").handler(this::healthCheck);

        vertx.createHttpServer()
            .requestHandler(router)
            .listen(PORT)
            .onSuccess(server -> {
                logger.info("Group Service started on port {}", PORT);
                startPromise.complete();
            })
            .onFailure(startPromise::fail);
    }

    private void createGroup(RoutingContext ctx) {
        JsonObject body = ctx.body().asJsonObject();

        String name = body.getString("name");
        String description = body.getString("description", "");
        String creatorId = body.getString("creatorId");
        JsonArray memberIdsArray = body.getJsonArray("memberIds", new JsonArray());
        List<String> memberIds = new ArrayList<>();
        for (int i = 0; i < memberIdsArray.size(); i++) {
            memberIds.add(memberIdsArray.getString(i));
        }

        cassandraClient.createGroup(name, description, creatorId, memberIds)
            .onSuccess(groupId -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject()
                        .put("success", true)
                        .put("groupId", groupId)
                        .encode());
            })
            .onFailure(err -> {
                logger.error("Failed to create group", err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to create group").encode());
            });
    }

    private void getGroup(RoutingContext ctx) {
        String groupId = ctx.pathParam("groupId");

        cassandraClient.getGroup(groupId)
            .onSuccess(group -> {
                if (group != null) {
                    ctx.response()
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("group", group).encode());
                } else {
                    ctx.response()
                        .setStatusCode(404)
                        .putHeader("Content-Type", "application/json")
                        .end(new JsonObject().put("error", "Group not found").encode());
                }
            })
            .onFailure(err -> {
                logger.error("Failed to get group {}", groupId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Internal server error").encode());
            });
    }

    private void addMember(RoutingContext ctx) {
        String groupId = ctx.pathParam("groupId");
        JsonObject body = ctx.body().asJsonObject();
        String userId = body.getString("userId");
        String role = body.getString("role", "member");

        cassandraClient.addGroupMember(groupId, userId, role)
            .onSuccess(v -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("success", true).encode());
            })
            .onFailure(err -> {
                logger.error("Failed to add member to group {}", groupId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to add member").encode());
            });
    }

    private void removeMember(RoutingContext ctx) {
        String groupId = ctx.pathParam("groupId");
        String userId = ctx.pathParam("userId");

        cassandraClient.removeGroupMember(groupId, userId)
            .onSuccess(v -> {
                ctx.response()
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("success", true).encode());
            })
            .onFailure(err -> {
                logger.error("Failed to remove member from group {}", groupId, err);
                ctx.response()
                    .setStatusCode(500)
                    .putHeader("Content-Type", "application/json")
                    .end(new JsonObject().put("error", "Failed to remove member").encode());
            });
    }

    private void healthCheck(RoutingContext ctx) {
        ctx.response()
            .putHeader("Content-Type", "application/json")
            .end(new JsonObject()
                .put("status", "ok")
                .put("service", "group-service")
                .encode());
    }

    @Override
    public void stop() {
        if (cassandraClient != null) {
            cassandraClient.close();
        }
    }
}
