FROM maven:3.9-eclipse-temurin-17 AS build

WORKDIR /build

# Copy parent POM
COPY pom.xml .

# Copy shared module
COPY shared/pom.xml shared/
COPY shared/src shared/src

# Copy auth-service
COPY auth-service/pom.xml auth-service/
COPY auth-service/src auth-service/src

# Build
RUN mvn clean package -pl auth-service -am -DskipTests

# Runtime stage
FROM eclipse-temurin:17-jre-alpine

WORKDIR /app

COPY --from=build /build/auth-service/target/auth-service-1.0.0.jar app.jar

EXPOSE 3001

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:3001/health || exit 1

ENV JAVA_OPTS="-Xmx512m -Xms256m -XX:+UseG1GC -XX:MaxGCPauseMillis=200"

ENTRYPOINT sh -c "java $JAVA_OPTS -jar app.jar -conf '{\"port\":${PORT:-3001},\"cassandra\":{\"hosts\":\"${CASSANDRA_HOSTS:-localhost}\"},\"redis\":{\"host\":\"${REDIS_HOST:-localhost}\",\"port\":${REDIS_PORT:-6379}},\"jwt\":{\"secret\":\"${JWT_SECRET:-your-jwt-secret}\"}}'"
