FROM erlang:26-alpine AS builder

WORKDIR /app

# Copy rebar3 and config
COPY rebar.config rebar.lock* ./
COPY config ./config

# Copy all apps
COPY apps ./apps

# Get dependencies and compile
RUN rebar3 get-deps && \
    rebar3 as prod release

# Runtime stage
FROM alpine:3.18

# Install runtime dependencies
RUN apk add --no-cache \
    openssl \
    ncurses-libs \
    libstdc++ \
    libgcc

# Copy release from builder
COPY --from=builder /app/_build/prod/rel/tolkflip /opt/tolkflip

# Expose all service ports
EXPOSE 3000 3001 3002 3003 3004 3005 3006 3007 3008 3009 3010 3011

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD wget --no-verbose --tries=1 --spider http://localhost:3001/health || exit 1

# Set working directory
WORKDIR /opt/tolkflip

# Run as non-root user
RUN addgroup -g 1000 tolkflip && \
    adduser -D -u 1000 -G tolkflip tolkflip && \
    chown -R tolkflip:tolkflip /opt/tolkflip

USER tolkflip

# Start the release
CMD ["/opt/tolkflip/bin/tolkflip", "foreground"]
