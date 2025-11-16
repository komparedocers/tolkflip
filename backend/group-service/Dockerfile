FROM node:18-alpine

WORKDIR /app

# Copy shared models first
COPY shared/ ./shared/

# Copy service-specific files
COPY group-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy application code
COPY group-service/src ./src

# Expose port
EXPOSE 3011

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3011/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

# Run the service
CMD ["npm", "start"]
