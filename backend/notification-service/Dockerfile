FROM node:18-alpine

WORKDIR /app

# Copy shared modules
COPY shared/ ./shared/

# Copy notification service package files
COPY notification-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy notification service source
COPY notification-service/ .

# Expose port
EXPOSE 3009

# Start server
CMD ["node", "src/server.js"]
