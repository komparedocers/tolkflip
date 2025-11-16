FROM node:18-alpine

WORKDIR /app

# Copy shared modules
COPY shared/ ./shared/

# Copy message store service package files
COPY message-store-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy message store service source
COPY message-store-service/ .

# Expose port
EXPOSE 3008

# Start server
CMD ["node", "src/server.js"]
