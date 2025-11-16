FROM node:18-alpine

WORKDIR /app

# Copy shared modules
COPY shared/ ./shared/

# Copy chat service package files
COPY chat-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy chat service source
COPY chat-service/ .

# Expose port
EXPOSE 3003

# Start server
CMD ["node", "src/server.js"]
