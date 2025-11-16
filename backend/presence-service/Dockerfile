FROM node:18-alpine

WORKDIR /app

# Copy shared modules
COPY shared/ ./shared/

# Copy presence service package files
COPY presence-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy presence service source
COPY presence-service/ .

# Expose port
EXPOSE 3007

# Start server
CMD ["node", "src/server.js"]
