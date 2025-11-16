FROM node:18-alpine

WORKDIR /app

# Copy shared modules
COPY shared/ ./shared/

# Copy media service package files
COPY media-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy media service source
COPY media-service/ .

# Expose port
EXPOSE 3006

# Start server
CMD ["node", "src/server.js"]
