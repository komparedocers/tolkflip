FROM node:18-alpine

WORKDIR /app

# Copy shared modules first
COPY shared/ ./shared/

# Copy auth service package files
COPY auth-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy auth service source
COPY auth-service/ .

# Expose port
EXPOSE 3001

# Start server
CMD ["node", "src/server.js"]
