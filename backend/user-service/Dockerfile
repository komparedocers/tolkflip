FROM node:18-alpine

WORKDIR /app

# Copy shared modules
COPY shared/ ./shared/

# Copy user service package files
COPY user-service/package*.json ./

# Install dependencies
RUN npm ci --only=production

# Copy user service source
COPY user-service/ .

# Expose port
EXPOSE 3002

# Start server
CMD ["node", "src/server.js"]
