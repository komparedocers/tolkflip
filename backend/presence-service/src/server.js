// Presence Service - User online status and typing indicators
require('dotenv').config();
const express = require('express');
const winston = require('winston');

const RedisClient = require('../../shared/utils/redis-client');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'presence-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'presence-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const PORT = process.env.PORT || 3007;

// Redis initialization
const redisClient = new RedisClient();

// Initialize connections
async function initialize() {
  try {
    await redisClient.connect();
    logger.info('Presence service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize presence service:', error);
    process.exit(1);
  }
}

initialize();

// Middleware
app.use(express.json());

// Update user presence to online
app.post('/online', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];
    const { device_id, metadata } = req.body;

    if (!userId) {
      return res.status(401).json({ error: 'User ID required' });
    }

    await redisClient.setUserOnline(userId, {
      deviceId: device_id,
      ...metadata
    });

    logger.info(`User ${userId} is now online`);

    res.json({ success: true, status: 'online' });
  } catch (error) {
    logger.error('Error setting user online:', error);
    res.status(500).json({ error: 'Failed to update presence' });
  }
});

// Update user presence to offline
app.post('/offline', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];

    if (!userId) {
      return res.status(401).json({ error: 'User ID required' });
    }

    await redisClient.setUserOffline(userId);

    logger.info(`User ${userId} is now offline`);

    res.json({ success: true, status: 'offline' });
  } catch (error) {
    logger.error('Error setting user offline:', error);
    res.status(500).json({ error: 'Failed to update presence' });
  }
});

// Get user presence
app.get('/user/:userId', async (req, res) => {
  try {
    const { userId } = req.params;

    const presence = await redisClient.getUserPresence(userId);

    res.json({
      success: true,
      userId,
      ...presence
    });
  } catch (error) {
    logger.error('Error getting user presence:', error);
    res.status(500).json({ error: 'Failed to get presence' });
  }
});

// Get multiple users' presence
app.post('/users/batch', async (req, res) => {
  try {
    const { userIds } = req.body;

    if (!Array.isArray(userIds)) {
      return res.status(400).json({ error: 'userIds must be an array' });
    }

    const presences = {};

    for (const userId of userIds) {
      presences[userId] = await redisClient.getUserPresence(userId);
    }

    res.json({
      success: true,
      presences
    });
  } catch (error) {
    logger.error('Error getting batch presence:', error);
    res.status(500).json({ error: 'Failed to get presences' });
  }
});

// Set typing indicator
app.post('/typing', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];
    const { threadId, isTyping } = req.body;

    if (!userId || !threadId) {
      return res.status(400).json({ error: 'userId and threadId required' });
    }

    if (isTyping) {
      await redisClient.setUserTyping(userId, threadId);
    } else {
      await redisClient.removeUserTyping(userId, threadId);
    }

    res.json({ success: true });
  } catch (error) {
    logger.error('Error updating typing indicator:', error);
    res.status(500).json({ error: 'Failed to update typing indicator' });
  }
});

// Get online users (for admin/monitoring)
app.get('/online/users', async (req, res) => {
  try {
    const onlineUsers = await redisClient.getOnlineUsers();

    res.json({
      success: true,
      count: onlineUsers.length,
      users: onlineUsers
    });
  } catch (error) {
    logger.error('Error getting online users:', error);
    res.status(500).json({ error: 'Failed to get online users' });
  }
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'healthy', service: 'presence' });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`Presence service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  server.close(async () => {
    await redisClient.close();
    logger.info('Presence service shut down gracefully');
    process.exit(0);
  });
});

module.exports = app;
