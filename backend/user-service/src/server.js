// User Service - Profile management
require('dotenv').config();
const express = require('express');
const { body, validationResult } = require('express-validator');
const winston = require('winston');

const DatabaseModels = require('../../shared/models/database');
const RedisClient = require('../../shared/utils/redis-client');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'user-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'user-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const PORT = process.env.PORT || 3002;

// Database and Redis initialization
const db = new DatabaseModels(
  process.env.CASSANDRA_HOSTS?.split(',') || ['127.0.0.1'],
  'tolkflip'
);
const redisClient = new RedisClient();

// Initialize connections
async function initialize() {
  try {
    await db.connect();
    await redisClient.connect();
    logger.info('User service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize user service:', error);
    process.exit(1);
  }
}

initialize();

// Middleware
app.use(express.json());

// Get user profile
app.get('/profile/:userId', async (req, res) => {
  try {
    const { userId } = req.params;

    // Check cache
    const cached = await redisClient.cacheGet(`user:${userId}`);
    if (cached) {
      return res.json({ success: true, user: cached });
    }

    // Fetch from database
    const user = await db.getUserById(userId);

    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    const userData = {
      userId: user.user_id.toString(),
      phoneNumber: user.phone_number,
      displayName: user.display_name,
      avatarUrl: user.avatar_url,
      primaryLanguage: user.primary_language,
      additionalLanguages: user.additional_languages,
      createdAt: user.created_at,
      lastActive: user.last_active
    };

    // Cache it
    await redisClient.cacheSet(`user:${userId}`, userData, 3600);

    res.json({ success: true, user: userData });
  } catch (error) {
    logger.error('Error fetching user profile:', error);
    res.status(500).json({ error: 'Failed to fetch profile' });
  }
});

// Update user profile
app.put(
  '/profile',
  [
    body('display_name').optional().isString(),
    body('avatar_url').optional().isURL(),
    body('primary_language').optional().isString(),
    body('additional_languages').optional().isArray()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const userId = req.headers['x-user-id'];

      if (!userId) {
        return res.status(401).json({ error: 'User ID required' });
      }

      const {
        display_name,
        avatar_url,
        primary_language,
        additional_languages
      } = req.body;

      // Build update query dynamically
      const updates = [];
      const params = [];

      if (display_name) {
        updates.push('display_name = ?');
        params.push(display_name);
      }

      if (avatar_url) {
        updates.push('avatar_url = ?');
        params.push(avatar_url);
      }

      if (primary_language) {
        updates.push('primary_language = ?');
        params.push(primary_language);
      }

      if (additional_languages) {
        // Validate language limit (max 2 additional languages)
        if (additional_languages.length > 2) {
          return res.status(400).json({
            error: 'Maximum 2 additional languages allowed'
          });
        }
        updates.push('additional_languages = ?');
        params.push(additional_languages);
      }

      if (updates.length === 0) {
        return res.status(400).json({ error: 'No fields to update' });
      }

      // Update last_active
      updates.push('last_active = ?');
      params.push(new Date());

      params.push(userId);

      const query = `
        UPDATE users
        SET ${updates.join(', ')}
        WHERE user_id = ?
      `;

      await db.client.execute(query, params, { prepare: true });

      // Clear cache
      await redisClient.cacheDel(`user:${userId}`);

      logger.info(`User profile updated: ${userId}`);

      // Fetch updated profile
      const updatedUser = await db.getUserById(userId);

      const userData = {
        userId: updatedUser.user_id.toString(),
        phoneNumber: updatedUser.phone_number,
        displayName: updatedUser.display_name,
        avatarUrl: updatedUser.avatar_url,
        primaryLanguage: updatedUser.primary_language,
        additionalLanguages: updatedUser.additional_languages
      };

      res.json({ success: true, user: userData });
    } catch (error) {
      logger.error('Error updating user profile:', error);
      res.status(500).json({ error: 'Failed to update profile' });
    }
  }
);

// Search users by phone number
app.get('/search', async (req, res) => {
  try {
    const { phone } = req.query;

    if (!phone) {
      return res.status(400).json({ error: 'Phone number required' });
    }

    const user = await db.getUserByPhone(phone);

    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    res.json({
      success: true,
      user: {
        userId: user.user_id.toString(),
        phoneNumber: user.phone_number,
        displayName: user.display_name,
        avatarUrl: user.avatar_url,
        primaryLanguage: user.primary_language
      }
    });
  } catch (error) {
    logger.error('Error searching user:', error);
    res.status(500).json({ error: 'Search failed' });
  }
});

// Get user's contacts
app.get('/contacts', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];

    if (!userId) {
      return res.status(401).json({ error: 'User ID required' });
    }

    // Get user's threads
    const threads = await db.getThreadsForUser(userId);

    // Extract unique contact IDs
    const contactIds = [...new Set(
      threads.map(thread => thread.other_participant_id.toString())
    )];

    // Fetch contact details
    const contacts = [];
    for (const contactId of contactIds) {
      const user = await db.getUserById(contactId);
      if (user) {
        contacts.push({
          userId: user.user_id.toString(),
          phoneNumber: user.phone_number,
          displayName: user.display_name,
          avatarUrl: user.avatar_url,
          primaryLanguage: user.primary_language
        });
      }
    }

    res.json({ success: true, contacts });
  } catch (error) {
    logger.error('Error fetching contacts:', error);
    res.status(500).json({ error: 'Failed to fetch contacts' });
  }
});

// Update device token for push notifications
app.post('/device-token', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];
    const { token, platform } = req.body;

    if (!userId || !token) {
      return res.status(400).json({ error: 'User ID and token required' });
    }

    // Fetch current user
    const user = await db.getUserById(userId);

    if (!user) {
      return res.status(404).json({ error: 'User not found' });
    }

    // Add token to list (avoiding duplicates)
    let deviceTokens = user.device_tokens || [];
    if (!deviceTokens.includes(token)) {
      deviceTokens.push(token);

      const query = `
        UPDATE users
        SET device_tokens = ?
        WHERE user_id = ?
      `;

      await db.client.execute(query, [deviceTokens, userId], { prepare: true });

      logger.info(`Device token added for user ${userId}`);
    }

    res.json({ success: true });
  } catch (error) {
    logger.error('Error updating device token:', error);
    res.status(500).json({ error: 'Failed to update device token' });
  }
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'healthy', service: 'user' });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`User service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  server.close(async () => {
    await db.close();
    await redisClient.close();
    logger.info('User service shut down gracefully');
    process.exit(0);
  });
});

module.exports = app;
