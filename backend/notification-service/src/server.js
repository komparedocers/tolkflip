// Notification Service - Push notifications via FCM and APNS
require('dotenv').config();
const express = require('express');
const admin = require('firebase-admin');
const apn = require('apn');
const winston = require('winston');

const DatabaseModels = require('../../shared/models/database');
const RedisClient = require('../../shared/utils/redis-client');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'notification-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'notification-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const PORT = process.env.PORT || 3009;

// Database and Redis initialization
const db = new DatabaseModels(
  process.env.CASSANDRA_HOSTS?.split(',') || ['127.0.0.1'],
  'tolkflip'
);
const redisClient = new RedisClient();

// Initialize Firebase Admin (if credentials provided)
let firebaseInitialized = false;
if (process.env.FIREBASE_SERVICE_ACCOUNT) {
  try {
    const serviceAccount = JSON.parse(process.env.FIREBASE_SERVICE_ACCOUNT);
    admin.initializeApp({
      credential: admin.credential.cert(serviceAccount)
    });
    firebaseInitialized = true;
    logger.info('Firebase Admin initialized');
  } catch (error) {
    logger.error('Failed to initialize Firebase:', error);
  }
}

// Initialize APNS (if credentials provided)
let apnProvider = null;
if (process.env.APNS_KEY_PATH) {
  apnProvider = new apn.Provider({
    token: {
      key: process.env.APNS_KEY_PATH,
      keyId: process.env.APNS_KEY_ID,
      teamId: process.env.APNS_TEAM_ID
    },
    production: process.env.NODE_ENV === 'production'
  });
  logger.info('APNS initialized');
}

// Initialize connections
async function initialize() {
  try {
    await db.connect();
    await redisClient.connect();
    logger.info('Notification service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize notification service:', error);
    process.exit(1);
  }
}

initialize();

// Middleware
app.use(express.json());

// Send notification to a user
app.post('/send', async (req, res) => {
  try {
    const { userId, title, body, data, platform } = req.body;

    if (!userId || !title || !body) {
      return res.status(400).json({ error: 'userId, title, and body are required' });
    }

    // Get user's device tokens
    const user = await db.getUserById(userId);

    if (!user || !user.device_tokens || user.device_tokens.length === 0) {
      return res.status(404).json({ error: 'No device tokens found for user' });
    }

    const results = {
      fcm: [],
      apns: []
    };

    // Send to FCM (Android)
    if (firebaseInitialized && (!platform || platform === 'android')) {
      const fcmTokens = user.device_tokens.filter(t => !t.startsWith('apns:'));

      if (fcmTokens.length > 0) {
        const message = {
          notification: {
            title,
            body
          },
          data: data || {},
          tokens: fcmTokens
        };

        try {
          const response = await admin.messaging().sendMulticast(message);
          results.fcm = {
            success: response.successCount,
            failure: response.failureCount
          };
          logger.info(`FCM sent: ${response.successCount} success, ${response.failureCount} failed`);
        } catch (error) {
          logger.error('FCM send error:', error);
          results.fcm = { error: error.message };
        }
      }
    }

    // Send to APNS (iOS)
    if (apnProvider && (!platform || platform === 'ios')) {
      const apnsTokens = user.device_tokens
        .filter(t => t.startsWith('apns:'))
        .map(t => t.replace('apns:', ''));

      if (apnsTokens.length > 0) {
        const notification = new apn.Notification();
        notification.alert = {
          title,
          body
        };
        notification.topic = process.env.APNS_BUNDLE_ID || 'com.tolkflip';
        notification.payload = data || {};
        notification.sound = 'default';

        try {
          const response = await apnProvider.send(notification, apnsTokens);
          results.apns = {
            sent: response.sent.length,
            failed: response.failed.length
          };
          logger.info(`APNS sent: ${response.sent.length} success, ${response.failed.length} failed`);
        } catch (error) {
          logger.error('APNS send error:', error);
          results.apns = { error: error.message };
        }
      }
    }

    res.json({
      success: true,
      results
    });
  } catch (error) {
    logger.error('Error sending notification:', error);
    res.status(500).json({ error: 'Failed to send notification' });
  }
});

// Send notification to multiple users
app.post('/send-batch', async (req, res) => {
  try {
    const { userIds, title, body, data } = req.body;

    if (!userIds || !Array.isArray(userIds) || userIds.length === 0) {
      return res.status(400).json({ error: 'userIds array is required' });
    }

    const results = [];

    for (const userId of userIds) {
      try {
        const response = await sendNotificationToUser(userId, title, body, data);
        results.push({ userId, success: true, ...response });
      } catch (error) {
        results.push({ userId, success: false, error: error.message });
      }
    }

    res.json({
      success: true,
      results
    });
  } catch (error) {
    logger.error('Error sending batch notifications:', error);
    res.status(500).json({ error: 'Failed to send batch notifications' });
  }
});

// Helper function to send notification to a single user
async function sendNotificationToUser(userId, title, body, data) {
  const user = await db.getUserById(userId);

  if (!user || !user.device_tokens || user.device_tokens.length === 0) {
    throw new Error('No device tokens found');
  }

  const results = {};

  // FCM
  if (firebaseInitialized) {
    const fcmTokens = user.device_tokens.filter(t => !t.startsWith('apns:'));
    if (fcmTokens.length > 0) {
      const message = {
        notification: { title, body },
        data: data || {},
        tokens: fcmTokens
      };

      const response = await admin.messaging().sendMulticast(message);
      results.fcm = {
        success: response.successCount,
        failure: response.failureCount
      };
    }
  }

  // APNS
  if (apnProvider) {
    const apnsTokens = user.device_tokens
      .filter(t => t.startsWith('apns:'))
      .map(t => t.replace('apns:', ''));

    if (apnsTokens.length > 0) {
      const notification = new apn.Notification();
      notification.alert = { title, body };
      notification.topic = process.env.APNS_BUNDLE_ID || 'com.tolkflip';
      notification.payload = data || {};
      notification.sound = 'default';

      const response = await apnProvider.send(notification, apnsTokens);
      results.apns = {
        sent: response.sent.length,
        failed: response.failed.length
      };
    }
  }

  return results;
}

// Health check
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    service: 'notification',
    fcm: firebaseInitialized,
    apns: !!apnProvider
  });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`Notification service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  server.close(async () => {
    if (apnProvider) {
      apnProvider.shutdown();
    }
    await db.close();
    await redisClient.close();
    logger.info('Notification service shut down gracefully');
    process.exit(0);
  });
});

module.exports = app;
