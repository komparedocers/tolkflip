// Authentication Service - Phone number based auth
require('dotenv').config();
const express = require('express');
const { body, validationResult } = require('express-validator');
const jwt = require('jsonwebtoken');
const { v4: uuidv4 } = require('uuid');
const winston = require('winston');

// Import shared modules
const DatabaseModels = require('../../shared/models/database');
const RedisClient = require('../../shared/utils/redis-client');
const encryption = require('../../shared/utils/encryption');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'auth-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'auth-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const PORT = process.env.PORT || 3001;

// Database and Redis initialization
const db = new DatabaseModels(
  process.env.CASSANDRA_HOSTS?.split(',') || ['127.0.0.1'],
  'tolkflip'
);
const redisClient = new RedisClient();

// Middleware
app.use(express.json());

// Initialize connections
async function initialize() {
  try {
    await db.connect();
    await db.initializeSchema();
    await redisClient.connect();
    logger.info('Auth service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize auth service:', error);
    process.exit(1);
  }
}

initialize();

// Twilio client for SMS (configure with your credentials)
let twilioClient = null;
if (process.env.TWILIO_ACCOUNT_SID && process.env.TWILIO_AUTH_TOKEN) {
  const twilio = require('twilio');
  twilioClient = twilio(
    process.env.TWILIO_ACCOUNT_SID,
    process.env.TWILIO_AUTH_TOKEN
  );
}

// Send verification code via SMS
async function sendVerificationSMS(phoneNumber, code) {
  if (!twilioClient) {
    logger.warn('Twilio not configured, verification code:', code);
    return { success: true, mock: true };
  }

  try {
    await twilioClient.messages.create({
      body: `Your Tolkflip verification code is: ${code}`,
      from: process.env.TWILIO_PHONE_NUMBER,
      to: phoneNumber
    });
    return { success: true };
  } catch (error) {
    logger.error('Failed to send SMS:', error);
    throw new Error('Failed to send verification code');
  }
}

// Request verification code
app.post(
  '/request-code',
  [
    body('phone_number')
      .isMobilePhone()
      .withMessage('Invalid phone number format')
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { phone_number } = req.body;
      const code = encryption.generateVerificationCode();

      // Store verification code in Redis with 10 minute expiry
      await redisClient.cacheSet(
        `verification:${phone_number}`,
        { code, attempts: 0 },
        600
      );

      // Send SMS
      await sendVerificationSMS(phone_number, code);

      logger.info(`Verification code sent to ${phone_number}`);

      res.json({
        success: true,
        message: 'Verification code sent',
        expiresIn: 600
      });
    } catch (error) {
      logger.error('Error requesting verification code:', error);
      res.status(500).json({ error: 'Failed to send verification code' });
    }
  }
);

// Verify code and register/login
app.post(
  '/verify',
  [
    body('phone_number').isMobilePhone(),
    body('code').isLength({ min: 6, max: 6 }),
    body('display_name').optional().isString(),
    body('primary_language').optional().isString()
  ],
  async (req, res) => {
    const errors = validationResult(req);
    if (!errors.isEmpty()) {
      return res.status(400).json({ errors: errors.array() });
    }

    try {
      const { phone_number, code, display_name, primary_language } = req.body;

      // Get stored verification data
      const verificationData = await redisClient.cacheGet(`verification:${phone_number}`);

      if (!verificationData) {
        return res.status(400).json({ error: 'Verification code expired' });
      }

      // Check attempts
      if (verificationData.attempts >= 3) {
        await redisClient.cacheDel(`verification:${phone_number}`);
        return res.status(429).json({ error: 'Too many failed attempts' });
      }

      // Verify code
      if (verificationData.code !== code) {
        verificationData.attempts += 1;
        await redisClient.cacheSet(
          `verification:${phone_number}`,
          verificationData,
          600
        );
        return res.status(400).json({
          error: 'Invalid verification code',
          attemptsRemaining: 3 - verificationData.attempts
        });
      }

      // Code verified, clear from Redis
      await redisClient.cacheDel(`verification:${phone_number}`);

      // Check if user exists
      let user = await db.getUserByPhone(phone_number);

      if (!user) {
        // Create new user
        const userId = uuidv4();
        const keyPair = encryption.generateKeyPair();

        await db.createUser({
          user_id: userId,
          phone_number,
          display_name: display_name || phone_number,
          primary_language: primary_language || 'en',
          additional_languages: [],
          avatar_url: ''
        });

        user = await db.getUserByPhone(phone_number);

        // Store private key securely (in production, use key management service)
        await redisClient.cacheSet(
          `user:${userId}:private_key`,
          keyPair.privateKey,
          null
        );

        // Store public key
        await redisClient.cacheSet(
          `user:${userId}:public_key`,
          keyPair.publicKey,
          null
        );

        logger.info(`New user registered: ${userId}`);
      } else {
        logger.info(`User logged in: ${user.user_id}`);
      }

      // Generate JWT token
      const token = jwt.sign(
        {
          userId: user.user_id.toString(),
          phoneNumber: phone_number
        },
        process.env.JWT_SECRET || 'your-secret-key',
        { expiresIn: '30d' }
      );

      // Generate refresh token
      const refreshToken = jwt.sign(
        {
          userId: user.user_id.toString(),
          type: 'refresh'
        },
        process.env.JWT_REFRESH_SECRET || 'your-refresh-secret',
        { expiresIn: '90d' }
      );

      // Store refresh token
      await redisClient.cacheSet(
        `refresh_token:${user.user_id}`,
        refreshToken,
        90 * 24 * 60 * 60 // 90 days
      );

      res.json({
        success: true,
        token,
        refreshToken,
        user: {
          userId: user.user_id.toString(),
          phoneNumber: user.phone_number,
          displayName: user.display_name,
          primaryLanguage: user.primary_language,
          additionalLanguages: user.additional_languages,
          avatarUrl: user.avatar_url
        }
      });
    } catch (error) {
      logger.error('Error verifying code:', error);
      res.status(500).json({ error: 'Verification failed' });
    }
  }
);

// Refresh token
app.post('/refresh', async (req, res) => {
  try {
    const { refreshToken } = req.body;

    if (!refreshToken) {
      return res.status(400).json({ error: 'Refresh token required' });
    }

    // Verify refresh token
    const decoded = jwt.verify(
      refreshToken,
      process.env.JWT_REFRESH_SECRET || 'your-refresh-secret'
    );

    if (decoded.type !== 'refresh') {
      return res.status(401).json({ error: 'Invalid token type' });
    }

    // Check if refresh token exists in Redis
    const storedToken = await redisClient.cacheGet(`refresh_token:${decoded.userId}`);

    if (storedToken !== refreshToken) {
      return res.status(401).json({ error: 'Invalid refresh token' });
    }

    // Generate new access token
    const newToken = jwt.sign(
      { userId: decoded.userId },
      process.env.JWT_SECRET || 'your-secret-key',
      { expiresIn: '30d' }
    );

    res.json({
      success: true,
      token: newToken
    });
  } catch (error) {
    logger.error('Error refreshing token:', error);
    res.status(401).json({ error: 'Invalid refresh token' });
  }
});

// Logout
app.post('/logout', async (req, res) => {
  try {
    const { userId } = req.body;

    if (userId) {
      await redisClient.cacheDel(`refresh_token:${userId}`);
    }

    res.json({ success: true, message: 'Logged out successfully' });
  } catch (error) {
    logger.error('Error logging out:', error);
    res.status(500).json({ error: 'Logout failed' });
  }
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'healthy', service: 'auth' });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`Auth service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  server.close(async () => {
    await db.close();
    await redisClient.close();
    logger.info('Auth service shut down gracefully');
    process.exit(0);
  });
});

module.exports = app;
