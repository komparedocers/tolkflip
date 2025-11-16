// Message Store Service - Handles all message persistence to Cassandra
require('dotenv').config();
const express = require('express');
const winston = require('winston');
const { v4: uuidv4 } = require('uuid');

const DatabaseModels = require('../../shared/models/database');
const RedisClient = require('../../shared/utils/redis-client');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'message-store-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'message-store-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const PORT = process.env.PORT || 3008;

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
    await db.initializeSchema();
    await redisClient.connect();
    logger.info('Message Store service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize message store service:', error);
    process.exit(1);
  }
}

initialize();

// Middleware
app.use(express.json());

// Save message
app.post('/messages', async (req, res) => {
  try {
    const messageData = req.body;

    // Add message ID if not provided
    if (!messageData.message_id) {
      messageData.message_id = uuidv4();
    }

    await db.saveMessage(messageData);

    // Update thread metadata
    await db.createOrUpdateThread({
      thread_id: messageData.thread_id,
      participant_id: messageData.sender_id,
      other_participant_id: messageData.receiver_id,
      thread_type: messageData.is_group ? 'group' : 'direct',
      last_message_time: new Date(),
      last_message_preview: messageData.content.substring(0, 100)
    });

    // Update for receiver
    await db.createOrUpdateThread({
      thread_id: messageData.thread_id,
      participant_id: messageData.receiver_id,
      other_participant_id: messageData.sender_id,
      thread_type: messageData.is_group ? 'group' : 'direct',
      last_message_time: new Date(),
      last_message_preview: messageData.content.substring(0, 100),
      unread_count: 1
    });

    logger.info(`Message saved: ${messageData.message_id}`);

    res.json({
      success: true,
      message_id: messageData.message_id
    });
  } catch (error) {
    logger.error('Error saving message:', error);
    res.status(500).json({ error: 'Failed to save message' });
  }
});

// Get messages for thread
app.get('/messages/:threadId', async (req, res) => {
  try {
    const { threadId } = req.params;
    const { limit = 50, pageState } = req.query;

    const result = await db.getMessages(threadId, parseInt(limit), pageState);

    res.json({
      success: true,
      messages: result.rows,
      pageState: result.pageState
    });
  } catch (error) {
    logger.error('Error fetching messages:', error);
    res.status(500).json({ error: 'Failed to fetch messages' });
  }
});

// Update message status
app.put('/messages/:threadId/:messageId/status', async (req, res) => {
  try {
    const { threadId, messageId } = req.params;
    const { status } = req.body;

    await db.updateMessageStatus(threadId, messageId, status);

    res.json({ success: true });
  } catch (error) {
    logger.error('Error updating message status:', error);
    res.status(500).json({ error: 'Failed to update message status' });
  }
});

// Get threads for user
app.get('/threads/:userId', async (req, res) => {
  try {
    const { userId } = req.params;
    const { limit = 20 } = req.query;

    const threads = await db.getThreadsForUser(userId, parseInt(limit));

    res.json({
      success: true,
      threads
    });
  } catch (error) {
    logger.error('Error fetching threads:', error);
    res.status(500).json({ error: 'Failed to fetch threads' });
  }
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'healthy', service: 'message-store' });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`Message Store service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  server.close(async () => {
    await db.close();
    await redisClient.close();
    logger.info('Message Store service shut down gracefully');
    process.exit(0);
  });
});

module.exports = app;
