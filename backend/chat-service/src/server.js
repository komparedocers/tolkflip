// Chat Service - Real-time messaging with WebSocket
require('dotenv').config();
const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const jwt = require('jsonwebtoken');
const { v4: uuidv4 } = require('uuid');
const winston = require('winston');
const axios = require('axios');

// Import shared modules
const DatabaseModels = require('../../shared/models/database');
const RedisClient = require('../../shared/utils/redis-client');
const encryption = require('../../shared/utils/encryption');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'chat-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'chat-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const server = http.createServer(app);
const io = socketIo(server, {
  cors: {
    origin: process.env.ALLOWED_ORIGINS?.split(',') || '*',
    credentials: true
  }
});

const PORT = process.env.PORT || 3003;

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
    await redisClient.connect();
    logger.info('Chat service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize chat service:', error);
    process.exit(1);
  }
}

initialize();

// Store active connections
const activeConnections = new Map(); // userId -> socket

// WebSocket authentication middleware
io.use((socket, next) => {
  try {
    const token = socket.handshake.auth.token;

    if (!token) {
      return next(new Error('Authentication error'));
    }

    const decoded = jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key');
    socket.userId = decoded.userId;
    next();
  } catch (err) {
    logger.error('WebSocket auth error:', err);
    next(new Error('Authentication error'));
  }
});

// WebSocket connection handling
io.on('connection', (socket) => {
  const userId = socket.userId;
  logger.info(`User connected: ${userId}`);

  // Store connection
  activeConnections.set(userId, socket);

  // Update presence
  updateUserPresence(userId, 'online');

  // Join user's personal room
  socket.join(`user:${userId}`);

  // Handle typing indicator
  socket.on('typing', async (data) => {
    try {
      const { threadId, isTyping } = data;

      if (isTyping) {
        await redisClient.setUserTyping(userId, threadId);
      } else {
        await redisClient.removeUserTyping(userId, threadId);
      }

      // Notify other participants in the thread
      socket.to(`thread:${threadId}`).emit('user_typing', {
        userId,
        threadId,
        isTyping
      });
    } catch (error) {
      logger.error('Error handling typing event:', error);
    }
  });

  // Handle sending message
  socket.on('send_message', async (data) => {
    try {
      const {
        threadId,
        receiverId,
        content,
        messageType,
        originalLanguage,
        isGroup,
        mediaUrls,
        encryptedContent
      } = data;

      // Save message to database
      const messageData = {
        thread_id: threadId || uuidv4(),
        sender_id: userId,
        receiver_id: receiverId,
        message_type: messageType || 'text',
        content,
        original_language: originalLanguage,
        encrypted_content: encryptedContent,
        status: 'sent',
        is_group: isGroup || false,
        media_urls: mediaUrls || [],
        metadata: {
          timestamp: Date.now().toString()
        }
      };

      await db.saveMessage(messageData);

      // Update thread for both sender and receiver
      const threadData = {
        thread_id: threadId,
        participant_id: userId,
        other_participant_id: receiverId,
        thread_type: isGroup ? 'group' : 'direct',
        last_message_time: new Date(),
        last_message_preview: content.substring(0, 100)
      };

      await db.createOrUpdateThread(threadData);

      // Update thread for receiver
      await db.createOrUpdateThread({
        ...threadData,
        participant_id: receiverId,
        other_participant_id: userId,
        unread_count: 1
      });

      // Prepare message response
      const messageResponse = {
        messageId: uuidv4(),
        threadId,
        senderId: userId,
        receiverId,
        content,
        messageType,
        originalLanguage,
        timestamp: Date.now(),
        status: 'sent'
      };

      // Send to receiver if online
      io.to(`user:${receiverId}`).emit('new_message', messageResponse);

      // Send delivery confirmation to sender
      socket.emit('message_sent', {
        ...messageResponse,
        status: 'delivered'
      });

      // Get receiver's preferred language for this thread
      const threadSettings = await getThreadSettings(receiverId, threadId);

      // If translation needed, request translation
      if (threadSettings && threadSettings.preferred_language !== originalLanguage) {
        try {
          const translatedMessage = await translateMessage(
            content,
            originalLanguage,
            threadSettings.preferred_language
          );

          // Send translated version to receiver
          io.to(`user:${receiverId}`).emit('message_translated', {
            messageId: messageResponse.messageId,
            translatedContent: translatedMessage.translated_text,
            targetLanguage: threadSettings.preferred_language,
            emotion: translatedMessage.emotion
          });
        } catch (error) {
          logger.error('Translation error:', error);
        }
      }

      logger.info(`Message sent from ${userId} to ${receiverId}`);
    } catch (error) {
      logger.error('Error sending message:', error);
      socket.emit('message_error', { error: 'Failed to send message' });
    }
  });

  // Handle message read receipt
  socket.on('mark_read', async (data) => {
    try {
      const { threadId, messageId } = data;

      await db.updateMessageStatus(threadId, messageId, 'read');

      // Notify sender
      const messages = await db.getMessages(threadId, 1);
      if (messages.rows.length > 0) {
        const message = messages.rows[0];
        io.to(`user:${message.sender_id}`).emit('message_read', {
          threadId,
          messageId,
          readBy: userId
        });
      }
    } catch (error) {
      logger.error('Error marking message as read:', error);
    }
  });

  // Handle joining thread
  socket.on('join_thread', (data) => {
    const { threadId } = data;
    socket.join(`thread:${threadId}`);
    logger.info(`User ${userId} joined thread ${threadId}`);
  });

  // Handle leaving thread
  socket.on('leave_thread', (data) => {
    const { threadId } = data;
    socket.leave(`thread:${threadId}`);
    logger.info(`User ${userId} left thread ${threadId}`);
  });

  // Handle disconnect
  socket.on('disconnect', () => {
    logger.info(`User disconnected: ${userId}`);
    activeConnections.delete(userId);
    updateUserPresence(userId, 'offline');
  });
});

// Helper functions
async function updateUserPresence(userId, status) {
  try {
    if (status === 'online') {
      await redisClient.setUserOnline(userId);
    } else {
      await redisClient.setUserOffline(userId);
    }

    // Broadcast presence update to user's contacts
    io.emit('presence_update', {
      userId,
      status,
      timestamp: Date.now()
    });
  } catch (error) {
    logger.error('Error updating presence:', error);
  }
}

async function getThreadSettings(userId, threadId) {
  try {
    const cached = await redisClient.cacheGet(`thread_settings:${userId}:${threadId}`);
    if (cached) return cached;

    // Fetch from database
    const query = 'SELECT * FROM thread_settings WHERE user_id = ? AND thread_id = ?';
    const result = await db.client.execute(query, [userId, threadId], { prepare: true });

    if (result.rows.length > 0) {
      const settings = result.rows[0];
      await redisClient.cacheSet(`thread_settings:${userId}:${threadId}`, settings, 3600);
      return settings;
    }

    return null;
  } catch (error) {
    logger.error('Error getting thread settings:', error);
    return null;
  }
}

async function translateMessage(text, sourceLang, targetLang) {
  try {
    const response = await axios.post(
      `${process.env.TRANSLATION_SERVICE_URL || 'http://localhost:3004'}/translate`,
      {
        text,
        source_language: sourceLang,
        target_language: targetLang
      }
    );

    return response.data;
  } catch (error) {
    logger.error('Translation request failed:', error);
    throw error;
  }
}

// REST endpoints
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    service: 'chat',
    activeConnections: activeConnections.size
  });
});

// Get chat threads for user
app.get('/threads', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];

    if (!userId) {
      return res.status(401).json({ error: 'User ID required' });
    }

    const threads = await db.getThreadsForUser(userId);

    res.json({
      success: true,
      threads
    });
  } catch (error) {
    logger.error('Error fetching threads:', error);
    res.status(500).json({ error: 'Failed to fetch threads' });
  }
});

// Get messages for thread
app.get('/threads/:threadId/messages', async (req, res) => {
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

// Update thread settings
app.post('/threads/:threadId/settings', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];
    const { threadId } = req.params;
    const { preferredLanguage, showOriginal, enableEmotionDetection } = req.body;

    const query = `
      INSERT INTO thread_settings (user_id, thread_id, preferred_language,
                                   show_original, enable_emotion_detection)
      VALUES (?, ?, ?, ?, ?)
    `;

    await db.client.execute(
      query,
      [userId, threadId, preferredLanguage, showOriginal, enableEmotionDetection],
      { prepare: true }
    );

    // Clear cache
    await redisClient.cacheDel(`thread_settings:${userId}:${threadId}`);

    res.json({ success: true });
  } catch (error) {
    logger.error('Error updating thread settings:', error);
    res.status(500).json({ error: 'Failed to update settings' });
  }
});

// Start server
server.listen(PORT, () => {
  logger.info(`Chat service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  io.close(() => {
    server.close(async () => {
      await db.close();
      await redisClient.close();
      logger.info('Chat service shut down gracefully');
      process.exit(0);
    });
  });
});

module.exports = { app, io };
