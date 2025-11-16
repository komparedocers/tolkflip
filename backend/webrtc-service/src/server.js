const express = require('express');
const http = require('http');
const socketIo = require('socket.io');
const cors = require('cors');
const Redis = require('ioredis');
const jwt = require('jsonwebtoken');
const winston = require('winston');
const promClient = require('prom-client');

// Initialize Express app
const app = express();
const server = http.createServer(app);

// Configure logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.combine(
    winston.format.timestamp(),
    winston.format.json()
  ),
  transports: [
    new winston.transports.Console(),
    new winston.transports.File({ filename: 'webrtc-service.log' })
  ]
});

// Prometheus metrics
const register = new promClient.Registry();
promClient.collectDefaultMetrics({ register });

const callsTotal = new promClient.Counter({
  name: 'webrtc_calls_total',
  help: 'Total number of WebRTC calls initiated',
  labelNames: ['type']
});

const activeCallsGauge = new promClient.Gauge({
  name: 'webrtc_active_calls',
  help: 'Number of active WebRTC calls'
});

register.registerMetric(callsTotal);
register.registerMetric(activeCallsGauge);

// Middleware
app.use(cors());
app.use(express.json());

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'ok', service: 'webrtc-service' });
});

// Metrics endpoint
app.get('/metrics', async (req, res) => {
  res.set('Content-Type', register.contentType);
  res.end(await register.metrics());
});

// Redis client for storing active calls
const redisClient = new Redis({
  host: process.env.REDIS_HOST || 'localhost',
  port: process.env.REDIS_PORT || 6379,
  retryStrategy: (times) => {
    const delay = Math.min(times * 50, 2000);
    return delay;
  }
});

redisClient.on('connect', () => {
  logger.info('Connected to Redis');
});

redisClient.on('error', (err) => {
  logger.error('Redis error:', err);
});

// Socket.IO configuration
const io = socketIo(server, {
  cors: {
    origin: '*',
    methods: ['GET', 'POST']
  },
  transports: ['websocket', 'polling']
});

// Active calls map (callId -> {caller, callee, status, type})
const activeCalls = new Map();

// Socket authentication middleware
io.use((socket, next) => {
  const token = socket.handshake.auth.token || socket.handshake.query.token;

  if (!token) {
    return next(new Error('Authentication error: No token provided'));
  }

  try {
    const decoded = jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key');
    socket.userId = decoded.userId;
    socket.username = decoded.username || decoded.phone_number;
    next();
  } catch (err) {
    logger.error('Token verification failed:', err);
    next(new Error('Authentication error: Invalid token'));
  }
});

// Socket.IO connection handling
io.on('connection', (socket) => {
  logger.info(`User connected: ${socket.userId}`);

  // Join user to their personal room
  socket.join(`user:${socket.userId}`);

  // Store socket connection in Redis
  redisClient.hset('webrtc:online', socket.userId, socket.id);

  // Handle call initiation
  socket.on('call:initiate', async (data) => {
    const { calleeId, callType, threadId } = data; // callType: 'audio' | 'video'
    const callId = `call:${Date.now()}:${socket.userId}`;

    logger.info(`Call initiated: ${callId} from ${socket.userId} to ${calleeId}, type: ${callType}`);

    // Check if callee is online
    const calleeSocketId = await redisClient.hget('webrtc:online', calleeId);

    if (!calleeSocketId) {
      socket.emit('call:failed', {
        callId,
        reason: 'User offline',
        message: 'The user you are trying to call is currently offline'
      });
      return;
    }

    // Check if callee is already in a call
    const existingCall = Array.from(activeCalls.values()).find(
      call => (call.caller === calleeId || call.callee === calleeId) && call.status !== 'ended'
    );

    if (existingCall) {
      socket.emit('call:failed', {
        callId,
        reason: 'User busy',
        message: 'The user is currently in another call'
      });
      return;
    }

    // Create call record
    const callData = {
      callId,
      caller: socket.userId,
      callee: calleeId,
      callType,
      threadId,
      status: 'ringing',
      initiatedAt: new Date().toISOString()
    };

    activeCalls.set(callId, callData);
    activeCallsGauge.inc();
    callsTotal.inc({ type: callType });

    // Store in Redis with 1-hour expiry
    await redisClient.setex(`webrtc:call:${callId}`, 3600, JSON.stringify(callData));

    // Notify callee about incoming call
    io.to(`user:${calleeId}`).emit('call:incoming', {
      callId,
      callerId: socket.userId,
      callerName: socket.username,
      callType,
      threadId
    });

    // Confirm to caller
    socket.emit('call:initiated', { callId, status: 'ringing' });
  });

  // Handle call answer
  socket.on('call:answer', async (data) => {
    const { callId } = data;
    const call = activeCalls.get(callId);

    if (!call) {
      socket.emit('call:error', { message: 'Call not found' });
      return;
    }

    if (call.callee !== socket.userId) {
      socket.emit('call:error', { message: 'Unauthorized' });
      return;
    }

    logger.info(`Call answered: ${callId}`);

    // Update call status
    call.status = 'active';
    call.answeredAt = new Date().toISOString();
    activeCalls.set(callId, call);
    await redisClient.setex(`webrtc:call:${callId}`, 3600, JSON.stringify(call));

    // Notify caller
    io.to(`user:${call.caller}`).emit('call:answered', { callId });

    // Join both users to a call room
    socket.join(callId);
    io.sockets.sockets.get(await redisClient.hget('webrtc:online', call.caller))?.join(callId);
  });

  // Handle call rejection
  socket.on('call:reject', async (data) => {
    const { callId, reason } = data;
    const call = activeCalls.get(callId);

    if (!call) {
      return;
    }

    logger.info(`Call rejected: ${callId}, reason: ${reason || 'declined'}`);

    // Notify caller
    io.to(`user:${call.caller}`).emit('call:rejected', { callId, reason });

    // Clean up
    activeCalls.delete(callId);
    activeCallsGauge.dec();
    await redisClient.del(`webrtc:call:${callId}`);
  });

  // Handle call end
  socket.on('call:end', async (data) => {
    const { callId } = data;
    const call = activeCalls.get(callId);

    if (!call) {
      return;
    }

    logger.info(`Call ended: ${callId}`);

    // Calculate call duration
    const duration = call.answeredAt
      ? Math.floor((new Date() - new Date(call.answeredAt)) / 1000)
      : 0;

    call.status = 'ended';
    call.endedAt = new Date().toISOString();
    call.duration = duration;

    // Notify both parties
    io.to(callId).emit('call:ended', { callId, duration });

    // Clean up
    activeCalls.delete(callId);
    activeCallsGauge.dec();
    await redisClient.del(`webrtc:call:${callId}`);

    // Store call history in Redis (keep for 30 days)
    await redisClient.setex(
      `webrtc:history:${callId}`,
      30 * 24 * 60 * 60,
      JSON.stringify(call)
    );
  });

  // WebRTC Signaling: ICE Candidate
  socket.on('webrtc:ice-candidate', async (data) => {
    const { callId, candidate } = data;
    const call = activeCalls.get(callId);

    if (!call) {
      return;
    }

    // Forward ICE candidate to the other peer
    const targetUserId = call.caller === socket.userId ? call.callee : call.caller;
    io.to(`user:${targetUserId}`).emit('webrtc:ice-candidate', {
      callId,
      candidate,
      from: socket.userId
    });
  });

  // WebRTC Signaling: SDP Offer
  socket.on('webrtc:offer', async (data) => {
    const { callId, offer } = data;
    const call = activeCalls.get(callId);

    if (!call || call.caller !== socket.userId) {
      socket.emit('call:error', { message: 'Invalid call or unauthorized' });
      return;
    }

    logger.info(`WebRTC offer sent for call: ${callId}`);

    // Forward offer to callee
    io.to(`user:${call.callee}`).emit('webrtc:offer', {
      callId,
      offer,
      from: socket.userId
    });
  });

  // WebRTC Signaling: SDP Answer
  socket.on('webrtc:answer', async (data) => {
    const { callId, answer } = data;
    const call = activeCalls.get(callId);

    if (!call || call.callee !== socket.userId) {
      socket.emit('call:error', { message: 'Invalid call or unauthorized' });
      return;
    }

    logger.info(`WebRTC answer sent for call: ${callId}`);

    // Forward answer to caller
    io.to(`user:${call.caller}`).emit('webrtc:answer', {
      callId,
      answer,
      from: socket.userId
    });
  });

  // Handle disconnection
  socket.on('disconnect', async () => {
    logger.info(`User disconnected: ${socket.userId}`);

    // Remove from online list
    await redisClient.hdel('webrtc:online', socket.userId);

    // End any active calls
    for (const [callId, call] of activeCalls.entries()) {
      if (call.caller === socket.userId || call.callee === socket.userId) {
        const otherUserId = call.caller === socket.userId ? call.callee : call.caller;

        io.to(`user:${otherUserId}`).emit('call:ended', {
          callId,
          reason: 'User disconnected'
        });

        activeCalls.delete(callId);
        activeCallsGauge.dec();
        await redisClient.del(`webrtc:call:${callId}`);
      }
    }
  });
});

// REST API endpoints

// Get call history for a user
app.get('/api/calls/history/:userId', async (req, res) => {
  try {
    const { userId } = req.params;
    const { limit = 50, offset = 0 } = req.query;

    // Get call history keys from Redis
    const keys = await redisClient.keys(`webrtc:history:*`);
    const calls = [];

    for (const key of keys) {
      const callData = await redisClient.get(key);
      if (callData) {
        const call = JSON.parse(callData);
        if (call.caller === userId || call.callee === userId) {
          calls.push(call);
        }
      }
    }

    // Sort by initiated time (most recent first)
    calls.sort((a, b) => new Date(b.initiatedAt) - new Date(a.initiatedAt));

    // Apply pagination
    const paginatedCalls = calls.slice(offset, offset + parseInt(limit));

    res.json({
      calls: paginatedCalls,
      total: calls.length,
      limit: parseInt(limit),
      offset: parseInt(offset)
    });
  } catch (error) {
    logger.error('Error fetching call history:', error);
    res.status(500).json({ error: 'Failed to fetch call history' });
  }
});

// Get active calls
app.get('/api/calls/active', (req, res) => {
  const calls = Array.from(activeCalls.values());
  res.json({ calls, count: calls.length });
});

// Get STUN/TURN server configuration
app.get('/api/webrtc/ice-servers', (req, res) => {
  // In production, use dedicated STUN/TURN servers
  const iceServers = [
    { urls: 'stun:stun.l.google.com:19302' },
    { urls: 'stun:stun1.l.google.com:19302' },
    { urls: 'stun:stun2.l.google.com:19302' }
  ];

  // Add TURN servers if configured
  if (process.env.TURN_URL && process.env.TURN_USERNAME && process.env.TURN_CREDENTIAL) {
    iceServers.push({
      urls: process.env.TURN_URL,
      username: process.env.TURN_USERNAME,
      credential: process.env.TURN_CREDENTIAL
    });
  }

  res.json({ iceServers });
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received: closing HTTP server');

  // Notify all active call participants
  for (const [callId, call] of activeCalls.entries()) {
    io.to(callId).emit('call:ended', { callId, reason: 'Server shutdown' });
  }

  await redisClient.quit();
  server.close(() => {
    logger.info('HTTP server closed');
    process.exit(0);
  });
});

const PORT = process.env.PORT || 3010;
server.listen(PORT, () => {
  logger.info(`WebRTC Signaling Server running on port ${PORT}`);
});

module.exports = { app, server, io };
