// API Gateway - Main entry point
require('dotenv').config();
const express = require('express');
const helmet = require('helmet');
const cors = require('cors');
const compression = require('compression');
const { createProxyMiddleware } = require('http-proxy-middleware');
const rateLimit = require('express-rate-limit');
const jwt = require('jsonwebtoken');
const winston = require('winston');

// Logger configuration
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'error.log', level: 'error' }),
    new winston.transports.File({ filename: 'combined.log' }),
    new winston.transports.Console({
      format: winston.format.simple()
    })
  ]
});

const app = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(helmet());
app.use(cors({
  origin: process.env.ALLOWED_ORIGINS?.split(',') || '*',
  credentials: true
}));
app.use(compression());
app.use(express.json({ limit: '10mb' }));
app.use(express.urlencoded({ extended: true, limit: '10mb' }));

// Request logging
app.use((req, res, next) => {
  logger.info(`${req.method} ${req.path}`, {
    ip: req.ip,
    userAgent: req.get('user-agent')
  });
  next();
});

// Rate limiting
const limiter = rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 100, // limit each IP to 100 requests per windowMs
  message: 'Too many requests from this IP, please try again later.',
  standardHeaders: true,
  legacyHeaders: false
});

app.use('/api/', limiter);

// JWT verification middleware
const verifyToken = (req, res, next) => {
  const token = req.headers['authorization']?.split(' ')[1];

  if (!token) {
    return res.status(401).json({ error: 'No token provided' });
  }

  try {
    const decoded = jwt.verify(token, process.env.JWT_SECRET || 'your-secret-key');
    req.user = decoded;
    next();
  } catch (error) {
    logger.error('Token verification failed:', error);
    return res.status(401).json({ error: 'Invalid token' });
  }
};

// Service endpoints configuration
const services = {
  auth: process.env.AUTH_SERVICE_URL || 'http://localhost:3001',
  user: process.env.USER_SERVICE_URL || 'http://localhost:3002',
  chat: process.env.CHAT_SERVICE_URL || 'http://localhost:3003',
  translation: process.env.TRANSLATION_SERVICE_URL || 'http://localhost:3004',
  transcription: process.env.TRANSCRIPTION_SERVICE_URL || 'http://localhost:3005',
  media: process.env.MEDIA_SERVICE_URL || 'http://localhost:3006',
  presence: process.env.PRESENCE_SERVICE_URL || 'http://localhost:3007'
};

// Health check
app.get('/health', (req, res) => {
  res.json({
    status: 'healthy',
    timestamp: new Date().toISOString(),
    uptime: process.uptime(),
    services
  });
});

// Authentication routes (no token required)
app.use('/api/auth', createProxyMiddleware({
  target: services.auth,
  changeOrigin: true,
  pathRewrite: {
    '^/api/auth': '/'
  },
  onError: (err, req, res) => {
    logger.error('Auth service error:', err);
    res.status(503).json({ error: 'Authentication service unavailable' });
  }
}));

// Protected routes
app.use('/api/users', verifyToken, createProxyMiddleware({
  target: services.user,
  changeOrigin: true,
  pathRewrite: {
    '^/api/users': '/'
  },
  onProxyReq: (proxyReq, req) => {
    proxyReq.setHeader('X-User-Id', req.user.userId);
  },
  onError: (err, req, res) => {
    logger.error('User service error:', err);
    res.status(503).json({ error: 'User service unavailable' });
  }
}));

app.use('/api/chat', verifyToken, createProxyMiddleware({
  target: services.chat,
  changeOrigin: true,
  ws: true, // Enable WebSocket proxying
  pathRewrite: {
    '^/api/chat': '/'
  },
  onProxyReq: (proxyReq, req) => {
    proxyReq.setHeader('X-User-Id', req.user.userId);
  },
  onError: (err, req, res) => {
    logger.error('Chat service error:', err);
    res.status(503).json({ error: 'Chat service unavailable' });
  }
}));

app.use('/api/translate', verifyToken, createProxyMiddleware({
  target: services.translation,
  changeOrigin: true,
  pathRewrite: {
    '^/api/translate': '/'
  },
  onProxyReq: (proxyReq, req) => {
    proxyReq.setHeader('X-User-Id', req.user.userId);
  },
  onError: (err, req, res) => {
    logger.error('Translation service error:', err);
    res.status(503).json({ error: 'Translation service unavailable' });
  }
}));

app.use('/api/transcribe', verifyToken, createProxyMiddleware({
  target: services.transcription,
  changeOrigin: true,
  pathRewrite: {
    '^/api/transcribe': '/'
  },
  onProxyReq: (proxyReq, req) => {
    proxyReq.setHeader('X-User-Id', req.user.userId);
  },
  onError: (err, req, res) => {
    logger.error('Transcription service error:', err);
    res.status(503).json({ error: 'Transcription service unavailable' });
  }
}));

app.use('/api/media', verifyToken, createProxyMiddleware({
  target: services.media,
  changeOrigin: true,
  pathRewrite: {
    '^/api/media': '/'
  },
  onProxyReq: (proxyReq, req) => {
    proxyReq.setHeader('X-User-Id', req.user.userId);
  },
  onError: (err, req, res) => {
    logger.error('Media service error:', err);
    res.status(503).json({ error: 'Media service unavailable' });
  }
}));

app.use('/api/presence', verifyToken, createProxyMiddleware({
  target: services.presence,
  changeOrigin: true,
  pathRewrite: {
    '^/api/presence': '/'
  },
  onProxyReq: (proxyReq, req) => {
    proxyReq.setHeader('X-User-Id', req.user.userId);
  },
  onError: (err, req, res) => {
    logger.error('Presence service error:', err);
    res.status(503).json({ error: 'Presence service unavailable' });
  }
}));

// 404 handler
app.use((req, res) => {
  res.status(404).json({ error: 'Endpoint not found' });
});

// Error handler
app.use((err, req, res, next) => {
  logger.error('Unhandled error:', err);
  res.status(500).json({
    error: 'Internal server error',
    message: process.env.NODE_ENV === 'development' ? err.message : undefined
  });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`API Gateway listening on port ${PORT}`);
  logger.info('Service endpoints:', services);
});

// Graceful shutdown
process.on('SIGTERM', () => {
  logger.info('SIGTERM signal received: closing HTTP server');
  server.close(() => {
    logger.info('HTTP server closed');
    process.exit(0);
  });
});

module.exports = app;
