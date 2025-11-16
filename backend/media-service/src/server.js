// Media Service - File and media storage
require('dotenv').config();
const express = require('express');
const multer = require('multer');
const Minio = require('minio');
const sharp = require('sharp');
const { v4: uuidv4 } = require('uuid');
const winston = require('winston');
const mime = require('mime-types');

// Import shared modules
const DatabaseModels = require('../../shared/models/database');
const RedisClient = require('../../shared/utils/redis-client');
const encryption = require('../../shared/utils/encryption');

// Logger
const logger = winston.createLogger({
  level: 'info',
  format: winston.format.json(),
  transports: [
    new winston.transports.File({ filename: 'media-error.log', level: 'error' }),
    new winston.transports.File({ filename: 'media-combined.log' }),
    new winston.transports.Console({ format: winston.format.simple() })
  ]
});

const app = express();
const PORT = process.env.PORT || 3006;

// Database and Redis initialization
const db = new DatabaseModels(
  process.env.CASSANDRA_HOSTS?.split(',') || ['127.0.0.1'],
  'tolkflip'
);
const redisClient = new RedisClient();

// MinIO client configuration
const minioClient = new Minio.Client({
  endPoint: process.env.MINIO_ENDPOINT || 'localhost',
  port: parseInt(process.env.MINIO_PORT || '9000'),
  useSSL: process.env.MINIO_USE_SSL === 'true',
  accessKey: process.env.MINIO_ACCESS_KEY || 'minioadmin',
  secretKey: process.env.MINIO_SECRET_KEY || 'minioadmin'
});

// Buckets
const BUCKETS = {
  images: 'tolkflip-images',
  videos: 'tolkflip-videos',
  audio: 'tolkflip-audio',
  files: 'tolkflip-files',
  thumbnails: 'tolkflip-thumbnails'
};

// Multer configuration for file upload
const upload = multer({
  storage: multer.memoryStorage(),
  limits: {
    fileSize: 100 * 1024 * 1024 // 100MB limit
  }
});

// Initialize MinIO buckets
async function initializeBuckets() {
  for (const [key, bucketName] of Object.entries(BUCKETS)) {
    try {
      const exists = await minioClient.bucketExists(bucketName);
      if (!exists) {
        await minioClient.makeBucket(bucketName);
        logger.info(`Bucket created: ${bucketName}`);
      }
    } catch (error) {
      logger.error(`Error creating bucket ${bucketName}:`, error);
    }
  }
}

// Initialize connections
async function initialize() {
  try {
    await db.connect();
    await redisClient.connect();
    await initializeBuckets();
    logger.info('Media service initialized successfully');
  } catch (error) {
    logger.error('Failed to initialize media service:', error);
    process.exit(1);
  }
}

initialize();

// Middleware
app.use(express.json());

// Determine bucket based on mime type
function getBucketForMimeType(mimeType) {
  if (mimeType.startsWith('image/')) return BUCKETS.images;
  if (mimeType.startsWith('video/')) return BUCKETS.videos;
  if (mimeType.startsWith('audio/')) return BUCKETS.audio;
  return BUCKETS.files;
}

// Generate thumbnail for image
async function generateThumbnail(imageBuffer) {
  return await sharp(imageBuffer)
    .resize(200, 200, {
      fit: 'inside',
      withoutEnlargement: true
    })
    .jpeg({ quality: 80 })
    .toBuffer();
}

// Upload file endpoint
app.post('/upload', upload.single('file'), async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];
    const threadId = req.body.thread_id;
    const messageId = req.body.message_id;

    if (!userId) {
      return res.status(401).json({ error: 'User ID required' });
    }

    if (!req.file) {
      return res.status(400).json({ error: 'No file uploaded' });
    }

    const mediaId = uuidv4();
    const mimeType = req.file.mimetype;
    const originalName = req.file.originalname;
    const fileExtension = mime.extension(mimeType) || 'bin';
    const bucket = getBucketForMimeType(mimeType);

    // Generate unique file name
    const fileName = `${userId}/${Date.now()}_${mediaId}.${fileExtension}`;

    // Upload to MinIO
    await minioClient.putObject(
      bucket,
      fileName,
      req.file.buffer,
      req.file.size,
      {
        'Content-Type': mimeType,
        'X-Original-Name': originalName
      }
    );

    logger.info(`File uploaded: ${fileName} to bucket ${bucket}`);

    // Generate thumbnail for images
    let thumbnailPath = null;
    if (mimeType.startsWith('image/')) {
      try {
        const thumbnail = await generateThumbnail(req.file.buffer);
        const thumbnailName = `${userId}/${Date.now()}_${mediaId}_thumb.jpg`;

        await minioClient.putObject(
          BUCKETS.thumbnails,
          thumbnailName,
          thumbnail,
          thumbnail.length,
          { 'Content-Type': 'image/jpeg' }
        );

        thumbnailPath = thumbnailName;
        logger.info(`Thumbnail generated: ${thumbnailName}`);
      } catch (thumbError) {
        logger.error('Error generating thumbnail:', thumbError);
      }
    }

    // Save metadata to Cassandra
    const query = `
      INSERT INTO media_metadata (media_id, thread_id, message_id, uploader_id,
                                 media_type, file_size, mime_type, storage_path,
                                 thumbnail_path, uploaded_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, toTimestamp(now()))
    `;

    await db.client.execute(
      query,
      [
        mediaId,
        threadId || null,
        messageId || null,
        userId,
        mimeType.split('/')[0],
        req.file.size,
        mimeType,
        fileName,
        thumbnailPath
      ],
      { prepare: true }
    );

    // Cache metadata in Redis
    await redisClient.cacheSet(
      `media:${mediaId}`,
      {
        mediaId,
        bucket,
        fileName,
        mimeType,
        thumbnailPath
      },
      3600
    );

    // Generate presigned URL
    const url = await minioClient.presignedGetObject(bucket, fileName, 24 * 60 * 60);
    let thumbnailUrl = null;

    if (thumbnailPath) {
      thumbnailUrl = await minioClient.presignedGetObject(
        BUCKETS.thumbnails,
        thumbnailPath,
        24 * 60 * 60
      );
    }

    res.json({
      success: true,
      mediaId,
      url,
      thumbnailUrl,
      mimeType,
      size: req.file.size
    });

  } catch (error) {
    logger.error('Upload error:', error);
    res.status(500).json({ error: 'Upload failed' });
  }
});

// Get media URL
app.get('/media/:mediaId', async (req, res) => {
  try {
    const { mediaId } = req.params;
    const thumbnail = req.query.thumbnail === 'true';

    // Check cache
    let metadata = await redisClient.cacheGet(`media:${mediaId}`);

    if (!metadata) {
      // Fetch from database
      const query = 'SELECT * FROM media_metadata WHERE media_id = ?';
      const result = await db.client.execute(query, [mediaId], { prepare: true });

      if (result.rows.length === 0) {
        return res.status(404).json({ error: 'Media not found' });
      }

      const row = result.rows[0];
      metadata = {
        mediaId: row.media_id,
        bucket: getBucketForMimeType(row.mime_type),
        fileName: row.storage_path,
        mimeType: row.mime_type,
        thumbnailPath: row.thumbnail_path
      };

      // Cache it
      await redisClient.cacheSet(`media:${mediaId}`, metadata, 3600);
    }

    // Generate presigned URL
    const path = thumbnail && metadata.thumbnailPath
      ? metadata.thumbnailPath
      : metadata.fileName;

    const bucket = thumbnail && metadata.thumbnailPath
      ? BUCKETS.thumbnails
      : metadata.bucket;

    const url = await minioClient.presignedGetObject(bucket, path, 24 * 60 * 60);

    res.json({
      success: true,
      url,
      mediaId,
      mimeType: metadata.mimeType
    });

  } catch (error) {
    logger.error('Error getting media URL:', error);
    res.status(500).json({ error: 'Failed to get media URL' });
  }
});

// Delete media
app.delete('/media/:mediaId', async (req, res) => {
  try {
    const userId = req.headers['x-user-id'];
    const { mediaId } = req.params;

    // Get metadata
    const query = 'SELECT * FROM media_metadata WHERE media_id = ?';
    const result = await db.client.execute(query, [mediaId], { prepare: true });

    if (result.rows.length === 0) {
      return res.status(404).json({ error: 'Media not found' });
    }

    const metadata = result.rows[0];

    // Check ownership
    if (metadata.uploader_id.toString() !== userId) {
      return res.status(403).json({ error: 'Unauthorized' });
    }

    // Delete from MinIO
    const bucket = getBucketForMimeType(metadata.mime_type);
    await minioClient.removeObject(bucket, metadata.storage_path);

    if (metadata.thumbnail_path) {
      await minioClient.removeObject(BUCKETS.thumbnails, metadata.thumbnail_path);
    }

    // Delete from database
    const deleteQuery = 'DELETE FROM media_metadata WHERE media_id = ?';
    await db.client.execute(deleteQuery, [mediaId], { prepare: true });

    // Delete from cache
    await redisClient.cacheDel(`media:${mediaId}`);

    logger.info(`Media deleted: ${mediaId}`);

    res.json({ success: true });

  } catch (error) {
    logger.error('Error deleting media:', error);
    res.status(500).json({ error: 'Failed to delete media' });
  }
});

// Health check
app.get('/health', (req, res) => {
  res.json({ status: 'healthy', service: 'media' });
});

// Start server
const server = app.listen(PORT, () => {
  logger.info(`Media service listening on port ${PORT}`);
});

// Graceful shutdown
process.on('SIGTERM', async () => {
  logger.info('SIGTERM signal received');
  server.close(async () => {
    await db.close();
    await redisClient.close();
    logger.info('Media service shut down gracefully');
    process.exit(0);
  });
});

module.exports = app;
