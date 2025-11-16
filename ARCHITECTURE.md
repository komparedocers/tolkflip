# Tolkflip - Detailed Architecture Documentation

## System Overview

Tolkflip is a multilingual chat application built using a microservices architecture pattern. The system is designed to be:
- **Scalable**: Handles millions of concurrent users
- **Cloud-agnostic**: Can be deployed on any cloud provider or on-premises
- **Resilient**: Fault-tolerant with automatic failover
- **Secure**: End-to-end encryption using Signal Protocol
- **Real-time**: WebSocket-based messaging with sub-second latency

## Core Components

### 1. API Gateway (Port 3000)

**Technology**: Node.js, Express, http-proxy-middleware

**Responsibilities**:
- Single entry point for all client requests
- JWT token verification
- Rate limiting (100 requests per 15 minutes per IP)
- Request routing to microservices
- Load balancing
- CORS handling

**Key Features**:
- Automatic service discovery
- Health check aggregation
- Request/response logging
- Error handling and standardization

### 2. Authentication Service (Port 3001)

**Technology**: Node.js, Twilio, JWT, Cassandra, Redis

**Responsibilities**:
- Phone number-based authentication
- SMS verification code generation and validation
- JWT token generation (access + refresh tokens)
- User registration
- Session management

**Authentication Flow**:
1. User enters phone number
2. Service generates 6-digit code, stores in Redis (10-min TTL)
3. Code sent via Twilio SMS
4. User enters code
5. Service validates code (max 3 attempts)
6. On success: creates/fetches user, generates JWT tokens
7. Tokens returned to client

**Security**:
- bcrypt password hashing (not used for phone auth, but available)
- JWT with 30-day access token, 90-day refresh token
- Refresh tokens stored in Redis for revocation
- Rate limiting on verification attempts

### 3. Chat Service (Port 3003)

**Technology**: Node.js, Socket.IO, Cassandra, Redis

**Responsibilities**:
- Real-time WebSocket messaging
- Message persistence
- Message delivery confirmation
- Read receipts
- Typing indicators
- Thread management

**WebSocket Events**:
- `send_message`: Send new message
- `new_message`: Receive message
- `message_sent`: Delivery confirmation
- `message_read`: Read receipt
- `typing`: Typing indicator
- `join_thread`: Join conversation
- `leave_thread`: Leave conversation

**Message Flow**:
1. Client sends message via WebSocket
2. Service validates and encrypts (if needed)
3. Saves to Cassandra
4. Updates thread metadata
5. Checks receiver's language preference
6. If translation needed, calls Translation Service
7. Delivers message to receiver via WebSocket
8. Sends translated version if applicable
9. Confirms delivery to sender

**Scalability**:
- Horizontal scaling with sticky sessions
- Redis pub/sub for cross-instance messaging
- WebSocket connection pooling
- Message queue for offline delivery

### 4. Translation Service (Port 3004)

**Technology**: Python, Flask, MarianNMT, TextBlob, Redis, Cassandra

**Responsibilities**:
- Text translation between 80+ languages
- Language auto-detection
- Emotion/sentiment analysis
- Translation caching

**Translation Pipeline**:
1. Receive text and language pair
2. Check Redis cache
3. If cache miss, load MarianMT model
4. Perform translation
5. Detect emotion using TextBlob
6. Cache result (1-hour TTL in Redis, 30-day TTL in Cassandra)
7. Return translated text + confidence + emotion

**Models**:
- Helsinki-NLP MarianMT models
- Separate models for each language pair
- Model caching in memory
- Lazy loading of models

**Performance**:
- GPU acceleration when available
- Batch translation support
- ~500ms average translation time
- Model warm-up on service start

### 5. Transcription Service (Port 3005)

**Technology**: Python, Flask, OpenAI Whisper, FFmpeg

**Responsibilities**:
- Audio-to-text transcription
- Language detection from audio
- Optional translation of transcription
- Support for voice notes and audio messages

**Transcription Pipeline**:
1. Receive audio file (multipart upload)
2. Save to temporary file
3. Process with Whisper model
4. Extract text + detected language + timestamps
5. If translation requested, call Translation Service
6. Return transcription + optional translation + emotion
7. Clean up temporary file

**Supported Formats**:
- MP3, WAV, M4A, OGG, FLAC
- Automatic format conversion via FFmpeg

**Model Sizes**:
- `tiny`: Fastest, less accurate
- `base`: Balanced (default)
- `small`: More accurate
- `medium`: High accuracy
- `large`: Best accuracy, slowest

### 6. Media Service (Port 3006)

**Technology**: Node.js, MinIO, Sharp, Cassandra, Redis

**Responsibilities**:
- File upload handling
- Image/video storage
- Thumbnail generation
- Presigned URL generation
- Media metadata management

**Upload Flow**:
1. Receive file (max 100MB)
2. Validate mime type
3. Generate unique media ID
4. Upload to MinIO bucket (images/videos/audio/files)
5. Generate thumbnail (for images)
6. Store metadata in Cassandra
7. Cache metadata in Redis
8. Return presigned URL (24-hour expiration)

**Storage Organization**:
```
Buckets:
- tolkflip-images
- tolkflip-videos
- tolkflip-audio
- tolkflip-files
- tolkflip-thumbnails

Path structure:
/{userId}/{timestamp}_{mediaId}.{ext}
```

**Thumbnail Generation**:
- 200x200 pixels
- Maintained aspect ratio
- JPEG format, 80% quality

### 7. Presence Service (Port 3007)

**Technology**: Node.js, Redis

**Responsibilities**:
- User online/offline status
- Last seen tracking
- Typing indicators
- Active user counting

**Presence Tracking**:
- Online status stored in Redis with 5-min TTL
- Heartbeat from clients every 2 minutes
- Typing indicators with 10-sec TTL
- Pub/sub for presence updates

### 8. User Service (Port 3002)

**Technology**: Node.js, Cassandra, Redis

**Responsibilities**:
- User profile management
- Language preference management
- Contact list management
- Device token management (push notifications)

**User Profile**:
```javascript
{
  userId: UUID,
  phoneNumber: String,
  displayName: String,
  avatarUrl: String,
  primaryLanguage: String,
  additionalLanguages: [String], // Max 2
  createdAt: Timestamp,
  lastActive: Timestamp
}
```

## Data Models

### Cassandra Schema

**users** table:
```cql
CREATE TABLE users (
  user_id uuid PRIMARY KEY,
  phone_number text,
  display_name text,
  avatar_url text,
  primary_language text,
  additional_languages list<text>,
  created_at timestamp,
  last_active timestamp,
  device_tokens list<text>,
  public_key text
);
CREATE INDEX ON users (phone_number);
```

**messages** table:
```cql
CREATE TABLE messages (
  thread_id uuid,
  message_id timeuuid,
  sender_id uuid,
  receiver_id uuid,
  message_type text,
  content text,
  original_language text,
  encrypted_content blob,
  timestamp timestamp,
  status text,
  is_group boolean,
  media_urls list<text>,
  metadata map<text, text>,
  PRIMARY KEY (thread_id, message_id)
) WITH CLUSTERING ORDER BY (message_id DESC);
```

**chat_threads** table:
```cql
CREATE TABLE chat_threads (
  thread_id uuid,
  participant_id uuid,
  other_participant_id uuid,
  thread_type text,
  created_at timestamp,
  last_message_time timestamp,
  last_message_preview text,
  unread_count int,
  is_archived boolean,
  PRIMARY KEY (participant_id, last_message_time, thread_id)
) WITH CLUSTERING ORDER BY (last_message_time DESC, thread_id ASC);
```

## Security Architecture

### End-to-End Encryption

**Signal Protocol Implementation**:
1. Each user has RSA key pair (2048-bit)
2. Public keys stored in Redis/Cassandra
3. Private keys stored securely on client
4. Session keys for each conversation
5. AES-256-GCM for message encryption

**Encryption Flow**:
1. Generate session key (256-bit random)
2. Encrypt message with session key (AES-GCM)
3. Encrypt session key with recipient's public key (RSA)
4. Send encrypted message + encrypted session key
5. Recipient decrypts session key with private key
6. Recipient decrypts message with session key

### Authentication

- JWT-based authentication
- Access token: 30 days
- Refresh token: 90 days
- Tokens signed with HS256
- Refresh tokens stored in Redis for revocation

## Scaling Strategy

### Horizontal Scaling

**Stateless Services** (can scale freely):
- API Gateway
- Auth Service
- User Service
- Translation Service
- Transcription Service
- Media Service

**Stateful Services** (need session affinity):
- Chat Service (WebSocket connections)
  - Use session affinity/sticky sessions
  - Redis pub/sub for cross-instance communication

### Database Scaling

**Cassandra**:
- Replication factor: 3
- Consistency level: QUORUM
- Add nodes for capacity
- Use NetworkTopologyStrategy for multi-DC

**Redis**:
- Redis Cluster for horizontal scaling
- Redis Sentinel for high availability
- Master-replica setup

### Caching Strategy

**Multi-Level Caching**:
1. **Client Cache**: Recent messages, user profiles
2. **Redis Cache**: Translations, user presence, sessions
3. **Application Cache**: Loaded ML models, configs
4. **Database Cache**: Cassandra row cache

**Cache Invalidation**:
- TTL-based expiration
- Event-based invalidation
- Write-through for critical data

## Performance Optimizations

### Database Optimizations

**Cassandra**:
- Partition by thread_id for messages (hot partition management)
- Secondary indices on frequently queried fields
- Compaction strategy: LeveledCompactionStrategy
- Bloom filters for faster lookups

**Redis**:
- Pipeline commands for bulk operations
- Lua scripts for atomic operations
- maxmemory-policy: allkeys-lru

### API Optimizations

- Response compression (gzip)
- Connection pooling
- Request batching
- Lazy loading for chat history
- Pagination for large result sets

### Translation Optimizations

- Aggressive caching (1-hour Redis, 30-day Cassandra)
- Model preloading for common language pairs
- Batch translation API
- GPU acceleration

## Monitoring and Observability

### Metrics (Prometheus)

**Application Metrics**:
- Request rate, latency, error rate
- WebSocket connections (active, total)
- Message throughput
- Translation requests

**System Metrics**:
- CPU, memory, disk usage
- Network I/O
- Database connection pool
- Cache hit/miss ratio

**Business Metrics**:
- Active users
- Messages per second
- Most used languages
- Translation accuracy

### Logging

**Structured Logging**:
- JSON format
- Correlation IDs for request tracking
- Log levels: ERROR, WARN, INFO, DEBUG
- Centralized logging (ELK stack optional)

### Tracing

- Distributed tracing with OpenTelemetry (optional)
- Request flow visualization
- Latency breakdown by service

## Disaster Recovery

### Backup Strategy

**Cassandra**:
- Daily snapshots
- Incremental backups
- Off-site replication
- Point-in-time recovery

**Redis**:
- RDB snapshots every 6 hours
- AOF for durability
- Replica for failover

**MinIO**:
- Versioning enabled
- Replication to secondary region
- Lifecycle policies for old media

### High Availability

- Multi-AZ deployment
- Load balancer health checks
- Automatic failover
- Circuit breakers
- Retry logic with exponential backoff

## Cost Optimization

- Auto-scaling based on metrics
- Spot instances for non-critical workloads
- Storage tiering (hot/cold data)
- CDN for media delivery
- Compression for data transfer
- Reserved instances for stable workloads

## Future Enhancements

1. **Video Calling**: WebRTC integration
2. **Voice Cloning**: Multilingual voice synthesis
3. **Smart Replies**: AI-powered suggestions
4. **Message Search**: Elasticsearch integration
5. **Analytics Dashboard**: User insights
6. **A/B Testing**: Feature experimentation
7. **Rate Limiting**: Per-user quotas
8. **Content Moderation**: AI-powered filtering

---

This architecture is designed to be production-ready, scalable, and maintainable. All components follow microservices best practices and can be independently developed, tested, and deployed.
