# Tolkflip - Quick Start Guide

Welcome to Tolkflip! This guide will help you get the application running in minutes.

## What You Just Got

A production-ready, multilingual chat application with:

### ✅ **8 Backend Microservices**
1. **API Gateway** - Routes all requests, handles authentication
2. **Auth Service** - Phone number verification via SMS
3. **Chat Service** - Real-time WebSocket messaging
4. **Translation Service** - 80+ languages using MarianNMT
5. **Transcription Service** - Voice-to-text using Whisper AI
6. **Media Service** - Image/video/file storage
7. **Presence Service** - Online status and typing indicators
8. **User Service** - Profile management

### ✅ **Mobile Applications**
- **Android** (Java) - Full-featured native app
- **iOS** (Swift) - Full-featured native app

### ✅ **Infrastructure**
- Docker Compose for local development
- Kubernetes manifests for production
- Cassandra (scalable NoSQL database)
- Redis (caching and real-time data)
- MinIO (object storage)
- Prometheus + Grafana (monitoring)

### ✅ **CI/CD Pipelines**
- GitHub Actions for automated testing
- Docker image building and deployment
- Mobile app deployment workflows

## 🚀 Start in 3 Minutes

### Step 1: Clone and Configure

```bash
cd tolkflip

# Copy environment template
cp .env.example .env

# Edit .env with your Twilio credentials (for SMS)
# You can skip Twilio for local testing - it will mock SMS
nano .env
```

### Step 2: Start Everything

```bash
# Start all services with Docker Compose
docker-compose up -d

# Watch the logs
docker-compose logs -f
```

Wait 30-60 seconds for Cassandra to initialize.

### Step 3: Test the API

```bash
# Check API health
curl http://localhost:3000/health

# Request verification code (will be logged if Twilio not configured)
curl -X POST http://localhost:3000/api/auth/request-code \
  -H "Content-Type: application/json" \
  -d '{"phone_number": "+1234567890"}'

# Check logs for verification code if Twilio not configured
docker-compose logs auth-service | grep "verification code"
```

### Step 4: Access Services

- **API Gateway**: http://localhost:3000
- **Prometheus**: http://localhost:9090
- **Grafana**: http://localhost:3001 (login: admin/admin)
- **MinIO Console**: http://localhost:9001 (login: minioadmin/minioadmin)

## 📱 Test Mobile Apps

### Android

```bash
cd mobile/android

# Build the app
./gradlew build

# Install on device/emulator
./gradlew installDebug

# Or open in Android Studio
```

Update `mobile/android/app/build.gradle` to point to your local backend:
```gradle
buildConfigField "String", "API_BASE_URL", "\"http://YOUR_LOCAL_IP:3000\""
```

### iOS

```bash
cd mobile/ios

# Install dependencies
pod install

# Open in Xcode
open Tolkflip.xcworkspace
```

Update the API URL in `APIService.swift` for local testing.

## 🔑 Key Features to Test

### 1. Phone Authentication
- Request code → Verify code → Get JWT token
- Works without Twilio (check logs for code)

### 2. Real-Time Chat
```javascript
// Connect via WebSocket
const socket = io('ws://localhost:3003', {
  auth: { token: 'YOUR_JWT_TOKEN' }
});

// Send message
socket.emit('send_message', {
  threadId: 'thread-uuid',
  receiverId: 'user-uuid',
  content: 'Hello!',
  messageType: 'text',
  originalLanguage: 'en'
});
```

### 3. Translation
```bash
curl -X POST http://localhost:3000/api/translate \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "text": "Hello, how are you?",
    "source_language": "en",
    "target_language": "es",
    "detect_emotion": true
  }'
```

### 4. Voice Transcription
```bash
curl -X POST http://localhost:3000/api/transcribe \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -F "audio=@voice.mp3" \
  -F "translate_to=en"
```

### 5. Media Upload
```bash
curl -X POST http://localhost:3000/api/media/upload \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -F "file=@image.jpg" \
  -F "thread_id=thread-uuid" \
  -F "message_id=msg-uuid"
```

## 🌍 Supported Languages

**All 80+ languages are ready to use!**

**European**: English, German, French, Spanish, Italian, Portuguese, Dutch, Polish, Czech, Slovak, Romanian, Hungarian, Swedish, Danish, Finnish, Norwegian, Greek, Bulgarian, Croatian, Serbian, and more...

**Asian**: Chinese, Japanese, Korean, Hindi, Bengali, Tamil, Telugu, Marathi, Gujarati, Kannada, Malayalam, Punjabi, Urdu, Vietnamese, Thai, Indonesian, Malay

**Others**: Arabic, Hebrew, Persian, Turkish, Russian, Ukrainian, Belarusian

## 📊 Monitor Your Application

### Prometheus Metrics
Visit http://localhost:9090 and query:
- `http_request_duration_seconds` - API latency
- `websocket_connections_total` - Active connections
- `translation_requests_total` - Translation usage

### Grafana Dashboards
1. Visit http://localhost:3001
2. Login: admin/admin
3. Add Prometheus datasource: http://prometheus:9090
4. Import pre-built dashboards from `infrastructure/monitoring/grafana/`

## 🔧 Common Operations

### View Logs
```bash
# All services
docker-compose logs -f

# Specific service
docker-compose logs -f chat-service
docker-compose logs -f translation-service
```

### Scale Services
```bash
# Scale chat service to 3 instances
docker-compose up -d --scale chat-service=3

# Scale translation service
docker-compose up -d --scale translation-service=2
```

### Database Access

**Cassandra:**
```bash
docker exec -it tolkflip-cassandra cqlsh

# View keyspaces
DESCRIBE KEYSPACES;

# Use tolkflip keyspace
USE tolkflip;

# View tables
DESCRIBE TABLES;

# Query users
SELECT * FROM users LIMIT 10;

# Query messages
SELECT * FROM messages WHERE thread_id = <uuid> LIMIT 20;
```

**Redis:**
```bash
docker exec -it tolkflip-redis redis-cli

# View all keys
KEYS *

# Get user presence
GET presence:user-id

# View online users
SMEMBERS online_users
```

### Restart Services
```bash
# Restart all
docker-compose restart

# Restart specific service
docker-compose restart chat-service
```

### Stop Everything
```bash
docker-compose down

# Stop and remove volumes (fresh start)
docker-compose down -v
```

## 🚀 Deploy to Production

See [DEPLOYMENT.md](./DEPLOYMENT.md) for:
- Kubernetes deployment
- Cloud provider setup (AWS, GCP, Azure)
- SSL/TLS configuration
- Domain setup
- Production security checklist
- Scaling strategies
- Monitoring setup

### Quick Production Deploy (Kubernetes)

```bash
# Create cluster (AWS EKS example)
eksctl create cluster --name tolkflip --region us-east-1

# Create secrets
kubectl create secret generic tolkflip-secrets \
  --from-literal=JWT_SECRET=your-secret \
  --from-literal=TWILIO_ACCOUNT_SID=your-sid \
  --from-literal=TWILIO_AUTH_TOKEN=your-token

# Deploy
kubectl apply -f infrastructure/kubernetes/

# Check status
kubectl get pods -n tolkflip
```

## 🐛 Troubleshooting

### Cassandra won't start
```bash
# Check logs
docker logs tolkflip-cassandra

# Increase memory in docker-compose.yml
# Then restart
docker-compose down && docker-compose up -d
```

### Translation is slow
```bash
# First run loads ML models (2-3 minutes)
# Check if GPU is available
docker exec tolkflip-translation-service python -c "import torch; print(torch.cuda.is_available())"

# Scale up
docker-compose up -d --scale translation-service=3
```

### WebSocket connections fail
```bash
# Check Redis is running
docker-compose ps redis

# Check chat service logs
docker-compose logs chat-service

# Verify JWT token is valid
```

### Can't upload media
```bash
# Check MinIO is running
docker-compose ps minio

# Access MinIO console
# http://localhost:9001 (minioadmin/minioadmin)

# Check buckets exist
```

## 📚 Next Steps

1. **Read Full Documentation**
   - [README.md](./README.md) - Overview and features
   - [ARCHITECTURE.md](./ARCHITECTURE.md) - Technical details
   - [DEPLOYMENT.md](./DEPLOYMENT.md) - Production deployment

2. **Customize the App**
   - Add your branding to mobile apps
   - Configure Twilio for real SMS
   - Set up Firebase for push notifications
   - Add custom languages or features

3. **Production Checklist**
   - Change all secrets in `.env`
   - Set up SSL/TLS
   - Configure firewall rules
   - Enable monitoring alerts
   - Set up backups
   - Load test your deployment

4. **Get Help**
   - GitHub Issues: Report bugs or request features
   - Documentation: Read the comprehensive guides
   - Community: Join our Discord (if available)

## 🎉 You're Ready!

You now have a fully functional multilingual chat application running locally. The system is designed to scale to millions of users and supports 80+ languages out of the box.

**Key URLs to Remember:**
- API: http://localhost:3000
- Monitoring: http://localhost:9090
- Dashboards: http://localhost:3001
- Storage: http://localhost:9001

Happy chatting across languages! 🌍💬
