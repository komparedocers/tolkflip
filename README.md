# Tolkflip - Multilingual Chat Application

> Breaking language barriers through real-time translation and transcription

Tolkflip is a modern, scalable chat application that enables people from different cultures to communicate seamlessly through automatic message translation and voice transcription. Built with microservices architecture, it supports all major languages and provides end-to-end encryption for secure communication.

## Features

### Core Features
- 📱 **Cross-Platform** - Native Android (Java) and iOS (Swift) apps
- 🌍 **Multilingual** - Support for 80+ languages including European, Asian, Arabic, and more
- 🔄 **Real-Time Translation** - Automatic message translation using MarianNMT
- 🎤 **Voice Transcription** - Audio message transcription using OpenAI Whisper
- 🔒 **End-to-End Encryption** - Signal Protocol implementation
- 💬 **Real-Time Messaging** - WebSocket-based instant messaging
- 👥 **Group Chats** - Multilingual group conversations
- 📸 **Media Sharing** - Images, videos, audio, and files
- 🎭 **Emotion Detection** - AI-powered sentiment analysis
- 👀 **Show Original** - Toggle between original and translated messages

### Unique Features
- **Per-Thread Language Settings** - Choose different languages for different conversations
- **Free Language Limit** - Native language + 2 additional languages free
- **Typing Indicators** - See when others are typing
- **Online/Offline Status** - Real-time presence tracking
- **Lazy Loading** - Efficient chat history loading
- **Read Receipts** - Know when messages are delivered and read

## Technology Stack

### Backend
- **API Gateway:** Node.js, Express
- **Authentication:** Node.js, Twilio (SMS verification)
- **Chat Service:** Node.js, Socket.IO (WebSocket)
- **Translation:** Python, MarianNMT, TextBlob
- **Transcription:** Python, OpenAI Whisper
- **Media Storage:** Node.js, MinIO
- **User/Presence:** Node.js, Redis

### Databases
- **Cassandra** - Scalable NoSQL for messages and user data
- **Redis** - Caching and real-time presence
- **MinIO** - Object storage for media files

### Mobile
- **Android** - Java, Retrofit, Socket.IO, Signal Protocol
- **iOS** - Swift, Alamofire, Socket.IO, Signal Protocol

### Infrastructure
- **Docker** - Containerization
- **Kubernetes** - Orchestration
- **Prometheus** - Metrics
- **Grafana** - Visualization
- **GitHub Actions** - CI/CD

## Architecture

```
┌─────────────┐
│   Clients   │
│ Android/iOS │
└──────┬──────┘
       │
       ▼
┌─────────────┐
│ API Gateway │
│   (Port     │
│   3000)     │
└──────┬──────┘
       │
       ├────────┬────────┬────────┬────────┬────────┐
       ▼        ▼        ▼        ▼        ▼        ▼
┌──────────┐ ┌─────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐
│   Auth   │ │User │ │ Chat │ │Trans │ │Trans │ │Media │
│  :3001   │ │:3002│ │:3003 │ │:3004 │ │:3005 │ │:3006 │
└────┬─────┘ └──┬──┘ └───┬──┘ └──┬───┘ └──┬───┘ └──┬───┘
     │          │        │       │        │        │
     └──────────┴────────┴───────┴────────┴────────┘
                         │
                ┌────────┴─────────┐
                ▼                  ▼
         ┌──────────┐       ┌──────────┐
         │Cassandra │       │  Redis   │
         │  :9042   │       │  :6379   │
         └──────────┘       └──────────┘
```

## Quick Start

### Prerequisites
- Docker and Docker Compose
- Node.js 18+ (for local development)
- Python 3.10+ (for translation/transcription services)

### Local Development

1. **Clone the repository:**
```bash
git clone https://github.com/yourusername/tolkflip.git
cd tolkflip
```

2. **Configure environment variables:**
```bash
cp .env.example .env
# Edit .env with your credentials (Twilio, Firebase, etc.)
```

3. **Start all services:**
```bash
docker-compose up -d
```

4. **Access the application:**
- API Gateway: http://localhost:3000
- Prometheus: http://localhost:9090
- Grafana: http://localhost:3001 (admin/admin)

### Mobile App Development

**Android:**
```bash
cd mobile/android
./gradlew build
./gradlew installDebug
```

**iOS:**
```bash
cd mobile/ios
pod install
open Tolkflip.xcworkspace
```

## Supported Languages

### European Languages
English, German, French, Spanish, Italian, Portuguese, Dutch, Polish, Czech, Slovak, Romanian, Hungarian, Swedish, Danish, Finnish, Norwegian, Greek, Bulgarian, Croatian, Serbian, Slovenian, Estonian, Latvian, Lithuanian

### Asian Languages
Chinese (Simplified & Traditional), Japanese, Korean, Hindi, Bengali, Tamil, Telugu, Marathi, Gujarati, Kannada, Malayalam, Punjabi, Urdu, Vietnamese, Thai, Indonesian, Malay

### Other Languages
Arabic, Hebrew, Persian, Turkish, Russian, Ukrainian, Belarusian

## API Documentation

### Authentication
```http
POST /api/auth/request-code
Content-Type: application/json

{
  "phone_number": "+1234567890"
}
```

```http
POST /api/auth/verify
Content-Type: application/json

{
  "phone_number": "+1234567890",
  "code": "123456",
  "display_name": "John Doe",
  "primary_language": "en"
}
```

### Sending Messages
```javascript
// WebSocket connection
const socket = io('ws://localhost:3003', {
  auth: { token: 'your-jwt-token' }
});

// Send message
socket.emit('send_message', {
  threadId: 'thread-uuid',
  receiverId: 'user-uuid',
  content: 'Hello!',
  messageType: 'text',
  originalLanguage: 'en'
});

// Receive message
socket.on('new_message', (message) => {
  console.log('New message:', message);
});

// Receive translation
socket.on('message_translated', (data) => {
  console.log('Translated:', data.translatedContent);
});
```

### Translation
```http
POST /api/translate
Authorization: Bearer <token>
Content-Type: application/json

{
  "text": "Hello, how are you?",
  "source_language": "en",
  "target_language": "es",
  "detect_emotion": true
}
```

## Deployment

See [DEPLOYMENT.md](./DEPLOYMENT.md) for detailed deployment instructions including:
- Kubernetes deployment
- Docker Swarm deployment
- Cloud provider setup (AWS, GCP, Azure)
- Monitoring and scaling
- Security best practices

## Project Structure

```
tolkflip/
├── backend/
│   ├── api-gateway/
│   ├── auth-service/
│   ├── chat-service/
│   ├── translation-service/
│   ├── transcription-service/
│   ├── media-service/
│   ├── presence-service/
│   ├── user-service/
│   └── shared/
├── mobile/
│   ├── android/
│   └── ios/
├── infrastructure/
│   ├── docker/
│   ├── kubernetes/
│   ├── cassandra/
│   ├── redis/
│   └── monitoring/
├── devops/
│   ├── ci-cd/
│   └── scripts/
├── docker-compose.yml
└── README.md
```

## Contributing

We welcome contributions! Please follow these steps:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## Testing

**Backend Services:**
```bash
cd backend/auth-service
npm test
```

**Android:**
```bash
cd mobile/android
./gradlew test
```

**iOS:**
```bash
cd mobile/ios
xcodebuild test -workspace Tolkflip.xcworkspace -scheme Tolkflip
```

## Performance

- **Message Latency:** < 100ms (p95)
- **Translation Speed:** < 500ms for typical messages
- **Transcription Speed:** 2-5x real-time (depends on audio length)
- **Concurrent Users:** 1M+ with proper scaling
- **WebSocket Connections:** 10K+ per chat service instance

## Security

- End-to-end encryption using Signal Protocol
- JWT-based authentication
- Phone number verification via Twilio
- Rate limiting on all endpoints
- Input validation and sanitization
- Regular security audits
- OWASP Top 10 compliance

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- [MarianNMT](https://github.com/Helsinki-NLP/Opus-MT) for translation models
- [OpenAI Whisper](https://github.com/openai/whisper) for transcription
- [Signal Protocol](https://signal.org/docs/) for encryption
- Apache Cassandra community
- Socket.IO team

## Contact

- **Website:** https://tolkflip.com
- **Email:** support@tolkflip.com
- **Twitter:** @tolkflip
- **Discord:** https://discord.gg/tolkflip

## Roadmap

- [ ] Video calling support
- [ ] Screen sharing
- [ ] AI-powered message suggestions
- [ ] Voice cloning for multilingual audio
- [ ] Desktop applications (Electron)
- [ ] Web application
- [ ] Smart replies
- [ ] Message scheduling
- [ ] Custom stickers and GIFs
- [ ] Integration with translation memory

---

Made with ❤️ by the Tolkflip Team
