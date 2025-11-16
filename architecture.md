# Architecture.md

See `description.md` for full architecture overview. This document summarizes:

- Real-time messaging with WebSocket
- Microservice backend (chat, presence, media, translation, auth)
- NoSQL DB (Cassandra) for messages
- Open-source translation (MarianNMT) and audio transcription (Whisper)
- Native apps (Android/Java, iOS/Swift)
- Cloud-agnostic, scalable, secure (Signal Protocol encryption)
