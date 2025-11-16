# Cross‑Cultural Chat App – README

This project is a WhatsApp‑style messenger with built‑in automatic translation for text and audio.

## Highlights
- Phone‑number signup (very simple onboarding)
- Real‑time 1:1 and group chats
- Per‑thread language settings, with user default + 2 extra free languages
- Voice notes with transcription + translation
- Voice/video calls
- End‑to‑end encryption
- Open‑source, locally deployable translation stack
- Cloud‑agnostic microservices backend

## Run Locally (Conceptual)

1. Start infra (Cassandra, Redis, MinIO) with Docker Compose.
2. Start backend services (Auth, Chat, Presence, Message Store, Media, Translation, Notifications, Gateway).
3. Configure Android & iOS apps to point at the gateway URL.
4. Run the apps from Android Studio / Xcode.

For production, containerize all services and deploy on Kubernetes with autoscaling and monitoring.
