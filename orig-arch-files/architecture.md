# Architecture

This file describes the technical structure of the cross‑cultural chat app.

- Native clients: Android (Java) and iOS (Swift)
- Persistent WebSocket connection to a real‑time Chat Service
- Microservice backend: Auth, Chat, Presence, Message Store, Media, Translation/NLP, Notifications
- NoSQL message store (e.g. Cassandra/ScyllaDB) with messages partitioned per thread and ordered by ID/timestamp
- Object storage (e.g. MinIO/S3) for encrypted media
- Translation & transcription via open‑source MarianNMT + Whisper
- End‑to‑end encryption using a Signal‑style protocol on the clients
- Cloud‑agnostic deployment using Docker + Kubernetes

See description.md for a longer narrative and data‑flow details.
