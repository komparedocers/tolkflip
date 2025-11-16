# Description

This chat application enables people of different cultures to talk to each other easily by
automatically translating their conversations.

## Product Summary

- The app looks and feels like WhatsApp.
- Users sign up with their phone number.
- Each user chooses a default/native language and may also choose up to two extra languages that
  are free to use for viewing chats.
- Every chat thread has a configurable “view language”. The same conversation can be shown in
  different languages to different users.
- Text and audio messages are end‑to‑end encrypted. Translation uses open‑source MarianNMT and
  Whisper, deployed locally or in your own cloud environment.

## High‑Level Architecture

- **Mobile clients** (Android in Java, iOS in Swift):
  - Maintain a persistent WebSocket to the backend Chat Service.
  - Use local SQLite/Core Data for caching chats and settings.
  - Perform end‑to‑end encryption and decryption on device, using a Signal‑style protocol.
  - Call a Translation/NLP API when the user’s view language differs from the original message.
- **API Gateway**:
  - Terminates TLS, routes WebSocket and HTTP traffic to internal services.
- **Auth & User Service**:
  - Handles phone‑number signup/login and profile management.
  - Stores language preferences and public encryption keys.
- **Chat Service**:
  - Non‑blocking, event‑driven WebSocket server that routes encrypted messages and real‑time
    events (typing, receipts, presence).
- **Presence Service**:
  - Tracks who is online and typing using in‑memory data (Redis).
- **Message Store Service + NoSQL DB**:
  - Persists encrypted messages in Cassandra/ScyllaDB, partitioned per thread and ordered by
    message ID or timestamp for efficient history loading.
- **Media Service + Object Storage**:
  - Stores encrypted images, videos, and audio files in S3/MinIO or equivalent.
- **Translation & NLP Service**:
  - Uses MarianNMT for text translation and Whisper for speech‑to‑text.
  - Optionally performs emotion/tone analysis.
- **Notification Service**:
  - Sends push notifications (FCM/APNS) when recipients are offline.

## Data Flow (Example)

1. Sender writes a message in their own language.
2. Client encrypts it and sends it via WebSocket to the Chat Service.
3. Chat Service persists the message via Message Store Service and fan‑outs to recipients.
4. Recipient client decrypts the message, sees that their thread is set to another language, and
   requests a translation from the Translation Service.
5. Translated text is shown in the UI, with an option to reveal the original text.
6. For voice notes, the client also requests transcription (Whisper) and then translation of the
   resulting text.

## Scalability & Deployment

- All backend services are containerized.
- They can be orchestrated using Kubernetes on any cloud (AWS, GCP, Azure, on‑prem).
- No vendor‑locked services are required: Cassandra, Redis, MinIO, MarianNMT, and Whisper are all
  open‑source and self‑hostable.
- Horizontal scaling of chat, translation, and storage services allows the system to grow to
  millions of users.

This description is intentionally concise but complete enough to act as a standalone specification
for building the system.
