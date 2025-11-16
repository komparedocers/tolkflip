# Tolkflip Android App

This is the Android client for the Tolkflip multilingual chat application.

## Features

- Real-time messaging with WebSocket
- Automatic message translation (80+ languages)
- Voice message transcription
- End-to-end encryption
- Media sharing (images, videos, audio, files)
- Typing indicators and read receipts
- Per-thread language settings

## Building

```bash
# Build debug APK
./gradlew assembleDebug

# Build release APK
./gradlew assembleRelease

# Install on device
./gradlew installDebug

# Run tests
./gradlew test
```

## Configuration

Update `app/build.gradle` with your API endpoint:

```gradle
buildConfigField "String", "API_BASE_URL", "\"http://YOUR_API_URL:3000\""
```

## Requirements

- Android SDK 24+
- Java 17
- Gradle 8.0+
