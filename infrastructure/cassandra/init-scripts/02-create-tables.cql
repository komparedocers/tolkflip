-- Tolkflip Database Schema
USE tolkflip;

-- Users table
CREATE TABLE IF NOT EXISTS users (
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

CREATE INDEX IF NOT EXISTS idx_users_phone ON users (phone_number);

-- Chat threads table (optimized for listing user's chats)
CREATE TABLE IF NOT EXISTS chat_threads (
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
) WITH CLUSTERING ORDER BY (last_message_time DESC, thread_id ASC)
  AND compaction = {'class': 'LeveledCompactionStrategy'}
  AND comment = 'Chat threads per user, sorted by most recent activity';

-- Messages table (time-series, partitioned by thread)
CREATE TABLE IF NOT EXISTS messages (
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
) WITH CLUSTERING ORDER BY (message_id DESC)
  AND compaction = {'class': 'TimeWindowCompactionStrategy'}
  AND comment = 'Messages partitioned per thread, ordered by message ID (time-based UUID)';

-- Group chats table
CREATE TABLE IF NOT EXISTS group_chats (
  group_id uuid PRIMARY KEY,
  group_name text,
  created_by uuid,
  created_at timestamp,
  avatar_url text,
  description text,
  member_count int
);

-- Group members table
CREATE TABLE IF NOT EXISTS group_members (
  group_id uuid,
  user_id uuid,
  joined_at timestamp,
  role text,
  preferred_language text,
  PRIMARY KEY (group_id, user_id)
);

-- User language preferences per thread
CREATE TABLE IF NOT EXISTS thread_settings (
  user_id uuid,
  thread_id uuid,
  preferred_language text,
  show_original boolean,
  enable_emotion_detection boolean,
  custom_settings map<text, text>,
  PRIMARY KEY (user_id, thread_id)
);

-- Translation cache table (with TTL)
CREATE TABLE IF NOT EXISTS translation_cache (
  source_text text,
  source_lang text,
  target_lang text,
  translated_text text,
  confidence float,
  cached_at timestamp,
  PRIMARY KEY ((source_text, source_lang), target_lang)
) WITH default_time_to_live = 2592000
  AND comment = 'Translation cache with 30-day TTL';

-- Media metadata table
CREATE TABLE IF NOT EXISTS media_metadata (
  media_id uuid PRIMARY KEY,
  thread_id uuid,
  message_id timeuuid,
  uploader_id uuid,
  media_type text,
  file_size bigint,
  mime_type text,
  storage_path text,
  thumbnail_path text,
  uploaded_at timestamp,
  encryption_key blob
);

-- User presence (also tracked in Redis for real-time)
CREATE TABLE IF NOT EXISTS user_presence (
  user_id uuid,
  status text,
  last_seen timestamp,
  device_id text,
  PRIMARY KEY (user_id, device_id)
);

-- Call history table
CREATE TABLE IF NOT EXISTS call_history (
  call_id uuid,
  caller_id uuid,
  callee_id uuid,
  call_type text,
  started_at timestamp,
  ended_at timestamp,
  duration int,
  status text,
  PRIMARY KEY (caller_id, started_at, call_id)
) WITH CLUSTERING ORDER BY (started_at DESC, call_id ASC);
