// Cassandra database models and queries for the chat application

const cassandra = require('cassandra-driver');

class DatabaseModels {
  constructor(contactPoints, keyspace) {
    this.client = new cassandra.Client({
      contactPoints: contactPoints || ['127.0.0.1'],
      localDataCenter: 'datacenter1',
      keyspace: keyspace || 'tolkflip'
    });
  }

  async connect() {
    await this.client.connect();
    console.log('Connected to Cassandra');
  }

  // Initialize keyspace and tables
  async initializeSchema() {
    const queries = [
      // Create keyspace
      `CREATE KEYSPACE IF NOT EXISTS tolkflip
       WITH replication = {'class': 'NetworkTopologyStrategy', 'datacenter1': 3}`,

      // Users table
      `CREATE TABLE IF NOT EXISTS users (
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
      )`,

      // Create index on phone_number for lookup
      `CREATE INDEX IF NOT EXISTS ON users (phone_number)`,

      // Chat threads table
      `CREATE TABLE IF NOT EXISTS chat_threads (
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
      ) WITH CLUSTERING ORDER BY (last_message_time DESC, thread_id ASC)`,

      // Messages table - optimized for time-series queries
      `CREATE TABLE IF NOT EXISTS messages (
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
      ) WITH CLUSTERING ORDER BY (message_id DESC)`,

      // Group chats table
      `CREATE TABLE IF NOT EXISTS group_chats (
        group_id uuid PRIMARY KEY,
        group_name text,
        created_by uuid,
        created_at timestamp,
        avatar_url text,
        description text,
        member_count int,
        metadata map<text, text>
      )`,

      // Group members table
      `CREATE TABLE IF NOT EXISTS group_members (
        group_id uuid,
        user_id uuid,
        joined_at timestamp,
        role text,
        preferred_language text,
        PRIMARY KEY (group_id, user_id)
      )`,

      // User language preferences per thread
      `CREATE TABLE IF NOT EXISTS thread_settings (
        user_id uuid,
        thread_id uuid,
        preferred_language text,
        show_original boolean,
        enable_emotion_detection boolean,
        custom_settings map<text, text>,
        PRIMARY KEY (user_id, thread_id)
      )`,

      // Translation cache table
      `CREATE TABLE IF NOT EXISTS translation_cache (
        source_text text,
        source_lang text,
        target_lang text,
        translated_text text,
        confidence float,
        cached_at timestamp,
        PRIMARY KEY ((source_text, source_lang), target_lang)
      )`,

      // Media metadata table
      `CREATE TABLE IF NOT EXISTS media_metadata (
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
      )`,

      // Presence tracking (also in Redis for real-time)
      `CREATE TABLE IF NOT EXISTS user_presence (
        user_id uuid,
        status text,
        last_seen timestamp,
        device_id text,
        PRIMARY KEY (user_id, device_id)
      )`
    ];

    for (const query of queries) {
      try {
        await this.client.execute(query);
      } catch (error) {
        console.error(`Error executing query: ${query}`, error);
      }
    }

    console.log('Database schema initialized');
  }

  // User operations
  async createUser(userData) {
    const query = `
      INSERT INTO users (user_id, phone_number, display_name, avatar_url,
                        primary_language, additional_languages, created_at, last_active)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?)
    `;
    const params = [
      userData.user_id,
      userData.phone_number,
      userData.display_name,
      userData.avatar_url || '',
      userData.primary_language || 'en',
      userData.additional_languages || [],
      new Date(),
      new Date()
    ];
    return await this.client.execute(query, params, { prepare: true });
  }

  async getUserByPhone(phoneNumber) {
    const query = 'SELECT * FROM users WHERE phone_number = ? LIMIT 1';
    const result = await this.client.execute(query, [phoneNumber], { prepare: true });
    return result.rows[0];
  }

  async getUserById(userId) {
    const query = 'SELECT * FROM users WHERE user_id = ?';
    const result = await this.client.execute(query, [userId], { prepare: true });
    return result.rows[0];
  }

  // Message operations
  async saveMessage(messageData) {
    const query = `
      INSERT INTO messages (thread_id, message_id, sender_id, receiver_id,
                           message_type, content, original_language, encrypted_content,
                           timestamp, status, is_group, media_urls, metadata)
      VALUES (?, now(), ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
    `;
    const params = [
      messageData.thread_id,
      messageData.sender_id,
      messageData.receiver_id,
      messageData.message_type,
      messageData.content,
      messageData.original_language,
      messageData.encrypted_content,
      new Date(),
      messageData.status || 'sent',
      messageData.is_group || false,
      messageData.media_urls || [],
      messageData.metadata || {}
    ];
    return await this.client.execute(query, params, { prepare: true });
  }

  async getMessages(threadId, limit = 50, pagingState = null) {
    const query = `
      SELECT * FROM messages
      WHERE thread_id = ?
      LIMIT ?
    `;
    const options = { prepare: true, fetchSize: limit };
    if (pagingState) {
      options.pageState = pagingState;
    }

    return await this.client.execute(query, [threadId, limit], options);
  }

  async updateMessageStatus(threadId, messageId, status) {
    const query = `
      UPDATE messages
      SET status = ?
      WHERE thread_id = ? AND message_id = ?
    `;
    return await this.client.execute(query, [status, threadId, messageId], { prepare: true });
  }

  // Thread operations
  async createOrUpdateThread(threadData) {
    const query = `
      INSERT INTO chat_threads (thread_id, participant_id, other_participant_id,
                               thread_type, created_at, last_message_time,
                               last_message_preview, unread_count, is_archived)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
    `;
    const params = [
      threadData.thread_id,
      threadData.participant_id,
      threadData.other_participant_id,
      threadData.thread_type || 'direct',
      threadData.created_at || new Date(),
      threadData.last_message_time || new Date(),
      threadData.last_message_preview || '',
      threadData.unread_count || 0,
      threadData.is_archived || false
    ];
    return await this.client.execute(query, params, { prepare: true });
  }

  async getThreadsForUser(userId, limit = 20) {
    const query = `
      SELECT * FROM chat_threads
      WHERE participant_id = ?
      LIMIT ?
    `;
    const result = await this.client.execute(query, [userId, limit], { prepare: true });
    return result.rows;
  }

  // Translation cache operations
  async getCachedTranslation(sourceText, sourceLang, targetLang) {
    const query = `
      SELECT * FROM translation_cache
      WHERE source_text = ? AND source_lang = ? AND target_lang = ?
    `;
    const result = await this.client.execute(
      query,
      [sourceText, sourceLang, targetLang],
      { prepare: true }
    );
    return result.rows[0];
  }

  async cacheTranslation(sourceText, sourceLang, targetLang, translatedText, confidence) {
    const query = `
      INSERT INTO translation_cache (source_text, source_lang, target_lang,
                                     translated_text, confidence, cached_at)
      VALUES (?, ?, ?, ?, ?, ?)
      USING TTL 2592000
    `;
    const params = [sourceText, sourceLang, targetLang, translatedText, confidence, new Date()];
    return await this.client.execute(query, params, { prepare: true });
  }

  // Group operations
  async createGroup(groupData) {
    const query = `
      INSERT INTO group_chats (group_id, group_name, created_by, created_at,
                              avatar_url, description, member_count)
      VALUES (?, ?, ?, ?, ?, ?, ?)
    `;
    const params = [
      groupData.group_id,
      groupData.name,
      groupData.created_by,
      groupData.created_at,
      groupData.group_icon || '',
      groupData.description || '',
      groupData.member_count || 1
    ];
    return await this.client.execute(query, params, { prepare: true });
  }

  async getGroupById(groupId) {
    const query = 'SELECT * FROM group_chats WHERE group_id = ?';
    const result = await this.client.execute(query, [groupId], { prepare: true });
    return result.rows[0];
  }

  async updateGroup(groupId, updates) {
    const setClauses = [];
    const params = [];

    if (updates.name) {
      setClauses.push('group_name = ?');
      params.push(updates.name);
    }
    if (updates.description !== undefined) {
      setClauses.push('description = ?');
      params.push(updates.description);
    }
    if (updates.group_icon !== undefined) {
      setClauses.push('avatar_url = ?');
      params.push(updates.group_icon);
    }

    if (setClauses.length === 0) return;

    params.push(groupId);

    const query = `
      UPDATE group_chats
      SET ${setClauses.join(', ')}
      WHERE group_id = ?
    `;

    return await this.client.execute(query, params, { prepare: true });
  }

  async updateGroupMemberCount(groupId, count) {
    const query = 'UPDATE group_chats SET member_count = ? WHERE group_id = ?';
    return await this.client.execute(query, [count, groupId], { prepare: true });
  }

  async deleteGroup(groupId) {
    // Delete all group members first
    await this.client.execute('DELETE FROM group_members WHERE group_id = ?', [groupId], { prepare: true });

    // Delete the group
    const query = 'DELETE FROM group_chats WHERE group_id = ?';
    return await this.client.execute(query, [groupId], { prepare: true });
  }

  // Group member operations
  async addGroupMember(memberData) {
    const query = `
      INSERT INTO group_members (group_id, user_id, joined_at, role, preferred_language)
      VALUES (?, ?, ?, ?, ?)
    `;
    const params = [
      memberData.group_id,
      memberData.user_id,
      memberData.joined_at,
      memberData.role || 'member',
      memberData.preferred_language || 'en'
    ];
    return await this.client.execute(query, params, { prepare: true });
  }

  async getGroupMember(groupId, userId) {
    const query = 'SELECT * FROM group_members WHERE group_id = ? AND user_id = ?';
    const result = await this.client.execute(query, [groupId, userId], { prepare: true });
    return result.rows[0];
  }

  async getGroupMembers(groupId, limit = 100, offset = 0) {
    const query = `
      SELECT * FROM group_members
      WHERE group_id = ?
      LIMIT ?
    `;
    const result = await this.client.execute(query, [groupId, limit], { prepare: true });
    return result.rows;
  }

  async removeGroupMember(groupId, userId) {
    const query = 'DELETE FROM group_members WHERE group_id = ? AND user_id = ?';
    return await this.client.execute(query, [groupId, userId], { prepare: true });
  }

  async updateGroupMemberRole(groupId, userId, newRole) {
    const query = `
      UPDATE group_members
      SET role = ?
      WHERE group_id = ? AND user_id = ?
    `;
    return await this.client.execute(query, [newRole, groupId, userId], { prepare: true });
  }

  async getUserGroups(userId, limit = 50, offset = 0) {
    const query = `
      SELECT group_id FROM group_members
      WHERE user_id = ?
      LIMIT ?
    `;
    const result = await this.client.execute(query, [userId, limit], { prepare: true });

    // Fetch full group details
    const groups = [];
    for (const row of result.rows) {
      const group = await this.getGroupById(row.group_id);
      if (group) {
        groups.push(group);
      }
    }

    return groups;
  }

  async updateGroupSettings(groupId, settings) {
    // Note: This requires adding a settings column to group_chats table
    // For now, we'll store it in a metadata column
    const query = `
      UPDATE group_chats
      SET metadata = ?
      WHERE group_id = ?
    `;
    return await this.client.execute(query, [settings, groupId], { prepare: true });
  }

  async close() {
    await this.client.shutdown();
  }

  async shutdown() {
    await this.close();
  }
}

module.exports = DatabaseModels;
