// Redis client for presence and caching
const redis = require('redis');

class RedisClient {
  constructor(config = {}) {
    this.client = redis.createClient({
      host: config.host || process.env.REDIS_HOST || 'localhost',
      port: config.port || process.env.REDIS_PORT || 6379,
      password: config.password || process.env.REDIS_PASSWORD,
      retry_strategy: (options) => {
        if (options.error && options.error.code === 'ECONNREFUSED') {
          return new Error('Redis server refused connection');
        }
        if (options.total_retry_time > 1000 * 60 * 60) {
          return new Error('Redis retry time exhausted');
        }
        if (options.attempt > 10) {
          return undefined;
        }
        return Math.min(options.attempt * 100, 3000);
      }
    });

    this.client.on('error', (err) => {
      console.error('Redis Client Error:', err);
    });

    this.client.on('connect', () => {
      console.log('Redis Client Connected');
    });
  }

  async connect() {
    return new Promise((resolve, reject) => {
      this.client.on('ready', () => {
        resolve();
      });
      this.client.on('error', (err) => {
        reject(err);
      });
    });
  }

  // Presence operations
  async setUserOnline(userId, metadata = {}) {
    const key = `presence:${userId}`;
    const data = {
      status: 'online',
      lastSeen: Date.now(),
      ...metadata
    };

    await this.client.hset(key, data);
    await this.client.expire(key, 300); // 5 minutes TTL
    await this.client.sadd('online_users', userId);

    return data;
  }

  async setUserOffline(userId) {
    const key = `presence:${userId}`;
    await this.client.hset(key, {
      status: 'offline',
      lastSeen: Date.now()
    });
    await this.client.srem('online_users', userId);
  }

  async setUserTyping(userId, threadId) {
    const key = `typing:${threadId}:${userId}`;
    await this.client.set(key, '1', 'EX', 10); // 10 seconds TTL

    // Publish typing event
    await this.client.publish(
      `typing:${threadId}`,
      JSON.stringify({ userId, typing: true })
    );
  }

  async removeUserTyping(userId, threadId) {
    const key = `typing:${threadId}:${userId}`;
    await this.client.del(key);

    // Publish typing stopped event
    await this.client.publish(
      `typing:${threadId}`,
      JSON.stringify({ userId, typing: false })
    );
  }

  async getUserPresence(userId) {
    const key = `presence:${userId}`;
    const data = await this.client.hgetall(key);

    if (!data || Object.keys(data).length === 0) {
      return { status: 'offline', lastSeen: null };
    }

    return {
      status: data.status,
      lastSeen: parseInt(data.lastSeen)
    };
  }

  async getOnlineUsers() {
    return await this.client.smembers('online_users');
  }

  // Caching operations
  async cacheSet(key, value, ttl = 3600) {
    const serialized = JSON.stringify(value);
    if (ttl) {
      await this.client.setex(key, ttl, serialized);
    } else {
      await this.client.set(key, serialized);
    }
  }

  async cacheGet(key) {
    const value = await this.client.get(key);
    if (!value) return null;

    try {
      return JSON.parse(value);
    } catch (err) {
      return value;
    }
  }

  async cacheDel(key) {
    await this.client.del(key);
  }

  async cacheExists(key) {
    return await this.client.exists(key);
  }

  // Rate limiting
  async checkRateLimit(identifier, limit, window) {
    const key = `ratelimit:${identifier}`;
    const current = await this.client.incr(key);

    if (current === 1) {
      await this.client.expire(key, window);
    }

    return {
      allowed: current <= limit,
      current,
      limit,
      remaining: Math.max(0, limit - current)
    };
  }

  // PubSub operations
  createSubscriber() {
    return redis.createClient({
      host: process.env.REDIS_HOST || 'localhost',
      port: process.env.REDIS_PORT || 6379
    });
  }

  async publish(channel, message) {
    const data = typeof message === 'string' ? message : JSON.stringify(message);
    return await this.client.publish(channel, data);
  }

  async close() {
    await this.client.quit();
  }
}

module.exports = RedisClient;
