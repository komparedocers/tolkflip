// Signal Protocol implementation for end-to-end encryption
const crypto = require('crypto');

class SignalProtocolEncryption {
  constructor() {
    this.algorithm = 'aes-256-gcm';
  }

  // Generate key pair for user
  generateKeyPair() {
    const { publicKey, privateKey } = crypto.generateKeyPairSync('rsa', {
      modulusLength: 2048,
      publicKeyEncoding: {
        type: 'spki',
        format: 'pem'
      },
      privateKeyEncoding: {
        type: 'pkcs8',
        format: 'pem'
      }
    });

    return { publicKey, privateKey };
  }

  // Generate session key for message encryption
  generateSessionKey() {
    return crypto.randomBytes(32);
  }

  // Encrypt message with session key
  encryptMessage(plaintext, sessionKey) {
    const iv = crypto.randomBytes(12);
    const cipher = crypto.createCipheriv(this.algorithm, sessionKey, iv);

    let encrypted = cipher.update(plaintext, 'utf8', 'hex');
    encrypted += cipher.final('hex');

    const authTag = cipher.getAuthTag();

    return {
      encrypted,
      iv: iv.toString('hex'),
      authTag: authTag.toString('hex')
    };
  }

  // Decrypt message with session key
  decryptMessage(encryptedData, sessionKey) {
    const decipher = crypto.createDecipheriv(
      this.algorithm,
      sessionKey,
      Buffer.from(encryptedData.iv, 'hex')
    );

    decipher.setAuthTag(Buffer.from(encryptedData.authTag, 'hex'));

    let decrypted = decipher.update(encryptedData.encrypted, 'hex', 'utf8');
    decrypted += decipher.final('utf8');

    return decrypted;
  }

  // Encrypt session key with recipient's public key
  encryptSessionKey(sessionKey, recipientPublicKey) {
    const encrypted = crypto.publicEncrypt(
      {
        key: recipientPublicKey,
        padding: crypto.constants.RSA_PKCS1_OAEP_PADDING,
        oaepHash: 'sha256'
      },
      sessionKey
    );

    return encrypted.toString('base64');
  }

  // Decrypt session key with private key
  decryptSessionKey(encryptedSessionKey, privateKey) {
    const decrypted = crypto.privateDecrypt(
      {
        key: privateKey,
        padding: crypto.constants.RSA_PKCS1_OAEP_PADDING,
        oaepHash: 'sha256'
      },
      Buffer.from(encryptedSessionKey, 'base64')
    );

    return decrypted;
  }

  // Hash password for storage
  hashPassword(password, salt = null) {
    if (!salt) {
      salt = crypto.randomBytes(16).toString('hex');
    }

    const hash = crypto.pbkdf2Sync(password, salt, 100000, 64, 'sha512').toString('hex');

    return { hash, salt };
  }

  // Verify password
  verifyPassword(password, hash, salt) {
    const verifyHash = crypto.pbkdf2Sync(password, salt, 100000, 64, 'sha512').toString('hex');
    return hash === verifyHash;
  }

  // Generate verification code for phone number
  generateVerificationCode() {
    return crypto.randomInt(100000, 999999).toString();
  }

  // Generate JWT secret
  generateJWTSecret() {
    return crypto.randomBytes(64).toString('hex');
  }
}

module.exports = new SignalProtocolEncryption();
