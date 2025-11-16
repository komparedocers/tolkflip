import Foundation
import SignalProtocol

/**
 * End-to-end encryption manager using Signal Protocol for iOS
 */
class E2EEncryptionManager {
    static let shared = E2EEncryptionManager()

    private var protocolStore: SignalProtocolStoreImpl?
    private var userId: String?

    private let preKeyStartId = 1
    private let preKeyCount = 100

    private init() {}

    /**
     * Initialize the encryption manager for a user
     */
    func initialize(userId: String) {
        self.userId = userId
        self.protocolStore = SignalProtocolStoreImpl(userId: userId)

        // Generate keys if needed
        if protocolStore?.getIdentityKeyPair() == nil {
            generateIdentityKeys()
        }

        // Generate pre-keys if needed
        if protocolStore?.loadSignedPreKey(signedPreKeyId: 1) == nil {
            generatePreKeys()
        }

        print("E2E encryption initialized for user: \(userId)")
    }

    /**
     * Generate identity key pair
     */
    private func generateIdentityKeys() {
        guard let store = protocolStore else { return }

        let identityKeyPair = IdentityKeyPair.generate()
        store.saveIdentityKeyPair(identityKeyPair)

        // Generate registration ID
        let registrationId = UInt32.random(in: 1...16380)
        store.saveRegistrationId(registrationId)

        print("Generated new identity keys")
    }

    /**
     * Generate pre-keys for key exchange
     */
    private func generatePreKeys() {
        guard let store = protocolStore else { return }
        guard let identityKeyPair = store.getIdentityKeyPair() else { return }

        // Generate signed pre-key
        let signedPreKeyPair = KeyPair.generate()
        let timestamp = Date().timeIntervalSince1970

        // Sign the public key
        let signature = try? identityKeyPair.privateKey.sign(message: signedPreKeyPair.publicKey.serialize())

        let signedPreKeyRecord = SignedPreKeyRecord(
            id: 1,
            timestamp: UInt64(timestamp * 1000),
            keyPair: signedPreKeyPair,
            signature: signature ?? Data()
        )

        store.storeSignedPreKey(signedPreKeyRecord, signedPreKeyId: 1)

        // Generate one-time pre-keys
        for i in preKeyStartId..<(preKeyStartId + preKeyCount) {
            let preKeyPair = KeyPair.generate()
            let preKeyRecord = PreKeyRecord(id: UInt32(i), keyPair: preKeyPair)
            store.storePreKey(preKeyRecord, preKeyId: UInt32(i))
        }

        print("Generated \(preKeyCount) pre-keys")
    }

    /**
     * Get pre-key bundle for sharing with other users
     */
    func getPreKeyBundle() -> PreKeyBundle? {
        guard let store = protocolStore else { return nil }
        guard let identityKeyPair = store.getIdentityKeyPair() else { return nil }
        guard let signedPreKey = store.loadSignedPreKey(signedPreKeyId: 1) else { return nil }

        let registrationId = store.getLocalRegistrationId()

        // Get a random one-time pre-key
        let preKeyId = UInt32(preKeyStartId + Int.random(in: 0..<preKeyCount))
        guard let preKey = store.loadPreKey(preKeyId: preKeyId) else { return nil }

        return PreKeyBundle(
            registrationId: registrationId,
            deviceId: 1,
            preKeyId: preKeyId,
            preKeyPublic: preKey.keyPair.publicKey,
            signedPreKeyId: 1,
            signedPreKeyPublic: signedPreKey.keyPair.publicKey,
            signedPreKeySignature: signedPreKey.signature,
            identityKey: identityKeyPair.publicKey
        )
    }

    /**
     * Process received pre-key bundle from another user
     */
    func processPreKeyBundle(recipientId: String, bundle: PreKeyBundle) {
        guard let store = protocolStore else { return }

        let address = SignalAddress(name: recipientId, deviceId: 1)
        let sessionBuilder = SessionBuilder(store: store, address: address)

        do {
            try sessionBuilder.process(preKeyBundle: bundle)
            print("Processed pre-key bundle for \(recipientId)")
        } catch {
            print("Error processing pre-key bundle: \(error)")
        }
    }

    /**
     * Encrypt a message for a recipient
     */
    func encryptMessage(recipientId: String, plaintext: String) -> String? {
        guard let store = protocolStore else { return nil }

        let address = SignalAddress(name: recipientId, deviceId: 1)
        let sessionCipher = SessionCipher(store: store, address: address)

        do {
            guard let plaintextData = plaintext.data(using: .utf8) else { return nil }
            let ciphertext = try sessionCipher.encrypt(plaintextData)

            // Convert to base64
            let encoded = ciphertext.serialize().base64EncodedString()

            // Prepend message type (3 = PreKeySignalMessage, 2 = SignalMessage)
            let prefix = ciphertext.type == .preKey ? "3:" : "2:"

            print("Encrypted message for \(recipientId)")
            return prefix + encoded

        } catch {
            print("Error encrypting message: \(error)")
            return nil
        }
    }

    /**
     * Decrypt a received message
     */
    func decryptMessage(senderId: String, ciphertextString: String) -> String? {
        guard let store = protocolStore else { return nil }

        // Parse message type and ciphertext
        let parts = ciphertextString.components(separatedBy: ":")
        guard parts.count == 2 else {
            print("Invalid ciphertext format")
            return nil
        }

        guard let messageType = Int(parts[0]),
              let ciphertextData = Data(base64Encoded: parts[1]) else {
            print("Invalid ciphertext data")
            return nil
        }

        let address = SignalAddress(name: senderId, deviceId: 1)
        let sessionCipher = SessionCipher(store: store, address: address)

        do {
            let plaintext: Data

            if messageType == 3 {
                // PreKey message
                let preKeyMessage = try PreKeySignalMessage(bytes: ciphertextData)
                plaintext = try sessionCipher.decrypt(preKeyMessage: preKeyMessage)
            } else {
                // Regular message
                let signalMessage = try SignalMessage(bytes: ciphertextData)
                plaintext = try sessionCipher.decrypt(signalMessage: signalMessage)
            }

            print("Decrypted message from \(senderId)")
            return String(data: plaintext, encoding: .utf8)

        } catch {
            print("Error decrypting message: \(error)")
            return nil
        }
    }

    /**
     * Check if we have a session with a user
     */
    func hasSession(userId: String) -> Bool {
        guard let store = protocolStore else { return false }
        let address = SignalAddress(name: userId, deviceId: 1)
        return store.containsSession(for: address)
    }

    /**
     * Delete session with a user
     */
    func deleteSession(userId: String) {
        guard let store = protocolStore else { return }
        let address = SignalAddress(name: userId, deviceId: 1)
        store.deleteSession(for: address)
        print("Deleted session with \(userId)")
    }

    /**
     * Get the public identity key as a base64 string
     */
    func getPublicIdentityKey() -> String? {
        guard let store = protocolStore else { return nil }
        guard let identityKeyPair = store.getIdentityKeyPair() else { return nil }
        return identityKeyPair.publicKey.serialize().base64EncodedString()
    }

    /**
     * Save another user's public identity key
     */
    func savePublicIdentityKey(userId: String, publicKeyBase64: String) {
        guard let store = protocolStore else { return }
        guard let publicKeyData = Data(base64Encoded: publicKeyBase64) else { return }

        do {
            let identityKey = try PublicKey(bytes: publicKeyData)
            let address = SignalAddress(name: userId, deviceId: 1)
            _ = store.saveIdentity(identityKey, for: address)
            print("Saved public identity key for \(userId)")
        } catch {
            print("Error saving public identity key: \(error)")
        }
    }

    /**
     * Clear all encryption data (for logout)
     */
    func clearAll() {
        protocolStore?.deleteAllSessions()
        print("Cleared all encryption data")
    }
}

// MARK: - Supporting Types

struct PreKeyBundle {
    let registrationId: UInt32
    let deviceId: UInt32
    let preKeyId: UInt32
    let preKeyPublic: PublicKey
    let signedPreKeyId: UInt32
    let signedPreKeyPublic: PublicKey
    let signedPreKeySignature: Data
    let identityKey: PublicKey
}

struct PreKeyRecord {
    let id: UInt32
    let keyPair: KeyPair

    func serialize() -> Data {
        // Serialize the record for storage
        var data = Data()
        data.append(withUnsafeBytes(of: id.bigEndian) { Data($0) })
        data.append(keyPair.publicKey.serialize())
        data.append(keyPair.privateKey.serialize())
        return data
    }

    static func deserialize(_ data: Data) throws -> PreKeyRecord {
        guard data.count >= 4 else { throw EncryptionError.invalidData }

        let id = data[0..<4].withUnsafeBytes { $0.load(as: UInt32.self).bigEndian }
        let publicKey = try PublicKey(bytes: data[4..<37])
        let privateKey = try PrivateKey(bytes: data[37...])

        return PreKeyRecord(id: id, keyPair: KeyPair(publicKey: publicKey, privateKey: privateKey))
    }
}

struct SignedPreKeyRecord {
    let id: UInt32
    let timestamp: UInt64
    let keyPair: KeyPair
    let signature: Data

    func serialize() -> Data {
        var data = Data()
        data.append(withUnsafeBytes(of: id.bigEndian) { Data($0) })
        data.append(withUnsafeBytes(of: timestamp.bigEndian) { Data($0) })
        data.append(keyPair.publicKey.serialize())
        data.append(keyPair.privateKey.serialize())
        data.append(signature)
        return data
    }

    static func deserialize(_ data: Data) throws -> SignedPreKeyRecord {
        guard data.count >= 12 else { throw EncryptionError.invalidData }

        let id = data[0..<4].withUnsafeBytes { $0.load(as: UInt32.self).bigEndian }
        let timestamp = data[4..<12].withUnsafeBytes { $0.load(as: UInt64.self).bigEndian }
        let publicKey = try PublicKey(bytes: data[12..<45])
        let privateKey = try PrivateKey(bytes: data[45..<77])
        let signature = data[77...]

        return SignedPreKeyRecord(
            id: id,
            timestamp: timestamp,
            keyPair: KeyPair(publicKey: publicKey, privateKey: privateKey),
            signature: signature
        )
    }
}

enum EncryptionError: Error {
    case invalidData
    case keyGenerationFailed
    case encryptionFailed
    case decryptionFailed
}
