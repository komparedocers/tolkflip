package com.tolkflip.encryption;

import android.content.Context;
import android.util.Base64;
import android.util.Log;

import org.whispersystems.libsignal.DuplicateMessageException;
import org.whispersystems.libsignal.IdentityKey;
import org.whispersystems.libsignal.IdentityKeyPair;
import org.whispersystems.libsignal.InvalidKeyException;
import org.whispersystems.libsignal.InvalidKeyIdException;
import org.whispersystems.libsignal.InvalidMessageException;
import org.whispersystems.libsignal.InvalidVersionException;
import org.whispersystems.libsignal.LegacyMessageException;
import org.whispersystems.libsignal.NoSessionException;
import org.whispersystems.libsignal.SessionCipher;
import org.whispersystems.libsignal.SignalProtocolAddress;
import org.whispersystems.libsignal.UntrustedIdentityException;
import org.whispersystems.libsignal.ecc.Curve;
import org.whispersystems.libsignal.ecc.ECKeyPair;
import org.whispersystems.libsignal.protocol.CiphertextMessage;
import org.whispersystems.libsignal.protocol.PreKeySignalMessage;
import org.whispersystems.libsignal.protocol.SignalMessage;
import org.whispersystems.libsignal.state.PreKeyBundle;
import org.whispersystems.libsignal.state.PreKeyRecord;
import org.whispersystems.libsignal.state.SignalProtocolStore;
import org.whispersystems.libsignal.state.SignedPreKeyRecord;
import org.whispersystems.libsignal.util.KeyHelper;

import java.io.IOException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.List;

/**
 * End-to-end encryption manager using Signal Protocol
 */
public class E2EEncryptionManager {
    private static final String TAG = "E2EEncryptionManager";
    private static final int PREKEY_START_ID = 1;
    private static final int PREKEY_COUNT = 100;

    private final Context context;
    private final SignalProtocolStore protocolStore;
    private final String userId;

    private static E2EEncryptionManager instance;

    private E2EEncryptionManager(Context context, String userId) {
        this.context = context.getApplicationContext();
        this.userId = userId;
        this.protocolStore = new SignalProtocolStoreImpl(context, userId);
    }

    public static synchronized E2EEncryptionManager getInstance(Context context, String userId) {
        if (instance == null || !instance.userId.equals(userId)) {
            instance = new E2EEncryptionManager(context, userId);
        }
        return instance;
    }

    /**
     * Initialize the encryption manager (generate keys if needed)
     */
    public void initialize() {
        try {
            // Check if identity key pair exists
            IdentityKeyPair identityKeyPair = protocolStore.getIdentityKeyPair();

            if (identityKeyPair == null) {
                Log.i(TAG, "Generating new identity key pair");
                identityKeyPair = KeyHelper.generateIdentityKeyPair();
                protocolStore.saveIdentity(new SignalProtocolAddress(userId, 1), identityKeyPair.getPublicKey());
            }

            // Generate pre-keys if needed
            if (protocolStore.loadSignedPreKey(1) == null) {
                generatePreKeys();
            }

            Log.i(TAG, "E2E encryption initialized");
        } catch (Exception e) {
            Log.e(TAG, "Error initializing encryption", e);
        }
    }

    /**
     * Generate pre-keys for key exchange
     */
    private void generatePreKeys() throws InvalidKeyException {
        IdentityKeyPair identityKeyPair = protocolStore.getIdentityKeyPair();

        // Generate signed pre-key
        int signedPreKeyId = 1;
        ECKeyPair signedPreKeyPair = Curve.generateKeyPair();
        long timestamp = System.currentTimeMillis();
        byte[] signedPreKeySignature = Curve.calculateSignature(
            identityKeyPair.getPrivateKey(),
            signedPreKeyPair.getPublicKey().serialize()
        );

        SignedPreKeyRecord signedPreKeyRecord = new SignedPreKeyRecord(
            signedPreKeyId,
            timestamp,
            signedPreKeyPair,
            signedPreKeySignature
        );

        protocolStore.storeSignedPreKey(signedPreKeyId, signedPreKeyRecord);

        // Generate one-time pre-keys
        List<PreKeyRecord> preKeyRecords = KeyHelper.generatePreKeys(PREKEY_START_ID, PREKEY_COUNT);
        for (PreKeyRecord record : preKeyRecords) {
            protocolStore.storePreKey(record.getId(), record);
        }

        Log.i(TAG, "Generated " + PREKEY_COUNT + " pre-keys");
    }

    /**
     * Get pre-key bundle for sharing with other users
     */
    public PreKeyBundle getPreKeyBundle() {
        try {
            IdentityKeyPair identityKeyPair = protocolStore.getIdentityKeyPair();
            int registrationId = protocolStore.getLocalRegistrationId();

            // Get signed pre-key
            SignedPreKeyRecord signedPreKeyRecord = protocolStore.loadSignedPreKey(1);

            // Get a one-time pre-key
            int preKeyId = PREKEY_START_ID + new SecureRandom().nextInt(PREKEY_COUNT);
            PreKeyRecord preKeyRecord = protocolStore.loadPreKey(preKeyId);

            return new PreKeyBundle(
                registrationId,
                1, // device ID
                preKeyId,
                preKeyRecord.getKeyPair().getPublicKey(),
                signedPreKeyRecord.getId(),
                signedPreKeyRecord.getKeyPair().getPublicKey(),
                signedPreKeyRecord.getSignature(),
                identityKeyPair.getPublicKey()
            );
        } catch (Exception e) {
            Log.e(TAG, "Error getting pre-key bundle", e);
            return null;
        }
    }

    /**
     * Process received pre-key bundle from another user
     */
    public void processPreKeyBundle(String recipientId, PreKeyBundle preKeyBundle) {
        try {
            SignalProtocolAddress recipientAddress = new SignalProtocolAddress(recipientId, 1);

            org.whispersystems.libsignal.SessionBuilder sessionBuilder =
                new org.whispersystems.libsignal.SessionBuilder(protocolStore, recipientAddress);

            sessionBuilder.process(preKeyBundle);

            Log.i(TAG, "Processed pre-key bundle for " + recipientId);
        } catch (Exception e) {
            Log.e(TAG, "Error processing pre-key bundle", e);
        }
    }

    /**
     * Encrypt a message for a recipient
     */
    public String encryptMessage(String recipientId, String plaintext) {
        try {
            SignalProtocolAddress recipientAddress = new SignalProtocolAddress(recipientId, 1);
            SessionCipher sessionCipher = new SessionCipher(protocolStore, recipientAddress);

            CiphertextMessage ciphertext = sessionCipher.encrypt(plaintext.getBytes());

            // Convert to base64 for transmission
            byte[] serialized = ciphertext.serialize();
            String encoded = Base64.encodeToString(serialized, Base64.NO_WRAP);

            // Prepend message type (3 = PreKeySignalMessage, 2 = SignalMessage)
            String prefix = ciphertext.getType() == CiphertextMessage.PREKEY_TYPE ? "3:" : "2:";

            Log.i(TAG, "Encrypted message for " + recipientId);
            return prefix + encoded;

        } catch (UntrustedIdentityException e) {
            Log.e(TAG, "Untrusted identity for " + recipientId, e);
            return null;
        }
    }

    /**
     * Decrypt a received message
     */
    public String decryptMessage(String senderId, String ciphertextString) {
        try {
            // Parse message type and ciphertext
            String[] parts = ciphertextString.split(":", 2);
            if (parts.length != 2) {
                Log.e(TAG, "Invalid ciphertext format");
                return null;
            }

            int messageType = Integer.parseInt(parts[0]);
            byte[] ciphertext = Base64.decode(parts[1], Base64.NO_WRAP);

            SignalProtocolAddress senderAddress = new SignalProtocolAddress(senderId, 1);
            SessionCipher sessionCipher = new SessionCipher(protocolStore, senderAddress);

            byte[] plaintext;

            if (messageType == CiphertextMessage.PREKEY_TYPE) {
                // PreKey message (first message in a conversation)
                PreKeySignalMessage preKeyMessage = new PreKeySignalMessage(ciphertext);
                plaintext = sessionCipher.decrypt(preKeyMessage);
            } else {
                // Regular message
                SignalMessage signalMessage = new SignalMessage(ciphertext);
                plaintext = sessionCipher.decrypt(signalMessage);
            }

            Log.i(TAG, "Decrypted message from " + senderId);
            return new String(plaintext);

        } catch (InvalidMessageException | InvalidKeyException | InvalidKeyIdException |
                 DuplicateMessageException | InvalidVersionException | LegacyMessageException |
                 NoSessionException | UntrustedIdentityException e) {
            Log.e(TAG, "Error decrypting message from " + senderId, e);
            return null;
        }
    }

    /**
     * Check if we have a session with a user
     */
    public boolean hasSession(String userId) {
        SignalProtocolAddress address = new SignalProtocolAddress(userId, 1);
        return protocolStore.containsSession(address);
    }

    /**
     * Delete session with a user
     */
    public void deleteSession(String userId) {
        SignalProtocolAddress address = new SignalProtocolAddress(userId, 1);
        protocolStore.deleteSession(address);
        Log.i(TAG, "Deleted session with " + userId);
    }

    /**
     * Get the public identity key as a base64 string
     */
    public String getPublicIdentityKey() {
        try {
            IdentityKeyPair identityKeyPair = protocolStore.getIdentityKeyPair();
            byte[] publicKeyBytes = identityKeyPair.getPublicKey().serialize();
            return Base64.encodeToString(publicKeyBytes, Base64.NO_WRAP);
        } catch (Exception e) {
            Log.e(TAG, "Error getting public identity key", e);
            return null;
        }
    }

    /**
     * Save another user's public identity key
     */
    public void savePublicIdentityKey(String userId, String publicKeyBase64) {
        try {
            byte[] publicKeyBytes = Base64.decode(publicKeyBase64, Base64.NO_WRAP);
            IdentityKey identityKey = new IdentityKey(publicKeyBytes, 0);

            SignalProtocolAddress address = new SignalProtocolAddress(userId, 1);
            protocolStore.saveIdentity(address, identityKey);

            Log.i(TAG, "Saved public identity key for " + userId);
        } catch (Exception e) {
            Log.e(TAG, "Error saving public identity key", e);
        }
    }

    /**
     * Clear all encryption data (for logout)
     */
    public void clearAll() {
        protocolStore.deleteAllSessions();
        Log.i(TAG, "Cleared all encryption data");
    }
}
