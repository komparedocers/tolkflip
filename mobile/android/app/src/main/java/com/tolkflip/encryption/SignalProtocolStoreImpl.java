package com.tolkflip.encryption;

import android.content.Context;
import android.content.SharedPreferences;
import android.util.Base64;
import android.util.Log;

import org.whispersystems.libsignal.IdentityKey;
import org.whispersystems.libsignal.IdentityKeyPair;
import org.whispersystems.libsignal.InvalidKeyException;
import org.whispersystems.libsignal.SignalProtocolAddress;
import org.whispersystems.libsignal.ecc.Curve;
import org.whispersystems.libsignal.ecc.ECKeyPair;
import org.whispersystems.libsignal.ecc.ECPrivateKey;
import org.whispersystems.libsignal.ecc.ECPublicKey;
import org.whispersystems.libsignal.state.IdentityKeyStore;
import org.whispersystems.libsignal.state.PreKeyRecord;
import org.whispersystems.libsignal.state.PreKeyStore;
import org.whispersystems.libsignal.state.SessionRecord;
import org.whispersystems.libsignal.state.SessionStore;
import org.whispersystems.libsignal.state.SignalProtocolStore;
import org.whispersystems.libsignal.state.SignedPreKeyRecord;
import org.whispersystems.libsignal.state.SignedPreKeyStore;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of Signal Protocol Store using SharedPreferences
 */
public class SignalProtocolStoreImpl implements SignalProtocolStore {
    private static final String TAG = "SignalProtocolStore";
    private static final String PREF_NAME = "signal_protocol_store";

    private final SharedPreferences prefs;
    private final String userId;

    public SignalProtocolStoreImpl(Context context, String userId) {
        this.userId = userId;
        this.prefs = context.getSharedPreferences(PREF_NAME + "_" + userId, Context.MODE_PRIVATE);
    }

    // ========== IdentityKeyStore Implementation ==========

    @Override
    public IdentityKeyPair getIdentityKeyPair() {
        try {
            String privateKeyStr = prefs.getString("identity_private_key", null);
            String publicKeyStr = prefs.getString("identity_public_key", null);

            if (privateKeyStr == null || publicKeyStr == null) {
                // Generate new identity key pair
                IdentityKeyPair identityKeyPair = org.whispersystems.libsignal.util.KeyHelper.generateIdentityKeyPair();
                saveIdentityKeyPair(identityKeyPair);
                return identityKeyPair;
            }

            ECPrivateKey privateKey = Curve.decodePrivatePoint(Base64.decode(privateKeyStr, Base64.NO_WRAP));
            IdentityKey publicKey = new IdentityKey(Base64.decode(publicKeyStr, Base64.NO_WRAP), 0);

            return new IdentityKeyPair(publicKey, privateKey);
        } catch (Exception e) {
            Log.e(TAG, "Error getting identity key pair", e);
            return null;
        }
    }

    private void saveIdentityKeyPair(IdentityKeyPair identityKeyPair) {
        String privateKey = Base64.encodeToString(identityKeyPair.getPrivateKey().serialize(), Base64.NO_WRAP);
        String publicKey = Base64.encodeToString(identityKeyPair.getPublicKey().serialize(), Base64.NO_WRAP);

        prefs.edit()
            .putString("identity_private_key", privateKey)
            .putString("identity_public_key", publicKey)
            .apply();
    }

    @Override
    public int getLocalRegistrationId() {
        int registrationId = prefs.getInt("registration_id", -1);
        if (registrationId == -1) {
            registrationId = org.whispersystems.libsignal.util.KeyHelper.generateRegistrationId(false);
            prefs.edit().putInt("registration_id", registrationId).apply();
        }
        return registrationId;
    }

    @Override
    public boolean saveIdentity(SignalProtocolAddress address, IdentityKey identityKey) {
        String key = "identity_" + address.getName();
        String existingKey = prefs.getString(key, null);
        String newKey = Base64.encodeToString(identityKey.serialize(), Base64.NO_WRAP);

        if (!newKey.equals(existingKey)) {
            prefs.edit().putString(key, newKey).apply();
            return true;
        }
        return false;
    }

    @Override
    public boolean isTrustedIdentity(SignalProtocolAddress address, IdentityKey identityKey, Direction direction) {
        String key = "identity_" + address.getName();
        String storedKey = prefs.getString(key, null);

        if (storedKey == null) {
            // No stored identity, trust this one
            return true;
        }

        String newKey = Base64.encodeToString(identityKey.serialize(), Base64.NO_WRAP);
        return storedKey.equals(newKey);
    }

    @Override
    public IdentityKey getIdentity(SignalProtocolAddress address) {
        try {
            String key = "identity_" + address.getName();
            String storedKey = prefs.getString(key, null);

            if (storedKey == null) {
                return null;
            }

            return new IdentityKey(Base64.decode(storedKey, Base64.NO_WRAP), 0);
        } catch (Exception e) {
            Log.e(TAG, "Error getting identity", e);
            return null;
        }
    }

    // ========== PreKeyStore Implementation ==========

    @Override
    public PreKeyRecord loadPreKey(int preKeyId) {
        try {
            String key = "prekey_" + preKeyId;
            String serialized = prefs.getString(key, null);

            if (serialized == null) {
                return null;
            }

            return new PreKeyRecord(Base64.decode(serialized, Base64.NO_WRAP));
        } catch (Exception e) {
            Log.e(TAG, "Error loading pre-key", e);
            return null;
        }
    }

    @Override
    public void storePreKey(int preKeyId, PreKeyRecord record) {
        String key = "prekey_" + preKeyId;
        String serialized = Base64.encodeToString(record.serialize(), Base64.NO_WRAP);
        prefs.edit().putString(key, serialized).apply();
    }

    @Override
    public boolean containsPreKey(int preKeyId) {
        String key = "prekey_" + preKeyId;
        return prefs.contains(key);
    }

    @Override
    public void removePreKey(int preKeyId) {
        String key = "prekey_" + preKeyId;
        prefs.edit().remove(key).apply();
    }

    // ========== SignedPreKeyStore Implementation ==========

    @Override
    public SignedPreKeyRecord loadSignedPreKey(int signedPreKeyId) {
        try {
            String key = "signed_prekey_" + signedPreKeyId;
            String serialized = prefs.getString(key, null);

            if (serialized == null) {
                return null;
            }

            return new SignedPreKeyRecord(Base64.decode(serialized, Base64.NO_WRAP));
        } catch (Exception e) {
            Log.e(TAG, "Error loading signed pre-key", e);
            return null;
        }
    }

    @Override
    public List<SignedPreKeyRecord> loadSignedPreKeys() {
        List<SignedPreKeyRecord> results = new ArrayList<>();

        for (String key : prefs.getAll().keySet()) {
            if (key.startsWith("signed_prekey_")) {
                try {
                    String serialized = prefs.getString(key, null);
                    if (serialized != null) {
                        results.add(new SignedPreKeyRecord(Base64.decode(serialized, Base64.NO_WRAP)));
                    }
                } catch (Exception e) {
                    Log.e(TAG, "Error loading signed pre-key", e);
                }
            }
        }

        return results;
    }

    @Override
    public void storeSignedPreKey(int signedPreKeyId, SignedPreKeyRecord record) {
        String key = "signed_prekey_" + signedPreKeyId;
        String serialized = Base64.encodeToString(record.serialize(), Base64.NO_WRAP);
        prefs.edit().putString(key, serialized).apply();
    }

    @Override
    public boolean containsSignedPreKey(int signedPreKeyId) {
        String key = "signed_prekey_" + signedPreKeyId;
        return prefs.contains(key);
    }

    @Override
    public void removeSignedPreKey(int signedPreKeyId) {
        String key = "signed_prekey_" + signedPreKeyId;
        prefs.edit().remove(key).apply();
    }

    // ========== SessionStore Implementation ==========

    @Override
    public SessionRecord loadSession(SignalProtocolAddress address) {
        try {
            String key = "session_" + address.getName() + "_" + address.getDeviceId();
            String serialized = prefs.getString(key, null);

            if (serialized == null) {
                return new SessionRecord();
            }

            return new SessionRecord(Base64.decode(serialized, Base64.NO_WRAP));
        } catch (Exception e) {
            Log.e(TAG, "Error loading session", e);
            return new SessionRecord();
        }
    }

    @Override
    public List<Integer> getSubDeviceSessions(String name) {
        List<Integer> deviceIds = new ArrayList<>();

        for (String key : prefs.getAll().keySet()) {
            if (key.startsWith("session_" + name + "_")) {
                try {
                    String deviceIdStr = key.substring(("session_" + name + "_").length());
                    deviceIds.add(Integer.parseInt(deviceIdStr));
                } catch (NumberFormatException e) {
                    Log.e(TAG, "Error parsing device ID", e);
                }
            }
        }

        return deviceIds;
    }

    @Override
    public void storeSession(SignalProtocolAddress address, SessionRecord record) {
        String key = "session_" + address.getName() + "_" + address.getDeviceId();
        String serialized = Base64.encodeToString(record.serialize(), Base64.NO_WRAP);
        prefs.edit().putString(key, serialized).apply();
    }

    @Override
    public boolean containsSession(SignalProtocolAddress address) {
        String key = "session_" + address.getName() + "_" + address.getDeviceId();
        return prefs.contains(key);
    }

    @Override
    public void deleteSession(SignalProtocolAddress address) {
        String key = "session_" + address.getName() + "_" + address.getDeviceId();
        prefs.edit().remove(key).apply();
    }

    @Override
    public void deleteAllSessions(String name) {
        SharedPreferences.Editor editor = prefs.edit();

        for (String key : prefs.getAll().keySet()) {
            if (key.startsWith("session_" + name + "_")) {
                editor.remove(key);
            }
        }

        editor.apply();
    }

    public void deleteAllSessions() {
        SharedPreferences.Editor editor = prefs.edit();

        for (String key : prefs.getAll().keySet()) {
            if (key.startsWith("session_") || key.startsWith("prekey_") ||
                key.startsWith("signed_prekey_") || key.startsWith("identity_")) {
                editor.remove(key);
            }
        }

        editor.apply();
    }
}
