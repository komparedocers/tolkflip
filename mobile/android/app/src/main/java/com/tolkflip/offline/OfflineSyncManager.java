package com.tolkflip.offline;

import android.content.Context;
import android.util.Log;

import androidx.work.Constraints;
import androidx.work.Data;
import androidx.work.NetworkType;
import androidx.work.OneTimeWorkRequest;
import androidx.work.WorkManager;
import androidx.work.WorkRequest;

import com.tolkflip.database.AppDatabase;
import com.tolkflip.database.dao.MessageDao;
import com.tolkflip.database.entities.Message;
import com.tolkflip.network.ApiClient;
import com.tolkflip.network.WebSocketManager;

import java.util.List;
import java.util.concurrent.Executors;

/**
 * Manages offline message queue and synchronization
 */
public class OfflineSyncManager {
    private static final String TAG = "OfflineSyncManager";
    private static OfflineSyncManager instance;

    private final Context context;
    private final AppDatabase database;
    private final MessageDao messageDao;
    private final WebSocketManager webSocketManager;

    private OfflineSyncManager(Context context) {
        this.context = context.getApplicationContext();
        this.database = AppDatabase.getInstance(context);
        this.messageDao = database.messageDao();
        this.webSocketManager = WebSocketManager.getInstance(context);
    }

    public static synchronized OfflineSyncManager getInstance(Context context) {
        if (instance == null) {
            instance = new OfflineSyncManager(context);
        }
        return instance;
    }

    /**
     * Queue a message for sending when online
     */
    public void queueMessage(Message message) {
        Executors.newSingleThreadExecutor().execute(() -> {
            // Mark message as pending
            message.setStatus(Message.MessageStatus.PENDING);
            message.setSyncStatus(Message.SyncStatus.PENDING);

            // Save to local database
            long id = messageDao.insert(message);
            message.setId(id);

            Log.i(TAG, "Message queued for sending: " + message.getMessageId());

            // Try to send immediately if online
            if (isOnline()) {
                sendPendingMessages();
            } else {
                // Schedule work to send when online
                scheduleSync();
            }
        });
    }

    /**
     * Send all pending messages
     */
    public void sendPendingMessages() {
        Executors.newSingleThreadExecutor().execute(() -> {
            List<Message> pendingMessages = messageDao.getPendingMessages();

            Log.i(TAG, "Sending " + pendingMessages.size() + " pending messages");

            for (Message message : pendingMessages) {
                try {
                    // Mark as sending
                    message.setSyncStatus(Message.SyncStatus.SYNCING);
                    messageDao.update(message);

                    // Send via WebSocket
                    boolean sent = webSocketManager.sendMessage(message);

                    if (sent) {
                        // Mark as synced
                        message.setStatus(Message.MessageStatus.SENT);
                        message.setSyncStatus(Message.SyncStatus.SYNCED);
                        messageDao.update(message);

                        Log.i(TAG, "Message sent successfully: " + message.getMessageId());
                    } else {
                        // Failed to send, revert to pending
                        message.setSyncStatus(Message.SyncStatus.PENDING);
                        messageDao.update(message);

                        Log.w(TAG, "Failed to send message: " + message.getMessageId());
                    }
                } catch (Exception e) {
                    Log.e(TAG, "Error sending message: " + message.getMessageId(), e);

                    // Mark as failed
                    message.setSyncStatus(Message.SyncStatus.FAILED);
                    messageDao.update(message);
                }
            }
        });
    }

    /**
     * Sync messages from server
     */
    public void syncMessagesFromServer(String threadId) {
        Executors.newSingleThreadExecutor().execute(() -> {
            try {
                // Get last synced timestamp for this thread
                Message lastMessage = messageDao.getLastMessageForThread(threadId);
                long lastTimestamp = lastMessage != null ? lastMessage.getTimestamp() : 0;

                // Fetch new messages from server
                ApiClient.getInstance(context)
                    .getMessageService()
                    .getMessages(threadId, 50, lastTimestamp)
                    .enqueue(new retrofit2.Callback<ApiClient.MessagesResponse>() {
                        @Override
                        public void onResponse(retrofit2.Call<ApiClient.MessagesResponse> call,
                                             retrofit2.Response<ApiClient.MessagesResponse> response) {
                            if (response.isSuccessful() && response.body() != null) {
                                List<Message> newMessages = response.body().getMessages();

                                Log.i(TAG, "Synced " + newMessages.size() + " messages from server");

                                // Save to local database
                                for (Message message : newMessages) {
                                    message.setSyncStatus(Message.SyncStatus.SYNCED);
                                    messageDao.insertOrUpdate(message);
                                }
                            }
                        }

                        @Override
                        public void onFailure(retrofit2.Call<ApiClient.MessagesResponse> call, Throwable t) {
                            Log.e(TAG, "Failed to sync messages from server", t);
                        }
                    });
            } catch (Exception e) {
                Log.e(TAG, "Error syncing messages from server", e);
            }
        });
    }

    /**
     * Sync all threads
     */
    public void syncAllThreads() {
        Executors.newSingleThreadExecutor().execute(() -> {
            try {
                ApiClient.getInstance(context)
                    .getThreadService()
                    .getThreads()
                    .enqueue(new retrofit2.Callback<ApiClient.ThreadsResponse>() {
                        @Override
                        public void onResponse(retrofit2.Call<ApiClient.ThreadsResponse> call,
                                             retrofit2.Response<ApiClient.ThreadsResponse> response) {
                            if (response.isSuccessful() && response.body() != null) {
                                Log.i(TAG, "Synced threads from server");

                                // Sync messages for each thread
                                for (ApiClient.ThreadInfo thread : response.body().getThreads()) {
                                    syncMessagesFromServer(thread.getThreadId());
                                }
                            }
                        }

                        @Override
                        public void onFailure(retrofit2.Call<ApiClient.ThreadsResponse> call, Throwable t) {
                            Log.e(TAG, "Failed to sync threads from server", t);
                        }
                    });
            } catch (Exception e) {
                Log.e(TAG, "Error syncing threads", e);
            }
        });
    }

    /**
     * Handle incoming message (save to local database)
     */
    public void handleIncomingMessage(Message message) {
        Executors.newSingleThreadExecutor().execute(() -> {
            message.setSyncStatus(Message.SyncStatus.SYNCED);
            messageDao.insertOrUpdate(message);

            Log.i(TAG, "Saved incoming message: " + message.getMessageId());
        });
    }

    /**
     * Update message status (delivery, read receipts)
     */
    public void updateMessageStatus(String messageId, Message.MessageStatus status) {
        Executors.newSingleThreadExecutor().execute(() -> {
            Message message = messageDao.getMessageById(messageId);

            if (message != null) {
                message.setStatus(status);
                messageDao.update(message);

                Log.i(TAG, "Updated message status: " + messageId + " -> " + status);
            }
        });
    }

    /**
     * Delete local message
     */
    public void deleteMessage(String messageId) {
        Executors.newSingleThreadExecutor().execute(() -> {
            messageDao.deleteByMessageId(messageId);
            Log.i(TAG, "Deleted message: " + messageId);
        });
    }

    /**
     * Clear all local data (for logout)
     */
    public void clearAllData() {
        Executors.newSingleThreadExecutor().execute(() -> {
            messageDao.deleteAll();
            Log.i(TAG, "Cleared all local data");
        });
    }

    /**
     * Schedule background sync when network is available
     */
    private void scheduleSync() {
        Constraints constraints = new Constraints.Builder()
            .setRequiredNetworkType(NetworkType.CONNECTED)
            .build();

        WorkRequest syncRequest = new OneTimeWorkRequest.Builder(SyncWorker.class)
            .setConstraints(constraints)
            .build();

        WorkManager.getInstance(context).enqueue(syncRequest);

        Log.i(TAG, "Scheduled background sync");
    }

    /**
     * Check if device is online
     */
    private boolean isOnline() {
        return webSocketManager.isConnected();
    }

    /**
     * Retry failed messages
     */
    public void retryFailedMessages() {
        Executors.newSingleThreadExecutor().execute(() -> {
            List<Message> failedMessages = messageDao.getFailedMessages();

            Log.i(TAG, "Retrying " + failedMessages.size() + " failed messages");

            for (Message message : failedMessages) {
                message.setSyncStatus(Message.SyncStatus.PENDING);
                messageDao.update(message);
            }

            sendPendingMessages();
        });
    }

    /**
     * Get sync statistics
     */
    public SyncStats getSyncStats() {
        int pendingCount = messageDao.getPendingMessages().size();
        int failedCount = messageDao.getFailedMessages().size();
        int syncedCount = messageDao.getSyncedMessagesCount();

        return new SyncStats(pendingCount, failedCount, syncedCount);
    }

    public static class SyncStats {
        public final int pendingCount;
        public final int failedCount;
        public final int syncedCount;

        public SyncStats(int pendingCount, int failedCount, int syncedCount) {
            this.pendingCount = pendingCount;
            this.failedCount = failedCount;
            this.syncedCount = syncedCount;
        }
    }
}
