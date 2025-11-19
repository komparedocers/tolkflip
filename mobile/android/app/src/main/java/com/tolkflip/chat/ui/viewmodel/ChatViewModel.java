package com.tolkflip.chat.ui.viewmodel;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import com.tolkflip.chat.data.local.AppDatabase;
import com.tolkflip.chat.data.local.dao.MessageDao;
import com.tolkflip.chat.data.local.dao.ThreadDao;
import com.tolkflip.chat.data.local.dao.UserDao;
import com.tolkflip.chat.data.local.entity.MessageEntity;
import com.tolkflip.chat.data.local.entity.ThreadEntity;
import com.tolkflip.chat.data.local.entity.UserEntity;
import com.tolkflip.chat.network.ApiClient;
import com.tolkflip.chat.service.WebSocketService;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * ViewModel for ChatActivity
 * Manages messages, threads, and real-time communication
 */
public class ChatViewModel extends AndroidViewModel {

    private final AppDatabase database;
    private final MessageDao messageDao;
    private final ThreadDao threadDao;
    private final UserDao userDao;
    private final ExecutorService executor;

    private final MutableLiveData<String> sendStatus = new MutableLiveData<>();
    private final MutableLiveData<Set<String>> typingUsers = new MutableLiveData<>(new HashSet<>());
    private final MutableLiveData<Boolean> contactOnline = new MutableLiveData<>(false);

    public ChatViewModel(@NonNull Application application) {
        super(application);

        database = AppDatabase.getInstance(application);
        messageDao = database.messageDao();
        threadDao = database.threadDao();
        userDao = database.userDao();
        executor = Executors.newSingleThreadExecutor();
    }

    /**
     * Get messages for a thread
     */
    public LiveData<List<MessageEntity>> getMessages(String threadId) {
        return messageDao.getThreadMessages(threadId);
    }

    /**
     * Get thread details
     */
    public LiveData<ThreadEntity> getThread(String threadId) {
        return threadDao.getThreadById(threadId);
    }

    /**
     * Get contact online status
     */
    public LiveData<Boolean> getContactStatus(String contactId) {
        return contactOnline;
    }

    /**
     * Get typing users
     */
    public LiveData<Set<String>> getTypingUsers(String threadId) {
        return typingUsers;
    }

    /**
     * Get send status
     */
    public LiveData<String> getSendStatus() {
        return sendStatus;
    }

    /**
     * Load thread data
     */
    public void loadThread(String threadId) {
        executor.execute(() -> {
            ThreadEntity thread = threadDao.getThreadByIdSync(threadId);

            if (thread == null) {
                // Fetch from API
                fetchThreadFromApi(threadId);
            }
        });
    }

    /**
     * Send a message
     */
    public void sendMessage(MessageEntity message) {
        executor.execute(() -> {
            try {
                // Save to local database immediately
                messageDao.insert(message);

                // Update thread preview
                threadDao.updateLastMessage(
                    message.getThreadId(),
                    message.getContent(),
                    message.getTimestamp()
                );

                // Send via WebSocket (if connected) or API
                sendMessageToServer(message);

                sendStatus.postValue("success");
            } catch (Exception e) {
                e.printStackTrace();
                message.setStatus("failed");
                messageDao.update(message);
                sendStatus.postValue("error");
            }
        });
    }

    /**
     * Update message status
     */
    public void updateMessageStatus(String messageId, String status) {
        executor.execute(() -> {
            messageDao.updateMessageStatus(messageId, status);
        });
    }

    /**
     * Mark thread as read
     */
    public void markThreadAsRead(String threadId, String currentUserId) {
        executor.execute(() -> {
            messageDao.markThreadAsRead(threadId, currentUserId);
            threadDao.markAsRead(threadId);
        });
    }

    /**
     * Send typing indicator
     */
    public void sendTypingIndicator(String threadId, String contactId, boolean isTyping) {
        // Send via WebSocket
        WebSocketService.sendTypingIndicator(threadId, contactId, isTyping);
    }

    /**
     * Handle incoming message from WebSocket
     */
    public void onMessageReceived(MessageEntity message) {
        executor.execute(() -> {
            // Save to database
            messageDao.insert(message);

            // Update thread
            threadDao.updateLastMessage(
                message.getThreadId(),
                message.getContent(),
                message.getTimestamp()
            );

            // Increment unread count if not in this chat
            ThreadEntity thread = threadDao.getThreadByIdSync(message.getThreadId());
            if (thread != null) {
                threadDao.updateUnreadCount(
                    message.getThreadId(),
                    thread.getUnreadCount() + 1
                );
            }
        });
    }

    /**
     * Handle typing event from WebSocket
     */
    public void onTypingEvent(String userId, boolean isTyping) {
        Set<String> currentTyping = typingUsers.getValue();
        if (currentTyping == null) {
            currentTyping = new HashSet<>();
        }

        if (isTyping) {
            currentTyping.add(userId);
        } else {
            currentTyping.remove(userId);
        }

        typingUsers.postValue(currentTyping);
    }

    /**
     * Handle presence update from WebSocket
     */
    public void onPresenceUpdate(String userId, boolean isOnline) {
        executor.execute(() -> {
            userDao.updateUserStatus(userId, isOnline, System.currentTimeMillis());
        });

        contactOnline.postValue(isOnline);
    }

    /**
     * Send message to server via API
     */
    private void sendMessageToServer(MessageEntity message) {
        // TODO: Implement API call
        // ApiClient.getInstance().sendMessage(message, response -> {
        //     if (response.isSuccessful()) {
        //         updateMessageStatus(message.getMessageId(), "sent");
        //     } else {
        //         updateMessageStatus(message.getMessageId(), "failed");
        //     }
        // });
    }

    /**
     * Fetch thread from API
     */
    private void fetchThreadFromApi(String threadId) {
        // TODO: Implement API call
        // ApiClient.getInstance().getThread(threadId, response -> {
        //     if (response.isSuccessful()) {
        //         ThreadEntity thread = response.getThread();
        //         executor.execute(() -> threadDao.insert(thread));
        //     }
        // });
    }

    /**
     * Retry failed messages
     */
    public void retryFailedMessages() {
        executor.execute(() -> {
            List<MessageEntity> failedMessages = messageDao.getPendingMessages();
            for (MessageEntity message : failedMessages) {
                message.setStatus("sending");
                messageDao.update(message);
                sendMessageToServer(message);
            }
        });
    }

    @Override
    protected void onCleared() {
        super.onCleared();
        executor.shutdown();
    }
}
