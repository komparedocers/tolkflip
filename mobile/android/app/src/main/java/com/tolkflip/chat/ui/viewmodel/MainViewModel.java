package com.tolkflip.chat.ui.viewmodel;

import android.app.Application;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Transformations;

import com.tolkflip.chat.data.local.AppDatabase;
import com.tolkflip.chat.data.local.dao.MessageDao;
import com.tolkflip.chat.data.local.dao.ThreadDao;
import com.tolkflip.chat.data.local.dao.UserDao;
import com.tolkflip.chat.data.local.entity.ThreadEntity;
import com.tolkflip.chat.network.ApiClient;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * ViewModel for MainActivity (Thread list)
 * Manages thread list, search, unread counts
 */
public class MainViewModel extends AndroidViewModel {

    private final AppDatabase database;
    private final ThreadDao threadDao;
    private final MessageDao messageDao;
    private final UserDao userDao;
    private final ExecutorService executor;

    private final MutableLiveData<String> currentUserId = new MutableLiveData<>();
    private final MutableLiveData<String> searchQuery = new MutableLiveData<>("");
    private final MutableLiveData<Boolean> showArchived = new MutableLiveData<>(false);

    public MainViewModel(@NonNull Application application) {
        super(application);

        database = AppDatabase.getInstance(application);
        threadDao = database.threadDao();
        messageDao = database.messageDao();
        userDao = database.userDao();
        executor = Executors.newSingleThreadExecutor();
    }

    /**
     * Set current user ID
     */
    public void setCurrentUserId(String userId) {
        currentUserId.setValue(userId);
    }

    /**
     * Get all threads for current user
     */
    public LiveData<List<ThreadEntity>> getThreads() {
        return Transformations.switchMap(currentUserId, userId -> {
            if (userId == null) return new MutableLiveData<>();

            Boolean archived = showArchived.getValue();
            if (archived != null && archived) {
                return threadDao.getArchivedThreads(userId);
            } else {
                return threadDao.getAllThreads(userId);
            }
        });
    }

    /**
     * Get threads filtered by search query
     */
    public LiveData<List<ThreadEntity>> getSearchResults() {
        return Transformations.switchMap(searchQuery, query -> {
            String userId = currentUserId.getValue();
            if (userId == null || query == null || query.isEmpty()) {
                return new MutableLiveData<>();
            }
            return threadDao.searchThreads(userId, query);
        });
    }

    /**
     * Get total unread count
     */
    public LiveData<Integer> getTotalUnreadCount() {
        return Transformations.switchMap(currentUserId, userId -> {
            if (userId == null) return new MutableLiveData<>(0);
            return threadDao.getTotalUnreadCount(userId);
        });
    }

    /**
     * Get pinned threads
     */
    public LiveData<List<ThreadEntity>> getPinnedThreads() {
        return Transformations.switchMap(currentUserId, userId -> {
            if (userId == null) return new MutableLiveData<>();
            return threadDao.getPinnedThreads(userId);
        });
    }

    /**
     * Search threads
     */
    public void searchThreads(String query) {
        searchQuery.setValue(query);
    }

    /**
     * Toggle archived view
     */
    public void setShowArchived(boolean show) {
        showArchived.setValue(show);
    }

    /**
     * Archive/unarchive thread
     */
    public void setThreadArchived(String threadId, boolean archived) {
        executor.execute(() -> {
            threadDao.setArchived(threadId, archived);
        });
    }

    /**
     * Mute/unmute thread
     */
    public void setThreadMuted(String threadId, boolean muted) {
        executor.execute(() -> {
            threadDao.setMuted(threadId, muted);
        });
    }

    /**
     * Pin/unpin thread
     */
    public void setThreadPinned(String threadId, boolean pinned) {
        executor.execute(() -> {
            threadDao.setPinned(threadId, pinned);
        });
    }

    /**
     * Delete thread
     */
    public void deleteThread(String threadId) {
        executor.execute(() -> {
            ThreadEntity thread = threadDao.getThreadByIdSync(threadId);
            if (thread != null) {
                threadDao.delete(thread);
                messageDao.deleteThreadMessages(threadId);
            }
        });
    }

    /**
     * Mark thread as read
     */
    public void markThreadAsRead(String threadId) {
        executor.execute(() -> {
            threadDao.markAsRead(threadId);
            String userId = currentUserId.getValue();
            if (userId != null) {
                messageDao.markThreadAsRead(threadId, userId);
            }
        });
    }

    /**
     * Load threads from server
     */
    public void refreshThreads() {
        String userId = currentUserId.getValue();
        if (userId == null) return;

        executor.execute(() -> {
            // TODO: Fetch from API
            // ApiClient.getInstance().getThreads(userId, response -> {
            //     if (response.isSuccessful()) {
            //         List<ThreadEntity> threads = response.getThreads();
            //         threadDao.insertAll(threads);
            //     }
            // });
        });
    }

    /**
     * Handle incoming message update
     */
    public void onMessageReceived(String threadId, String preview, long timestamp) {
        executor.execute(() -> {
            // Update thread
            threadDao.updateLastMessage(threadId, preview, timestamp);

            // Increment unread count
            ThreadEntity thread = threadDao.getThreadByIdSync(threadId);
            if (thread != null) {
                threadDao.updateUnreadCount(threadId, thread.getUnreadCount() + 1);
            }
        });
    }

    /**
     * Get thread count
     */
    public int getThreadCount() {
        String userId = currentUserId.getValue();
        if (userId == null) return 0;

        return threadDao.getThreadCount(userId);
    }

    @Override
    protected void onCleared() {
        super.onCleared();
        executor.shutdown();
    }
}
