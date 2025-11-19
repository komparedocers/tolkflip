package com.tolkflip.chat.ui.viewmodel;

import android.app.Application;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.lifecycle.AndroidViewModel;
import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import com.tolkflip.chat.data.local.AppDatabase;
import com.tolkflip.chat.data.local.dao.UserDao;
import com.tolkflip.chat.data.local.entity.UserEntity;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * ViewModel for ProfileActivity
 * Manages user profile data and updates
 */
public class ProfileViewModel extends AndroidViewModel {

    private final AppDatabase database;
    private final UserDao userDao;
    private final ExecutorService executor;

    private final MutableLiveData<String> saveStatus = new MutableLiveData<>();
    private final MutableLiveData<Boolean> loading = new MutableLiveData<>(false);

    public ProfileViewModel(@NonNull Application application) {
        super(application);

        database = AppDatabase.getInstance(application);
        userDao = database.userDao();
        executor = Executors.newSingleThreadExecutor();
    }

    /**
     * Get user by ID
     */
    public LiveData<UserEntity> getUser(String userId) {
        return userDao.getUserById(userId);
    }

    /**
     * Get save status
     */
    public LiveData<String> getSaveStatus() {
        return saveStatus;
    }

    /**
     * Get loading state
     */
    public LiveData<Boolean> isLoading() {
        return loading;
    }

    /**
     * Load user from database/API
     */
    public void loadUser(String userId) {
        loading.setValue(true);

        executor.execute(() -> {
            UserEntity user = userDao.getUserByIdSync(userId);

            if (user == null) {
                // TODO: Fetch from API
                // ApiClient.getInstance().getUser(userId, response -> {
                //     if (response.isSuccessful()) {
                //         UserEntity fetchedUser = response.getUser();
                //         userDao.insert(fetchedUser);
                //     }
                // });
            }

            loading.postValue(false);
        });
    }

    /**
     * Update user profile
     */
    public void updateProfile(String userId, String displayName, String bio,
                             Uri avatarUri, List<String> additionalLanguages) {
        loading.setValue(true);

        executor.execute(() -> {
            try {
                String avatarUrl = null;

                // Upload avatar if selected
                if (avatarUri != null) {
                    avatarUrl = uploadAvatar(avatarUri);
                }

                // Update in local database
                if (avatarUrl != null) {
                    userDao.updateUserProfile(userId, displayName, bio, avatarUrl);
                } else {
                    // Update without changing avatar
                    UserEntity user = userDao.getUserByIdSync(userId);
                    if (user != null) {
                        user.setDisplayName(displayName);
                        user.setBio(bio);
                        user.setAdditionalLanguages(additionalLanguages);
                        userDao.update(user);
                    }
                }

                // TODO: Sync with API
                // ApiClient.getInstance().updateProfile(userId, displayName, bio, avatarUrl,
                //     additionalLanguages, response -> {
                //         if (response.isSuccessful()) {
                //             saveStatus.postValue("success");
                //         } else {
                //             saveStatus.postValue("error");
                //         }
                //     });

                saveStatus.postValue("success");
            } catch (Exception e) {
                e.printStackTrace();
                saveStatus.postValue("error");
            } finally {
                loading.postValue(false);
            }
        });
    }

    /**
     * Update primary language
     */
    public void updatePrimaryLanguage(String userId, String language) {
        executor.execute(() -> {
            userDao.updatePrimaryLanguage(userId, language);

            // TODO: Sync with API
            // ApiClient.getInstance().updatePrimaryLanguage(userId, language);
        });
    }

    /**
     * Clear all data (for logout)
     */
    public void clearAllData() {
        executor.execute(() -> {
            database.clearAllData();
        });
    }

    /**
     * Upload avatar to server
     */
    private String uploadAvatar(Uri uri) {
        // TODO: Implement actual upload to MinIO/S3
        // For now, return local URI as string
        return uri.toString();

        /*
        try {
            // Read file from URI
            InputStream inputStream = getApplication().getContentResolver().openInputStream(uri);
            byte[] fileData = readBytes(inputStream);

            // Upload to server
            String response = ApiClient.getInstance().uploadFile(fileData, "avatar.jpg");
            return response.getUrl();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
        */
    }

    @Override
    protected void onCleared() {
        super.onCleared();
        executor.shutdown();
    }
}
