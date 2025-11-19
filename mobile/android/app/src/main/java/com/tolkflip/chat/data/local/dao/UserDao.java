package com.tolkflip.chat.data.local.dao;

import androidx.lifecycle.LiveData;
import androidx.room.Dao;
import androidx.room.Delete;
import androidx.room.Insert;
import androidx.room.OnConflictStrategy;
import androidx.room.Query;
import androidx.room.Update;

import com.tolkflip.chat.data.local.entity.UserEntity;

import java.util.List;

/**
 * Data Access Object for Users
 */
@Dao
public interface UserDao {

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insert(UserEntity user);

    @Insert(onConflict = OnConflictStrategy.REPLACE)
    void insertAll(List<UserEntity> users);

    @Update
    void update(UserEntity user);

    @Delete
    void delete(UserEntity user);

    @Query("DELETE FROM users")
    void deleteAll();

    @Query("SELECT * FROM users WHERE user_id = :userId LIMIT 1")
    LiveData<UserEntity> getUserById(String userId);

    @Query("SELECT * FROM users WHERE user_id = :userId LIMIT 1")
    UserEntity getUserByIdSync(String userId);

    @Query("SELECT * FROM users WHERE phone_number = :phoneNumber LIMIT 1")
    UserEntity getUserByPhone(String phoneNumber);

    @Query("SELECT * FROM users")
    LiveData<List<UserEntity>> getAllUsers();

    @Query("SELECT * FROM users WHERE is_online = 1")
    LiveData<List<UserEntity>> getOnlineUsers();

    @Query("UPDATE users SET is_online = :isOnline, last_active = :lastActive WHERE user_id = :userId")
    void updateUserStatus(String userId, boolean isOnline, long lastActive);

    @Query("UPDATE users SET display_name = :displayName, bio = :bio, avatar_url = :avatarUrl WHERE user_id = :userId")
    void updateUserProfile(String userId, String displayName, String bio, String avatarUrl);

    @Query("UPDATE users SET primary_language = :primaryLang WHERE user_id = :userId")
    void updatePrimaryLanguage(String userId, String primaryLang);

    @Query("SELECT COUNT(*) FROM users")
    int getUserCount();
}
