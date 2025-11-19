package com.tolkflip.chat.data.local;

import android.content.Context;

import androidx.room.Database;
import androidx.room.Room;
import androidx.room.RoomDatabase;
import androidx.room.TypeConverters;

import com.tolkflip.chat.data.local.dao.MessageDao;
import com.tolkflip.chat.data.local.dao.ThreadDao;
import com.tolkflip.chat.data.local.dao.UserDao;
import com.tolkflip.chat.data.local.entity.MessageEntity;
import com.tolkflip.chat.data.local.entity.ThreadEntity;
import com.tolkflip.chat.data.local.entity.UserEntity;

/**
 * Room Database for Tolkflip
 * Offline-first architecture for messages, threads, and users
 */
@Database(
    entities = {
        UserEntity.class,
        MessageEntity.class,
        ThreadEntity.class
    },
    version = 1,
    exportSchema = false
)
@TypeConverters({Converters.class})
public abstract class AppDatabase extends RoomDatabase {

    private static final String DATABASE_NAME = "tolkflip_db";
    private static volatile AppDatabase INSTANCE;

    // DAOs
    public abstract UserDao userDao();
    public abstract MessageDao messageDao();
    public abstract ThreadDao threadDao();

    /**
     * Get database instance (Singleton pattern)
     */
    public static AppDatabase getInstance(Context context) {
        if (INSTANCE == null) {
            synchronized (AppDatabase.class) {
                if (INSTANCE == null) {
                    INSTANCE = Room.databaseBuilder(
                        context.getApplicationContext(),
                        AppDatabase.class,
                        DATABASE_NAME
                    )
                    .fallbackToDestructiveMigration()
                    .build();
                }
            }
        }
        return INSTANCE;
    }

    /**
     * Clear all data (for logout)
     */
    public void clearAllData() {
        runInTransaction(() -> {
            userDao().deleteAll();
            messageDao().deleteAll();
            threadDao().deleteAll();
        });
    }
}
