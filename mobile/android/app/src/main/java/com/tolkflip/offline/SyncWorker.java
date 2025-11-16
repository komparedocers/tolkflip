package com.tolkflip.offline;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

/**
 * Background worker for syncing pending messages
 */
public class SyncWorker extends Worker {
    private static final String TAG = "SyncWorker";

    public SyncWorker(@NonNull Context context, @NonNull WorkerParameters params) {
        super(context, params);
    }

    @NonNull
    @Override
    public Result doWork() {
        Log.i(TAG, "Starting background sync");

        try {
            OfflineSyncManager syncManager = OfflineSyncManager.getInstance(getApplicationContext());

            // Send pending messages
            syncManager.sendPendingMessages();

            // Sync messages from server
            syncManager.syncAllThreads();

            Log.i(TAG, "Background sync completed successfully");
            return Result.success();

        } catch (Exception e) {
            Log.e(TAG, "Background sync failed", e);
            return Result.retry();
        }
    }
}
