package com.tolkflip;

import android.app.Application;

import com.tolkflip.network.ApiClient;
import com.tolkflip.utils.TokenManager;

public class TolkflipApplication extends Application {

    @Override
    public void onCreate() {
        super.onCreate();

        // Initialize API client
        ApiClient.initialize(this);

        // Initialize TokenManager
        TokenManager.getInstance(this);
    }
}
