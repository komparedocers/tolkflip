package com.tolkflip.activities;

import android.os.Bundle;
import android.view.MenuItem;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.fragment.app.Fragment;

import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.tolkflip.R;
import com.tolkflip.fragments.ChatsFragment;
import com.tolkflip.fragments.ContactsFragment;
import com.tolkflip.fragments.SettingsFragment;
import com.tolkflip.network.WebSocketManager;
import com.tolkflip.utils.TokenManager;

public class MainActivity extends AppCompatActivity {

    private BottomNavigationView bottomNav;
    private WebSocketManager webSocketManager;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        bottomNav = findViewById(R.id.bottom_navigation);
        bottomNav.setOnNavigationItemSelectedListener(navListener);

        // Load default fragment
        if (savedInstanceState == null) {
            getSupportFragmentManager().beginTransaction()
                .replace(R.id.fragment_container, new ChatsFragment())
                .commit();
        }

        // Initialize WebSocket connection
        initializeWebSocket();
    }

    private void initializeWebSocket() {
        String wsUrl = BuildConfig.API_BASE_URL.replace("http", "ws") + "/api/chat";
        webSocketManager = WebSocketManager.getInstance(wsUrl);

        String token = TokenManager.getInstance(this).getAccessToken();
        if (token != null) {
            webSocketManager.connect(token);
        }
    }

    private final BottomNavigationView.OnNavigationItemSelectedListener navListener =
        new BottomNavigationView.OnNavigationItemSelectedListener() {
            @Override
            public boolean onNavigationItemSelected(@NonNull MenuItem item) {
                Fragment selectedFragment = null;

                int itemId = item.getItemId();
                if (itemId == R.id.nav_chats) {
                    selectedFragment = new ChatsFragment();
                } else if (itemId == R.id.nav_contacts) {
                    selectedFragment = new ContactsFragment();
                } else if (itemId == R.id.nav_settings) {
                    selectedFragment = new SettingsFragment();
                }

                if (selectedFragment != null) {
                    getSupportFragmentManager().beginTransaction()
                        .replace(R.id.fragment_container, selectedFragment)
                        .commit();
                }

                return true;
            }
        };

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (webSocketManager != null) {
            webSocketManager.disconnect();
        }
    }
}
