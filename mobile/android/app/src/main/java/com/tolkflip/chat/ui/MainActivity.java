package com.tolkflip.chat.ui;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.SearchView;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.tolkflip.chat.R;
import com.tolkflip.chat.data.local.entity.ThreadEntity;
import com.tolkflip.chat.ui.adapter.ThreadAdapter;
import com.tolkflip.chat.ui.viewmodel.MainViewModel;
import com.tolkflip.chat.util.PreferenceManager;

/**
 * MainActivity - Thread list (Conversation list)
 * Main entry point after login
 */
public class MainActivity extends AppCompatActivity implements ThreadAdapter.OnThreadClickListener {

    // UI Components
    private MaterialToolbar toolbar;
    private RecyclerView recyclerView;
    private SwipeRefreshLayout swipeRefreshLayout;
    private TextView emptyStateText;
    private ProgressBar loadingProgress;
    private FloatingActionButton newChatFab;
    private SearchView searchView;

    // Data
    private MainViewModel viewModel;
    private ThreadAdapter adapter;
    private String currentUserId;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Check if logged in
        if (!PreferenceManager.isLoggedIn(this)) {
            navigateToLogin();
            return;
        }

        setContentView(R.layout.activity_main);

        currentUserId = PreferenceManager.getUserId(this);

        initializeViews();
        setupToolbar();
        setupRecyclerView();
        setupViewModel();
        setupListeners();
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (viewModel != null) {
            viewModel.refreshThreads();
        }
    }

    private void initializeViews() {
        toolbar = findViewById(R.id.toolbar);
        recyclerView = findViewById(R.id.threads_recycler_view);
        swipeRefreshLayout = findViewById(R.id.swipe_refresh);
        emptyStateText = findViewById(R.id.empty_state_text);
        loadingProgress = findViewById(R.id.loading_progress);
        newChatFab = findViewById(R.id.new_chat_fab);
    }

    private void setupToolbar() {
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setTitle("Tolkflip");
        }
    }

    private void setupRecyclerView() {
        adapter = new ThreadAdapter(this, java.util.Collections.emptyList(), this);
        recyclerView.setLayoutManager(new LinearLayoutManager(this));
        recyclerView.setAdapter(adapter);
    }

    private void setupViewModel() {
        viewModel = new ViewModelProvider(this).get(MainViewModel.class);
        viewModel.setCurrentUserId(currentUserId);

        // Observe threads
        viewModel.getThreads().observe(this, threads -> {
            loadingProgress.setVisibility(View.GONE);
            swipeRefreshLayout.setRefreshing(false);

            if (threads == null || threads.isEmpty()) {
                emptyStateText.setVisibility(View.VISIBLE);
                recyclerView.setVisibility(View.GONE);
            } else {
                emptyStateText.setVisibility(View.GONE);
                recyclerView.setVisibility(View.VISIBLE);
                adapter.setThreads(threads);
            }
        });

        // Observe unread count for badge
        viewModel.getTotalUnreadCount().observe(this, count -> {
            // Update badge if needed
            if (count != null && count > 0) {
                // TODO: Show badge on app icon
            }
        });
    }

    private void setupListeners() {
        // Swipe to refresh
        swipeRefreshLayout.setOnRefreshListener(() -> {
            viewModel.refreshThreads();
        });

        // New chat FAB
        newChatFab.setOnClickListener(v -> showNewChatDialog());
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.menu_main, menu);

        // Setup search
        MenuItem searchItem = menu.findItem(R.id.action_search);
        searchView = (SearchView) searchItem.getActionView();
        searchView.setQueryHint("Search conversations");

        searchView.setOnQueryTextListener(new SearchView.OnQueryTextListener() {
            @Override
            public boolean onQueryTextSubmit(String query) {
                viewModel.searchThreads(query);
                return true;
            }

            @Override
            public boolean onQueryTextChange(String newText) {
                viewModel.searchThreads(newText);
                return true;
            }
        });

        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();

        if (id == R.id.action_profile) {
            openProfile();
            return true;
        } else if (id == R.id.action_settings) {
            openSettings();
            return true;
        } else if (id == R.id.action_archived) {
            showArchivedThreads();
            return true;
        }

        return super.onOptionsItemSelected(item);
    }

    // ThreadAdapter.OnThreadClickListener implementation
    @Override
    public void onThreadClick(ThreadEntity thread) {
        openChat(thread);
    }

    @Override
    public void onThreadLongClick(ThreadEntity thread) {
        showThreadOptions(thread);
    }

    private void openChat(ThreadEntity thread) {
        Intent intent = new Intent(this, ChatActivity.class);
        intent.putExtra(ChatActivity.EXTRA_THREAD_ID, thread.getThreadId());
        intent.putExtra(ChatActivity.EXTRA_CONTACT_NAME, thread.getName());
        intent.putExtra(ChatActivity.EXTRA_CONTACT_ID, thread.getOtherParticipantId());
        startActivity(intent);
    }

    private void openProfile() {
        Intent intent = new Intent(this, ProfileActivity.class);
        intent.putExtra(ProfileActivity.EXTRA_USER_ID, currentUserId);
        startActivity(intent);
    }

    private void openSettings() {
        // TODO: Create SettingsActivity
        android.widget.Toast.makeText(this, "Settings coming soon", android.widget.Toast.LENGTH_SHORT).show();
    }

    private void showNewChatDialog() {
        // TODO: Implement contact picker
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("New Chat");
        builder.setMessage("Contact picker coming soon");
        builder.setPositiveButton("OK", null);
        builder.show();
    }

    private void showArchivedThreads() {
        viewModel.setShowArchived(true);

        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("Archived Chats");
        builder.setMessage("Showing archived conversations");
        builder.setPositiveButton("Back", (dialog, which) -> {
            viewModel.setShowArchived(false);
        });
        builder.show();
    }

    private void showThreadOptions(ThreadEntity thread) {
        String[] options;
        if (thread.isPinned()) {
            options = new String[]{"Unpin", "Archive", "Mute", "Delete"};
        } else {
            options = new String[]{"Pin", "Archive", "Mute", "Delete"};
        }

        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(thread.getName());
        builder.setItems(options, (dialog, which) -> {
            switch (which) {
                case 0: // Pin/Unpin
                    viewModel.setThreadPinned(thread.getThreadId(), !thread.isPinned());
                    android.widget.Toast.makeText(this,
                        thread.isPinned() ? "Unpinned" : "Pinned",
                        android.widget.Toast.LENGTH_SHORT).show();
                    break;
                case 1: // Archive
                    viewModel.setThreadArchived(thread.getThreadId(), true);
                    android.widget.Toast.makeText(this, "Archived", android.widget.Toast.LENGTH_SHORT).show();
                    break;
                case 2: // Mute
                    viewModel.setThreadMuted(thread.getThreadId(), !thread.isMuted());
                    android.widget.Toast.makeText(this,
                        thread.isMuted() ? "Unmuted" : "Muted",
                        android.widget.Toast.LENGTH_SHORT).show();
                    break;
                case 3: // Delete
                    confirmDeleteThread(thread);
                    break;
            }
        });
        builder.show();
    }

    private void confirmDeleteThread(ThreadEntity thread) {
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle("Delete Chat");
        builder.setMessage("Are you sure you want to delete this conversation? This cannot be undone.");
        builder.setPositiveButton("Delete", (dialog, which) -> {
            viewModel.deleteThread(thread.getThreadId());
            android.widget.Toast.makeText(this, "Deleted", android.widget.Toast.LENGTH_SHORT).show();
        });
        builder.setNegativeButton("Cancel", null);
        builder.show();
    }

    private void navigateToLogin() {
        Intent intent = new Intent(this, LoginActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        startActivity(intent);
        finish();
    }
}
