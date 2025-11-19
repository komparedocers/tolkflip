package com.tolkflip.chat.ui;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.floatingactionbutton.FloatingActionButton;
import com.tolkflip.chat.R;
import com.tolkflip.chat.data.local.entity.MessageEntity;
import com.tolkflip.chat.data.local.entity.ThreadEntity;
import com.tolkflip.chat.ui.adapter.MessageAdapter;
import com.tolkflip.chat.ui.viewmodel.ChatViewModel;
import com.tolkflip.chat.util.PreferenceManager;

import java.util.ArrayList;
import java.util.UUID;

/**
 * ChatActivity - Main chat interface with real-time messaging
 * Features: Message list, send messages, typing indicators, online presence
 */
public class ChatActivity extends AppCompatActivity {

    private static final String EXTRA_THREAD_ID = "thread_id";
    private static final String EXTRA_CONTACT_NAME = "contact_name";
    private static final String EXTRA_CONTACT_ID = "contact_id";

    // UI Components
    private MaterialToolbar toolbar;
    private TextView contactNameText;
    private TextView contactStatusText;
    private ImageView contactAvatar;
    private RecyclerView messagesRecyclerView;
    private EditText messageInput;
    private ImageButton sendButton;
    private ImageButton attachButton;
    private FloatingActionButton scrollToBottomButton;
    private View typingIndicator;

    // Data
    private ChatViewModel viewModel;
    private MessageAdapter messageAdapter;
    private String threadId;
    private String contactId;
    private String contactName;
    private String currentUserId;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_chat);

        // Get intent data
        threadId = getIntent().getStringExtra(EXTRA_THREAD_ID);
        contactId = getIntent().getStringExtra(EXTRA_CONTACT_ID);
        contactName = getIntent().getStringExtra(EXTRA_CONTACT_NAME);
        currentUserId = PreferenceManager.getUserId(this);

        // Initialize UI
        initializeViews();
        setupToolbar();
        setupRecyclerView();
        setupViewModel();
        setupListeners();
    }

    private void initializeViews() {
        toolbar = findViewById(R.id.toolbar);
        contactNameText = findViewById(R.id.contact_name);
        contactStatusText = findViewById(R.id.contact_status);
        contactAvatar = findViewById(R.id.contact_avatar);
        messagesRecyclerView = findViewById(R.id.messages_recycler_view);
        messageInput = findViewById(R.id.message_input);
        sendButton = findViewById(R.id.send_button);
        attachButton = findViewById(R.id.attach_button);
        scrollToBottomButton = findViewById(R.id.scroll_to_bottom_button);
        typingIndicator = findViewById(R.id.typing_indicator);

        // Set contact name
        if (contactName != null) {
            contactNameText.setText(contactName);
        }
    }

    private void setupToolbar() {
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setDisplayShowTitleEnabled(false);
        }

        toolbar.setNavigationOnClickListener(v -> onBackPressed());

        // Contact info click - Open profile
        View contactInfo = findViewById(R.id.contact_info_layout);
        if (contactInfo != null) {
            contactInfo.setOnClickListener(v -> openContactProfile());
        }
    }

    private void setupRecyclerView() {
        messageAdapter = new MessageAdapter(this, new ArrayList<>(), currentUserId);

        LinearLayoutManager layoutManager = new LinearLayoutManager(this);
        layoutManager.setStackFromEnd(true);
        messagesRecyclerView.setLayoutManager(layoutManager);
        messagesRecyclerView.setAdapter(messageAdapter);

        // Auto-scroll to bottom on new messages
        messageAdapter.registerAdapterDataObserver(new RecyclerView.AdapterDataObserver() {
            @Override
            public void onItemRangeInserted(int positionStart, int itemCount) {
                super.onItemRangeInserted(positionStart, itemCount);
                int messageCount = messageAdapter.getItemCount();
                int lastVisiblePosition = layoutManager.findLastCompletelyVisibleItemPosition();

                // Auto-scroll if user is at bottom
                if (lastVisiblePosition == -1 ||
                    (positionStart >= (messageCount - 1) && lastVisiblePosition == (positionStart - 1))) {
                    messagesRecyclerView.scrollToPosition(positionStart);
                }
            }
        });

        // Show/hide scroll to bottom button
        messagesRecyclerView.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
                int lastVisiblePosition = layoutManager.findLastCompletelyVisibleItemPosition();
                int totalItemCount = messageAdapter.getItemCount();

                if (lastVisiblePosition < totalItemCount - 3) {
                    scrollToBottomButton.show();
                } else {
                    scrollToBottomButton.hide();
                }
            }
        });
    }

    private void setupViewModel() {
        viewModel = new ViewModelProvider(this).get(ChatViewModel.class);

        // Load thread
        viewModel.loadThread(threadId);

        // Observe messages
        viewModel.getMessages(threadId).observe(this, messages -> {
            if (messages != null) {
                messageAdapter.setMessages(messages);

                // Mark as read
                viewModel.markThreadAsRead(threadId, currentUserId);
            }
        });

        // Observe thread data
        viewModel.getThread(threadId).observe(this, thread -> {
            if (thread != null) {
                updateThreadUI(thread);
            }
        });

        // Observe contact status
        viewModel.getContactStatus(contactId).observe(this, isOnline -> {
            if (isOnline != null && isOnline) {
                contactStatusText.setText("Online");
                contactStatusText.setTextColor(getColor(R.color.success));
            } else {
                contactStatusText.setText("Offline");
                contactStatusText.setTextColor(getColor(R.color.text_secondary));
            }
        });

        // Observe typing indicator
        viewModel.getTypingUsers(threadId).observe(this, typingUsers -> {
            if (typingUsers != null && typingUsers.contains(contactId)) {
                typingIndicator.setVisibility(View.VISIBLE);
            } else {
                typingIndicator.setVisibility(View.GONE);
            }
        });

        // Observe send status
        viewModel.getSendStatus().observe(this, status -> {
            if ("success".equals(status)) {
                messageInput.setText("");
            } else if ("error".equals(status)) {
                Toast.makeText(this, "Failed to send message", Toast.LENGTH_SHORT).show();
            }
        });
    }

    private void setupListeners() {
        // Send button
        sendButton.setOnClickListener(v -> sendMessage());

        // Attach button
        attachButton.setOnClickListener(v -> openAttachmentPicker());

        // Scroll to bottom button
        scrollToBottomButton.setOnClickListener(v -> scrollToBottom());

        // Message input - typing indicator
        messageInput.addTextChangedListener(new TextWatcher() {
            private boolean isTyping = false;

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                if (s.length() > 0) {
                    sendButton.setEnabled(true);

                    if (!isTyping) {
                        isTyping = true;
                        viewModel.sendTypingIndicator(threadId, contactId, true);
                    }
                } else {
                    sendButton.setEnabled(false);

                    if (isTyping) {
                        isTyping = false;
                        viewModel.sendTypingIndicator(threadId, contactId, false);
                    }
                }
            }

            @Override
            public void afterTextChanged(Editable s) {}
        });
    }

    private void sendMessage() {
        String content = messageInput.getText().toString().trim();
        if (content.isEmpty()) return;

        // Create message
        String messageId = UUID.randomUUID().toString();
        String primaryLanguage = PreferenceManager.getPrimaryLanguage(this);

        MessageEntity message = new MessageEntity(
            messageId,
            threadId,
            currentUserId,
            contactId,
            content,
            "text",
            primaryLanguage
        );

        // Send via ViewModel
        viewModel.sendMessage(message);

        // Clear input
        messageInput.setText("");
    }

    private void openAttachmentPicker() {
        // TODO: Implement attachment picker
        Toast.makeText(this, "Attachment picker coming soon", Toast.LENGTH_SHORT).show();
    }

    private void scrollToBottom() {
        if (messageAdapter.getItemCount() > 0) {
            messagesRecyclerView.smoothScrollToPosition(messageAdapter.getItemCount() - 1);
        }
    }

    private void openContactProfile() {
        Intent intent = new Intent(this, ProfileActivity.class);
        intent.putExtra("user_id", contactId);
        startActivity(intent);
    }

    private void updateThreadUI(ThreadEntity thread) {
        if (thread.getName() != null) {
            contactNameText.setText(thread.getName());
        }

        // Load avatar if available
        if (thread.getAvatarUrl() != null) {
            // TODO: Load with Glide/Picasso
            // Glide.with(this).load(thread.getAvatarUrl()).into(contactAvatar);
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        // Mark thread as read
        viewModel.markThreadAsRead(threadId, currentUserId);
    }

    @Override
    protected void onPause() {
        super.onPause();
        // Stop typing indicator
        viewModel.sendTypingIndicator(threadId, contactId, false);
    }
}
