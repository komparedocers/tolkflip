package com.tolkflip.chat.ui;

import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.chip.Chip;
import com.google.android.material.chip.ChipGroup;
import com.tolkflip.chat.R;
import com.tolkflip.chat.data.local.entity.UserEntity;
import com.tolkflip.chat.ui.viewmodel.ProfileViewModel;
import com.tolkflip.chat.util.PreferenceManager;

import java.util.ArrayList;
import java.util.List;

/**
 * ProfileActivity - User profile view and editing
 * Features: Avatar, display name, bio, languages, logout
 */
public class ProfileActivity extends AppCompatActivity {

    public static final String EXTRA_USER_ID = "user_id";
    private static final int MAX_BIO_LENGTH = 200;

    // UI Components
    private MaterialToolbar toolbar;
    private ImageView avatarImageView;
    private Button changeAvatarButton;
    private EditText displayNameEdit;
    private EditText bioEdit;
    private TextView bioCounterText;
    private TextView phoneNumberText;
    private ChipGroup languagesChipGroup;
    private Button addLanguageButton;
    private Button saveButton;
    private Button logoutButton;
    private ProgressBar loadingProgress;
    private View contentLayout;

    // Data
    private ProfileViewModel viewModel;
    private String userId;
    private String currentUserId;
    private boolean isOwnProfile;
    private Uri selectedAvatarUri;

    // Image picker
    private ActivityResultLauncher<String> imagePickerLauncher;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_profile);

        // Get user ID
        userId = getIntent().getStringExtra(EXTRA_USER_ID);
        currentUserId = PreferenceManager.getUserId(this);
        isOwnProfile = userId == null || userId.equals(currentUserId);

        if (isOwnProfile) {
            userId = currentUserId;
        }

        // Initialize image picker
        setupImagePicker();

        // Initialize UI
        initializeViews();
        setupToolbar();
        setupViewModel();
        setupListeners();

        // Load user data
        loadUserProfile();
    }

    private void setupImagePicker() {
        imagePickerLauncher = registerForActivityResult(
            new ActivityResultContracts.GetContent(),
            uri -> {
                if (uri != null) {
                    selectedAvatarUri = uri;
                    avatarImageView.setImageURI(uri);
                    // TODO: Upload to server
                }
            }
        );
    }

    private void initializeViews() {
        toolbar = findViewById(R.id.toolbar);
        avatarImageView = findViewById(R.id.avatar_image);
        changeAvatarButton = findViewById(R.id.change_avatar_button);
        displayNameEdit = findViewById(R.id.display_name_edit);
        bioEdit = findViewById(R.id.bio_edit);
        bioCounterText = findViewById(R.id.bio_counter_text);
        phoneNumberText = findViewById(R.id.phone_number_text);
        languagesChipGroup = findViewById(R.id.languages_chip_group);
        addLanguageButton = findViewById(R.id.add_language_button);
        saveButton = findViewById(R.id.save_button);
        logoutButton = findViewById(R.id.logout_button);
        loadingProgress = findViewById(R.id.loading_progress);
        contentLayout = findViewById(R.id.content_layout);

        // Hide edit controls if not own profile
        if (!isOwnProfile) {
            changeAvatarButton.setVisibility(View.GONE);
            displayNameEdit.setEnabled(false);
            bioEdit.setEnabled(false);
            addLanguageButton.setVisibility(View.GONE);
            saveButton.setVisibility(View.GONE);
            logoutButton.setVisibility(View.GONE);
        }
    }

    private void setupToolbar() {
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(isOwnProfile ? "My Profile" : "Profile");
        }

        toolbar.setNavigationOnClickListener(v -> onBackPressed());
    }

    private void setupViewModel() {
        viewModel = new ViewModelProvider(this).get(ProfileViewModel.class);

        // Observe user data
        viewModel.getUser(userId).observe(this, user -> {
            if (user != null) {
                updateUI(user);
            }
        });

        // Observe save status
        viewModel.getSaveStatus().observe(this, status -> {
            if ("success".equals(status)) {
                Toast.makeText(this, "Profile updated successfully", Toast.LENGTH_SHORT).show();
                finish();
            } else if ("error".equals(status)) {
                Toast.makeText(this, "Failed to update profile", Toast.LENGTH_SHORT).show();
            }
        });

        // Observe loading state
        viewModel.isLoading().observe(this, isLoading -> {
            loadingProgress.setVisibility(isLoading ? View.VISIBLE : View.GONE);
            contentLayout.setVisibility(isLoading ? View.GONE : View.VISIBLE);
        });
    }

    private void setupListeners() {
        // Change avatar
        changeAvatarButton.setOnClickListener(v -> {
            imagePickerLauncher.launch("image/*");
        });

        // Bio character counter
        bioEdit.addTextChangedListener(new android.text.TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                int remaining = MAX_BIO_LENGTH - s.length();
                bioCounterText.setText(remaining + " characters remaining");

                if (remaining < 0) {
                    bioCounterText.setTextColor(getColor(R.color.error));
                } else {
                    bioCounterText.setTextColor(getColor(R.color.text_secondary));
                }
            }

            @Override
            public void afterTextChanged(android.text.Editable s) {}
        });

        // Add language
        addLanguageButton.setOnClickListener(v -> {
            showLanguagePicker();
        });

        // Save button
        saveButton.setOnClickListener(v -> {
            saveProfile();
        });

        // Logout button
        logoutButton.setOnClickListener(v -> {
            handleLogout();
        });
    }

    private void loadUserProfile() {
        viewModel.loadUser(userId);
    }

    private void updateUI(UserEntity user) {
        // Display name
        displayNameEdit.setText(user.getDisplayName());

        // Bio
        if (user.getBio() != null) {
            bioEdit.setText(user.getBio());
        }

        // Phone number
        phoneNumberText.setText(user.getPhoneNumber());

        // Avatar
        if (user.getAvatarUrl() != null && !user.getAvatarUrl().isEmpty()) {
            // TODO: Load with Glide/Picasso
            // Glide.with(this).load(user.getAvatarUrl()).into(avatarImageView);
        }

        // Languages
        updateLanguageChips(user.getPrimaryLanguage(), user.getAdditionalLanguages());
    }

    private void updateLanguageChips(String primaryLanguage, List<String> additionalLanguages) {
        languagesChipGroup.removeAllViews();

        // Add primary language chip
        if (primaryLanguage != null) {
            Chip primaryChip = createLanguageChip(primaryLanguage, true);
            languagesChipGroup.addView(primaryChip);
        }

        // Add additional language chips
        if (additionalLanguages != null) {
            for (String lang : additionalLanguages) {
                Chip chip = createLanguageChip(lang, false);
                languagesChipGroup.addView(chip);
            }
        }
    }

    private Chip createLanguageChip(String language, boolean isPrimary) {
        Chip chip = new Chip(this);
        chip.setText(getLanguageName(language));

        if (isPrimary) {
            chip.setChipBackgroundColorResource(R.color.primary_600);
            chip.setTextColor(getColor(R.color.white));
            chip.setChipIcon(getDrawable(R.drawable.ic_star));
        } else {
            chip.setChipBackgroundColorResource(R.color.neutral_200);
            chip.setTextColor(getColor(R.color.text_primary));
        }

        if (isOwnProfile) {
            chip.setCloseIconVisible(!isPrimary);
            chip.setOnCloseIconClickListener(v -> {
                languagesChipGroup.removeView(chip);
            });
        }

        return chip;
    }

    private void showLanguagePicker() {
        // TODO: Implement language picker dialog
        String[] languages = {"English", "Spanish", "French", "German", "Chinese", "Japanese", "Korean", "Arabic"};
        String[] languageCodes = {"en", "es", "fr", "de", "zh", "ja", "ko", "ar"};

        android.app.AlertDialog.Builder builder = new android.app.AlertDialog.Builder(this);
        builder.setTitle("Add Language");
        builder.setItems(languages, (dialog, which) -> {
            Chip chip = createLanguageChip(languageCodes[which], false);
            languagesChipGroup.addView(chip);
        });
        builder.show();
    }

    private void saveProfile() {
        String displayName = displayNameEdit.getText().toString().trim();
        String bio = bioEdit.getText().toString().trim();

        if (displayName.isEmpty()) {
            Toast.makeText(this, "Display name cannot be empty", Toast.LENGTH_SHORT).show();
            return;
        }

        if (bio.length() > MAX_BIO_LENGTH) {
            Toast.makeText(this, "Bio is too long", Toast.LENGTH_SHORT).show();
            return;
        }

        // Get languages from chips
        List<String> languages = new ArrayList<>();
        for (int i = 1; i < languagesChipGroup.getChildCount(); i++) { // Skip primary
            Chip chip = (Chip) languagesChipGroup.getChildAt(i);
            languages.add(getLanguageCode(chip.getText().toString()));
        }

        // Save via ViewModel
        viewModel.updateProfile(userId, displayName, bio, selectedAvatarUri, languages);
    }

    private void handleLogout() {
        new android.app.AlertDialog.Builder(this)
            .setTitle("Logout")
            .setMessage("Are you sure you want to logout?")
            .setPositiveButton("Logout", (dialog, which) -> {
                // Clear session
                PreferenceManager.clearSession(this);

                // Clear database
                viewModel.clearAllData();

                // Redirect to login
                Intent intent = new Intent(this, MainActivity.class);
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
                startActivity(intent);
                finish();
            })
            .setNegativeButton("Cancel", null)
            .show();
    }

    private String getLanguageName(String code) {
        switch (code) {
            case "en": return "English";
            case "es": return "Spanish";
            case "fr": return "French";
            case "de": return "German";
            case "zh": return "Chinese";
            case "ja": return "Japanese";
            case "ko": return "Korean";
            case "ar": return "Arabic";
            case "pt": return "Portuguese";
            case "it": return "Italian";
            default: return code.toUpperCase();
        }
    }

    private String getLanguageCode(String name) {
        switch (name) {
            case "English": return "en";
            case "Spanish": return "es";
            case "French": return "fr";
            case "German": return "de";
            case "Chinese": return "zh";
            case "Japanese": return "ja";
            case "Korean": return "ko";
            case "Arabic": return "ar";
            case "Portuguese": return "pt";
            case "Italian": return "it";
            default: return name.toLowerCase();
        }
    }
}
