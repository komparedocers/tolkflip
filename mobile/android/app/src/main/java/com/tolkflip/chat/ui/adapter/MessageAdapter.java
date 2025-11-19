package com.tolkflip.chat.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tolkflip.chat.R;
import com.tolkflip.chat.data.local.entity.MessageEntity;
import com.tolkflip.chat.util.PreferenceManager;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Adapter for displaying messages in chat
 */
public class MessageAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final int VIEW_TYPE_SENT = 1;
    private static final int VIEW_TYPE_RECEIVED = 2;

    private final Context context;
    private List<MessageEntity> messages;
    private final String currentUserId;
    private final String selectedLanguage;

    public MessageAdapter(Context context, List<MessageEntity> messages, String currentUserId) {
        this.context = context;
        this.messages = messages;
        this.currentUserId = currentUserId;
        this.selectedLanguage = PreferenceManager.getPrimaryLanguage(context);
    }

    @Override
    public int getItemViewType(int position) {
        MessageEntity message = messages.get(position);
        return message.getSenderId().equals(currentUserId) ? VIEW_TYPE_SENT : VIEW_TYPE_RECEIVED;
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == VIEW_TYPE_SENT) {
            View view = LayoutInflater.from(context)
                .inflate(R.layout.item_message_sent, parent, false);
            return new SentMessageViewHolder(view);
        } else {
            View view = LayoutInflater.from(context)
                .inflate(R.layout.item_message_received, parent, false);
            return new ReceivedMessageViewHolder(view);
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        MessageEntity message = messages.get(position);

        if (holder instanceof SentMessageViewHolder) {
            ((SentMessageViewHolder) holder).bind(message);
        } else if (holder instanceof ReceivedMessageViewHolder) {
            ((ReceivedMessageViewHolder) holder).bind(message);
        }
    }

    @Override
    public int getItemCount() {
        return messages.size();
    }

    public void setMessages(List<MessageEntity> messages) {
        this.messages = messages;
        notifyDataSetChanged();
    }

    // ViewHolder for sent messages
    class SentMessageViewHolder extends RecyclerView.ViewHolder {
        private final TextView messageText;
        private final TextView translationText;
        private final TextView timeText;
        private final TextView statusText;
        private final View translationContainer;

        SentMessageViewHolder(@NonNull View itemView) {
            super(itemView);
            messageText = itemView.findViewById(R.id.message_text);
            translationText = itemView.findViewById(R.id.translation_text);
            timeText = itemView.findViewById(R.id.message_time);
            statusText = itemView.findViewById(R.id.message_status);
            translationContainer = itemView.findViewById(R.id.translation_container);
        }

        void bind(MessageEntity message) {
            // Set message content
            messageText.setText(message.getContent());

            // Set timestamp
            timeText.setText(formatTime(message.getTimestamp()));

            // Set status
            String status = message.getStatus();
            if ("sent".equals(status)) {
                statusText.setText("✓");
            } else if ("delivered".equals(status)) {
                statusText.setText("✓✓");
            } else if ("read".equals(status)) {
                statusText.setText("✓✓");
                statusText.setTextColor(context.getColor(R.color.primary_600));
            } else if ("sending".equals(status)) {
                statusText.setText("⏱");
            } else if ("failed".equals(status)) {
                statusText.setText("❌");
            }

            // Show translation if available and different from original language
            Map<String, String> translations = message.getTranslations();
            if (translations != null && !message.getOriginalLanguage().equals(selectedLanguage)) {
                String translation = translations.get(selectedLanguage);
                if (translation != null && !translation.isEmpty()) {
                    translationText.setText(translation);
                    translationContainer.setVisibility(View.VISIBLE);
                } else {
                    translationContainer.setVisibility(View.GONE);
                }
            } else {
                translationContainer.setVisibility(View.GONE);
            }
        }
    }

    // ViewHolder for received messages
    class ReceivedMessageViewHolder extends RecyclerView.ViewHolder {
        private final TextView messageText;
        private final TextView translationText;
        private final TextView timeText;
        private final View translationContainer;

        ReceivedMessageViewHolder(@NonNull View itemView) {
            super(itemView);
            messageText = itemView.findViewById(R.id.message_text);
            translationText = itemView.findViewById(R.id.translation_text);
            timeText = itemView.findViewById(R.id.message_time);
            translationContainer = itemView.findViewById(R.id.translation_container);
        }

        void bind(MessageEntity message) {
            // Set message content
            messageText.setText(message.getContent());

            // Set timestamp
            timeText.setText(formatTime(message.getTimestamp()));

            // Show translation if available and different from original language
            Map<String, String> translations = message.getTranslations();
            if (translations != null && !message.getOriginalLanguage().equals(selectedLanguage)) {
                String translation = translations.get(selectedLanguage);
                if (translation != null && !translation.isEmpty()) {
                    translationText.setText("🌐 " + translation);
                    translationContainer.setVisibility(View.VISIBLE);
                } else {
                    translationContainer.setVisibility(View.GONE);
                }
            } else {
                translationContainer.setVisibility(View.GONE);
            }
        }
    }

    /**
     * Format timestamp to readable time
     */
    private String formatTime(long timestamp) {
        Date date = new Date(timestamp);
        Date now = new Date();
        long diff = now.getTime() - timestamp;

        // Less than 1 minute
        if (diff < 60000) {
            return "Just now";
        }

        // Less than 1 hour
        if (diff < 3600000) {
            int minutes = (int) (diff / 60000);
            return minutes + "m ago";
        }

        // Less than 24 hours
        if (diff < 86400000) {
            SimpleDateFormat sdf = new SimpleDateFormat("HH:mm", Locale.getDefault());
            return sdf.format(date);
        }

        // Less than 7 days
        if (diff < 604800000) {
            SimpleDateFormat sdf = new SimpleDateFormat("EEE HH:mm", Locale.getDefault());
            return sdf.format(date);
        }

        // Older
        SimpleDateFormat sdf = new SimpleDateFormat("MMM dd, HH:mm", Locale.getDefault());
        return sdf.format(date);
    }
}
