package com.tolkflip.chat.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.tolkflip.chat.R;
import com.tolkflip.chat.data.local.entity.ThreadEntity;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;

/**
 * Adapter for displaying conversation threads/chats in list
 */
public class ThreadAdapter extends RecyclerView.Adapter<ThreadAdapter.ThreadViewHolder> {

    private final Context context;
    private List<ThreadEntity> threads;
    private OnThreadClickListener listener;

    public interface OnThreadClickListener {
        void onThreadClick(ThreadEntity thread);
        void onThreadLongClick(ThreadEntity thread);
    }

    public ThreadAdapter(Context context, List<ThreadEntity> threads, OnThreadClickListener listener) {
        this.context = context;
        this.threads = threads;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ThreadViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context)
            .inflate(R.layout.item_thread, parent, false);
        return new ThreadViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ThreadViewHolder holder, int position) {
        ThreadEntity thread = threads.get(position);
        holder.bind(thread);
    }

    @Override
    public int getItemCount() {
        return threads.size();
    }

    public void setThreads(List<ThreadEntity> threads) {
        this.threads = threads;
        notifyDataSetChanged();
    }

    public void updateThread(ThreadEntity thread) {
        for (int i = 0; i < threads.size(); i++) {
            if (threads.get(i).getThreadId().equals(thread.getThreadId())) {
                threads.set(i, thread);
                notifyItemChanged(i);
                break;
            }
        }
    }

    class ThreadViewHolder extends RecyclerView.ViewHolder {
        private final ImageView avatarImage;
        private final TextView nameText;
        private final TextView previewText;
        private final TextView timeText;
        private final TextView unreadBadge;
        private final View onlineIndicator;
        private final View mutedIndicator;
        private final View pinnedIndicator;

        ThreadViewHolder(@NonNull View itemView) {
            super(itemView);
            avatarImage = itemView.findViewById(R.id.thread_avatar);
            nameText = itemView.findViewById(R.id.thread_name);
            previewText = itemView.findViewById(R.id.thread_preview);
            timeText = itemView.findViewById(R.id.thread_time);
            unreadBadge = itemView.findViewById(R.id.unread_badge);
            onlineIndicator = itemView.findViewById(R.id.online_indicator);
            mutedIndicator = itemView.findViewById(R.id.muted_indicator);
            pinnedIndicator = itemView.findViewById(R.id.pinned_indicator);
        }

        void bind(ThreadEntity thread) {
            // Thread name
            nameText.setText(thread.getName() != null ? thread.getName() : "Unknown");

            // Last message preview
            String preview = thread.getLastMessagePreview();
            if (preview != null && !preview.isEmpty()) {
                previewText.setText(preview);
                previewText.setVisibility(View.VISIBLE);
            } else {
                previewText.setText("No messages yet");
                previewText.setVisibility(View.VISIBLE);
            }

            // Timestamp
            timeText.setText(formatTime(thread.getLastMessageTime()));

            // Unread badge
            int unreadCount = thread.getUnreadCount();
            if (unreadCount > 0) {
                unreadBadge.setVisibility(View.VISIBLE);
                unreadBadge.setText(unreadCount > 99 ? "99+" : String.valueOf(unreadCount));

                // Bold text for unread
                nameText.setTypeface(null, android.graphics.Typeface.BOLD);
                previewText.setTypeface(null, android.graphics.Typeface.BOLD);
            } else {
                unreadBadge.setVisibility(View.GONE);
                nameText.setTypeface(null, android.graphics.Typeface.NORMAL);
                previewText.setTypeface(null, android.graphics.Typeface.NORMAL);
            }

            // Online indicator (if available)
            // TODO: Connect with presence system
            onlineIndicator.setVisibility(View.GONE);

            // Muted indicator
            mutedIndicator.setVisibility(thread.isMuted() ? View.VISIBLE : View.GONE);

            // Pinned indicator
            pinnedIndicator.setVisibility(thread.isPinned() ? View.VISIBLE : View.GONE);

            // Avatar
            if (thread.getAvatarUrl() != null && !thread.getAvatarUrl().isEmpty()) {
                // TODO: Load with Glide/Picasso
                // Glide.with(context).load(thread.getAvatarUrl()).into(avatarImage);
            } else {
                // Show default avatar with first letter
                avatarImage.setImageResource(R.drawable.ic_default_avatar);
            }

            // Click listeners
            itemView.setOnClickListener(v -> {
                if (listener != null) {
                    listener.onThreadClick(thread);
                }
            });

            itemView.setOnLongClickListener(v -> {
                if (listener != null) {
                    listener.onThreadLongClick(thread);
                }
                return true;
            });

            // Highlight if pinned
            if (thread.isPinned()) {
                itemView.setBackgroundColor(context.getColor(R.color.neutral_50));
            } else {
                itemView.setBackgroundColor(context.getColor(R.color.white));
            }
        }

        private String formatTime(long timestamp) {
            Date date = new Date(timestamp);
            Date now = new Date();
            long diff = now.getTime() - timestamp;

            // Today
            if (diff < 86400000 && isSameDay(date, now)) {
                SimpleDateFormat sdf = new SimpleDateFormat("HH:mm", Locale.getDefault());
                return sdf.format(date);
            }

            // Yesterday
            if (diff < 172800000) {
                return "Yesterday";
            }

            // This week
            if (diff < 604800000) {
                SimpleDateFormat sdf = new SimpleDateFormat("EEE", Locale.getDefault());
                return sdf.format(date);
            }

            // Older
            SimpleDateFormat sdf = new SimpleDateFormat("MMM dd", Locale.getDefault());
            return sdf.format(date);
        }

        private boolean isSameDay(Date date1, Date date2) {
            SimpleDateFormat fmt = new SimpleDateFormat("yyyyMMdd", Locale.getDefault());
            return fmt.format(date1).equals(fmt.format(date2));
        }
    }
}
