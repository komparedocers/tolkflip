/**
 * Tolkflip Chat Application - Main Chat Functionality
 * Real-time messaging with translation, WebSocket, and media support
 */

// API Configuration
const API_BASE_URL = 'http://localhost:3000/api/v1';
const WS_URL = 'ws://localhost:3001';

// Global State
const state = {
    userId: null,
    authToken: null,
    currentUser: null,
    currentThread: null,
    threads: [],
    messages: {},
    contacts: [],
    onlineUsers: new Set(),
    typingUsers: new Map(),
    socket: null,
    isConnected: false,
    selectedLanguage: 'en'
};

// DOM Elements
let elements = {};

// Initialize on DOM load
document.addEventListener('DOMContentLoaded', async () => {
    checkAuthentication();
    initializeElements();
    attachEventListeners();
    await loadUserProfile();
    initializeWebSocket();
    await loadThreads();
});

/**
 * Check if user is authenticated
 */
function checkAuthentication() {
    state.authToken = localStorage.getItem('authToken');
    state.userId = localStorage.getItem('userId');

    if (!state.authToken || !state.userId) {
        window.location.href = '/index.html';
        return;
    }
}

/**
 * Initialize DOM element references
 */
function initializeElements() {
    elements = {
        // Sidebar
        userAvatar: document.getElementById('user-avatar'),
        userName: document.getElementById('user-name'),
        userStatus: document.getElementById('user-status'),
        threadList: document.getElementById('thread-list'),
        searchInput: document.getElementById('search-input'),
        newChatBtn: document.getElementById('new-chat-btn'),
        settingsBtn: document.getElementById('settings-btn'),

        // Chat Area
        chatHeader: document.getElementById('chat-header'),
        contactName: document.getElementById('contact-name'),
        contactStatus: document.getElementById('contact-status'),
        messagesContainer: document.getElementById('messages-container'),
        messageInput: document.getElementById('message-input'),
        sendBtn: document.getElementById('send-btn'),
        attachBtn: document.getElementById('attach-btn'),
        voiceCallBtn: document.getElementById('voice-call-btn'),
        videoCallBtn: document.getElementById('video-call-btn'),

        // Empty State
        emptyState: document.getElementById('empty-state'),

        // Typing Indicator
        typingIndicator: document.getElementById('typing-indicator')
    };
}

/**
 * Attach event listeners
 */
function attachEventListeners() {
    // Search
    elements.searchInput.addEventListener('input', handleSearch);

    // Message input
    elements.messageInput.addEventListener('input', handleTyping);
    elements.messageInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter' && !e.shiftKey) {
            e.preventDefault();
            handleSendMessage();
        }
    });

    // Buttons
    elements.sendBtn.addEventListener('click', handleSendMessage);
    elements.attachBtn.addEventListener('click', handleAttachment);
    elements.voiceCallBtn.addEventListener('click', () => handleCall('voice'));
    elements.videoCallBtn.addEventListener('click', () => handleCall('video'));
    elements.newChatBtn.addEventListener('click', handleNewChat);
    elements.settingsBtn.addEventListener('click', handleSettings);

    // Logout
    const logoutBtn = document.getElementById('logout-btn');
    if (logoutBtn) {
        logoutBtn.addEventListener('click', handleLogout);
    }
}

/**
 * Load current user profile
 */
async function loadUserProfile() {
    try {
        const response = await apiRequest(`/user/${state.userId}`);
        state.currentUser = response.user;

        elements.userName.textContent = state.currentUser.display_name || 'User';
        state.selectedLanguage = state.currentUser.primary_language || 'en';

        // Update status to online
        await updatePresence('online');
    } catch (error) {
        console.error('Failed to load user profile:', error);
    }
}

/**
 * Initialize WebSocket connection
 */
function initializeWebSocket() {
    state.socket = io(WS_URL, {
        auth: {
            token: state.authToken
        },
        reconnection: true,
        reconnectionDelay: 1000,
        reconnectionAttempts: 5
    });

    // Connection events
    state.socket.on('connect', () => {
        console.log('WebSocket connected');
        state.isConnected = true;
        updateConnectionStatus(true);

        // Authenticate
        state.socket.emit('authenticate', {
            userId: state.userId,
            token: state.authToken
        });
    });

    state.socket.on('disconnect', () => {
        console.log('WebSocket disconnected');
        state.isConnected = false;
        updateConnectionStatus(false);
    });

    state.socket.on('authenticated', () => {
        console.log('WebSocket authenticated');
    });

    // Message events
    state.socket.on('message', handleIncomingMessage);
    state.socket.on('message_sent', handleMessageSent);
    state.socket.on('message_delivered', handleMessageDelivered);
    state.socket.on('message_read', handleMessageRead);

    // Typing events
    state.socket.on('typing', handleTypingEvent);
    state.socket.on('stop_typing', handleStopTypingEvent);

    // Presence events
    state.socket.on('user_online', handleUserOnline);
    state.socket.on('user_offline', handleUserOffline);

    // Call events
    state.socket.on('call_incoming', handleIncomingCall);
    state.socket.on('call_accepted', handleCallAccepted);
    state.socket.on('call_rejected', handleCallRejected);
}

/**
 * Load chat threads
 */
async function loadThreads() {
    try {
        const response = await apiRequest(`/threads/${state.userId}`);
        state.threads = response.threads || [];
        renderThreadList();
    } catch (error) {
        console.error('Failed to load threads:', error);
    }
}

/**
 * Render thread list
 */
function renderThreadList() {
    if (state.threads.length === 0) {
        elements.threadList.innerHTML = `
            <div class="empty-threads">
                <p>No conversations yet</p>
                <button onclick="handleNewChat()">Start a chat</button>
            </div>
        `;
        return;
    }

    elements.threadList.innerHTML = state.threads.map(thread => `
        <div class="thread-item ${thread.thread_id === state.currentThread?.thread_id ? 'active' : ''}"
             data-thread-id="${thread.thread_id}"
             onclick="selectThread('${thread.thread_id}')">
            <div class="thread-avatar">
                <img src="${thread.avatar_url || '/assets/default-avatar.png'}" alt="${thread.name}">
                ${state.onlineUsers.has(thread.other_participant_id) ? '<div class="online-indicator"></div>' : ''}
            </div>
            <div class="thread-info">
                <div class="thread-header">
                    <h4>${thread.name || 'Unknown'}</h4>
                    <span class="thread-time">${formatTime(thread.last_message_time)}</span>
                </div>
                <div class="thread-preview">
                    <p>${thread.last_message_preview || 'No messages yet'}</p>
                    ${thread.unread_count > 0 ? `<span class="unread-badge">${thread.unread_count}</span>` : ''}
                </div>
            </div>
        </div>
    `).join('');
}

/**
 * Select a thread and load messages
 */
async function selectThread(threadId) {
    const thread = state.threads.find(t => t.thread_id === threadId);
    if (!thread) return;

    state.currentThread = thread;
    elements.emptyState.style.display = 'none';
    elements.chatHeader.style.display = 'flex';

    // Update UI
    elements.contactName.textContent = thread.name || 'Unknown';
    updateContactStatus(thread.other_participant_id);

    // Load messages
    await loadMessages(threadId);

    // Mark as read
    await markThreadAsRead(threadId);

    // Re-render thread list to update active state
    renderThreadList();
}

/**
 * Load messages for a thread
 */
async function loadMessages(threadId) {
    try {
        const response = await apiRequest(`/messages/${threadId}`);
        state.messages[threadId] = response.messages || [];
        renderMessages();
    } catch (error) {
        console.error('Failed to load messages:', error);
    }
}

/**
 * Render messages in chat area
 */
function renderMessages() {
    if (!state.currentThread) return;

    const messages = state.messages[state.currentThread.thread_id] || [];

    if (messages.length === 0) {
        elements.messagesContainer.innerHTML = `
            <div class="empty-messages">
                <p>No messages yet. Start the conversation!</p>
            </div>
        `;
        return;
    }

    elements.messagesContainer.innerHTML = messages.map(msg => {
        const isSent = msg.sender_id === state.userId;
        const translations = msg.translations || {};
        const hasTranslation = translations[state.selectedLanguage] &&
                              msg.original_language !== state.selectedLanguage;

        return `
            <div class="message ${isSent ? 'sent' : 'received'}" data-message-id="${msg.message_id}">
                <div class="message-bubble">
                    <div class="message-content">
                        ${msg.content}
                    </div>
                    ${hasTranslation ? `
                        <div class="message-translation">
                            <span class="translation-icon">🌐</span>
                            ${translations[state.selectedLanguage]}
                        </div>
                    ` : ''}
                    <div class="message-meta">
                        <span class="message-time">${formatTime(msg.timestamp)}</span>
                        ${isSent ? `
                            <span class="message-status">
                                ${msg.status === 'sent' ? '✓' : msg.status === 'delivered' ? '✓✓' : '✓✓'}
                            </span>
                        ` : ''}
                    </div>
                </div>
            </div>
        `;
    }).join('');

    // Scroll to bottom
    scrollToBottom();
}

/**
 * Handle send message
 */
async function handleSendMessage() {
    const content = elements.messageInput.value.trim();
    if (!content || !state.currentThread) return;

    const tempMessageId = Date.now().toString();
    const message = {
        message_id: tempMessageId,
        thread_id: state.currentThread.thread_id,
        sender_id: state.userId,
        receiver_id: state.currentThread.other_participant_id,
        content: content,
        message_type: 'text',
        original_language: state.selectedLanguage,
        timestamp: Date.now(),
        status: 'sending'
    };

    // Add to local messages immediately
    if (!state.messages[state.currentThread.thread_id]) {
        state.messages[state.currentThread.thread_id] = [];
    }
    state.messages[state.currentThread.thread_id].push(message);

    // Clear input
    elements.messageInput.value = '';

    // Render optimistically
    renderMessages();

    // Send via WebSocket
    if (state.isConnected) {
        state.socket.emit('send_message', {
            threadId: message.thread_id,
            receiverId: message.receiver_id,
            content: message.content,
            messageType: 'text',
            originalLanguage: state.selectedLanguage
        });
    } else {
        // Fallback to HTTP API
        try {
            const response = await apiRequest('/messages', 'POST', {
                thread_id: message.thread_id,
                receiver_id: message.receiver_id,
                content: message.content,
                message_type: 'text',
                original_language: state.selectedLanguage
            });

            // Update message with real ID
            const msgIndex = state.messages[state.currentThread.thread_id]
                .findIndex(m => m.message_id === tempMessageId);
            if (msgIndex !== -1) {
                state.messages[state.currentThread.thread_id][msgIndex] = response.message;
            }

            renderMessages();
        } catch (error) {
            console.error('Failed to send message:', error);
            // Mark message as failed
            const msgIndex = state.messages[state.currentThread.thread_id]
                .findIndex(m => m.message_id === tempMessageId);
            if (msgIndex !== -1) {
                state.messages[state.currentThread.thread_id][msgIndex].status = 'failed';
                renderMessages();
            }
        }
    }
}

/**
 * Handle incoming message from WebSocket
 */
function handleIncomingMessage(data) {
    const message = data.message;

    // Add to messages
    if (!state.messages[message.thread_id]) {
        state.messages[message.thread_id] = [];
    }
    state.messages[message.thread_id].push(message);

    // Update thread preview
    const thread = state.threads.find(t => t.thread_id === message.thread_id);
    if (thread) {
        thread.last_message_preview = message.content;
        thread.last_message_time = message.timestamp;
        if (message.thread_id !== state.currentThread?.thread_id) {
            thread.unread_count = (thread.unread_count || 0) + 1;
        }
    }

    // Re-render if current thread
    if (state.currentThread?.thread_id === message.thread_id) {
        renderMessages();
        markThreadAsRead(message.thread_id);
    }

    // Re-render thread list
    renderThreadList();

    // Play notification sound
    playNotificationSound();
}

/**
 * Handle message sent confirmation
 */
function handleMessageSent(data) {
    const { threadId, tempMessageId, message } = data;

    const messages = state.messages[threadId];
    if (messages) {
        const msgIndex = messages.findIndex(m => m.message_id === tempMessageId);
        if (msgIndex !== -1) {
            messages[msgIndex] = message;
            if (state.currentThread?.thread_id === threadId) {
                renderMessages();
            }
        }
    }
}

/**
 * Handle typing event
 */
let typingTimeout;
function handleTyping() {
    if (!state.currentThread || !state.isConnected) return;

    // Emit typing event
    state.socket.emit('typing', {
        receiverId: state.currentThread.other_participant_id,
        threadId: state.currentThread.thread_id
    });

    // Clear previous timeout
    clearTimeout(typingTimeout);

    // Stop typing after 3 seconds
    typingTimeout = setTimeout(() => {
        state.socket.emit('stop_typing', {
            receiverId: state.currentThread.other_participant_id,
            threadId: state.currentThread.thread_id
        });
    }, 3000);
}

/**
 * Handle typing event from other user
 */
function handleTypingEvent(data) {
    if (state.currentThread?.other_participant_id === data.userId) {
        state.typingUsers.set(data.userId, true);
        updateTypingIndicator();
    }
}

/**
 * Handle stop typing event
 */
function handleStopTypingEvent(data) {
    state.typingUsers.delete(data.userId);
    updateTypingIndicator();
}

/**
 * Update typing indicator
 */
function updateTypingIndicator() {
    if (state.typingUsers.size > 0) {
        elements.typingIndicator.style.display = 'flex';
    } else {
        elements.typingIndicator.style.display = 'none';
    }
}

/**
 * Handle user online event
 */
function handleUserOnline(data) {
    state.onlineUsers.add(data.userId);
    updateContactStatus(data.userId);
    renderThreadList();
}

/**
 * Handle user offline event
 */
function handleUserOffline(data) {
    state.onlineUsers.delete(data.userId);
    updateContactStatus(data.userId);
    renderThreadList();
}

/**
 * Update contact status
 */
function updateContactStatus(userId) {
    if (state.currentThread?.other_participant_id === userId) {
        const isOnline = state.onlineUsers.has(userId);
        elements.contactStatus.textContent = isOnline ? 'Online' : 'Offline';
        elements.contactStatus.className = isOnline ? 'online' : 'offline';
    }
}

/**
 * Handle search
 */
function handleSearch(e) {
    const query = e.target.value.toLowerCase();

    const filtered = state.threads.filter(thread =>
        thread.name.toLowerCase().includes(query) ||
        thread.last_message_preview.toLowerCase().includes(query)
    );

    // Render filtered threads (simplified, could be improved)
    renderThreadList();
}

/**
 * Handle new chat
 */
function handleNewChat() {
    // TODO: Implement contact picker modal
    alert('Contact picker not implemented yet');
}

/**
 * Handle attachment
 */
function handleAttachment() {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = 'image/*,video/*,audio/*';
    input.onchange = async (e) => {
        const file = e.target.files[0];
        if (file) {
            await uploadAndSendMedia(file);
        }
    };
    input.click();
}

/**
 * Upload and send media file
 */
async function uploadAndSendMedia(file) {
    try {
        const formData = new FormData();
        formData.append('file', file);
        formData.append('userId', state.userId);

        const response = await fetch(`${API_BASE_URL}/media/upload`, {
            method: 'POST',
            headers: {
                'Authorization': `Bearer ${state.authToken}`
            },
            body: formData
        });

        const data = await response.json();

        if (data.success) {
            // Send media message
            state.socket.emit('send_message', {
                threadId: state.currentThread.thread_id,
                receiverId: state.currentThread.other_participant_id,
                content: data.url,
                messageType: file.type.startsWith('image/') ? 'image' :
                           file.type.startsWith('video/') ? 'video' : 'file',
                originalLanguage: state.selectedLanguage
            });
        }
    } catch (error) {
        console.error('Failed to upload media:', error);
    }
}

/**
 * Handle call (voice/video)
 */
function handleCall(type) {
    if (!state.currentThread) return;

    // Emit call request
    state.socket.emit('call_request', {
        receiverId: state.currentThread.other_participant_id,
        callType: type
    });

    alert(`${type} call feature coming soon!`);
}

/**
 * Handle settings
 */
function handleSettings() {
    alert('Settings modal not implemented yet');
}

/**
 * Handle logout
 */
function handleLogout() {
    // Update presence to offline
    updatePresence('offline');

    // Disconnect WebSocket
    if (state.socket) {
        state.socket.disconnect();
    }

    // Clear localStorage
    localStorage.removeItem('authToken');
    localStorage.removeItem('refreshToken');
    localStorage.removeItem('userId');
    localStorage.removeItem('phoneNumber');

    // Redirect to login
    window.location.href = '/index.html';
}

/**
 * Update user presence
 */
async function updatePresence(status) {
    try {
        await apiRequest('/presence/update', 'POST', {
            userId: state.userId,
            status: status
        });
    } catch (error) {
        console.error('Failed to update presence:', error);
    }
}

/**
 * Mark thread as read
 */
async function markThreadAsRead(threadId) {
    try {
        await apiRequest(`/threads/${threadId}/read`, 'POST', {
            userId: state.userId
        });

        // Update local state
        const thread = state.threads.find(t => t.thread_id === threadId);
        if (thread) {
            thread.unread_count = 0;
            renderThreadList();
        }
    } catch (error) {
        console.error('Failed to mark thread as read:', error);
    }
}

/**
 * Update connection status UI
 */
function updateConnectionStatus(isConnected) {
    const statusElement = document.getElementById('connection-status');
    if (statusElement) {
        statusElement.textContent = isConnected ? 'Connected' : 'Disconnected';
        statusElement.className = isConnected ? 'connected' : 'disconnected';
    }
}

/**
 * Scroll messages to bottom
 */
function scrollToBottom() {
    elements.messagesContainer.scrollTop = elements.messagesContainer.scrollHeight;
}

/**
 * Play notification sound
 */
function playNotificationSound() {
    const audio = new Audio('/assets/notification.mp3');
    audio.volume = 0.5;
    audio.play().catch(() => {
        // Autoplay might be blocked
    });
}

/**
 * Format timestamp to readable time
 */
function formatTime(timestamp) {
    const date = new Date(timestamp);
    const now = new Date();
    const diffMs = now - date;
    const diffMins = Math.floor(diffMs / 60000);
    const diffHours = Math.floor(diffMs / 3600000);
    const diffDays = Math.floor(diffMs / 86400000);

    if (diffMins < 1) return 'Just now';
    if (diffMins < 60) return `${diffMins}m ago`;
    if (diffHours < 24) return `${diffHours}h ago`;
    if (diffDays < 7) return `${diffDays}d ago`;

    return date.toLocaleDateString();
}

/**
 * API request helper
 */
async function apiRequest(endpoint, method = 'GET', body = null) {
    const options = {
        method,
        headers: {
            'Content-Type': 'application/json',
            'Authorization': `Bearer ${state.authToken}`
        }
    };

    if (body) {
        options.body = JSON.stringify(body);
    }

    const response = await fetch(`${API_BASE_URL}${endpoint}`, options);

    if (response.status === 401) {
        // Token expired, try to refresh
        const refreshed = await refreshToken();
        if (refreshed) {
            // Retry request
            options.headers['Authorization'] = `Bearer ${state.authToken}`;
            return apiRequest(endpoint, method, body);
        } else {
            // Refresh failed, logout
            handleLogout();
            throw new Error('Authentication failed');
        }
    }

    return await response.json();
}

/**
 * Refresh auth token
 */
async function refreshToken() {
    try {
        const refreshToken = localStorage.getItem('refreshToken');
        const response = await fetch(`${API_BASE_URL}/auth/refresh`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ refresh_token: refreshToken })
        });

        const data = await response.json();

        if (data.success) {
            state.authToken = data.access_token;
            localStorage.setItem('authToken', data.access_token);
            return true;
        }

        return false;
    } catch (error) {
        console.error('Failed to refresh token:', error);
        return false;
    }
}
