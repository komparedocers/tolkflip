import Foundation
import RealmSwift

/**
 * Manages offline message queue and synchronization for iOS
 */
class OfflineSyncManager {
    static let shared = OfflineSyncManager()

    private var realm: Realm?
    private let apiService = APIService.shared
    private let webSocketManager = WebSocketManager.shared

    private init() {
        setupRealm()
    }

    private func setupRealm() {
        do {
            realm = try Realm()
            print("Realm initialized for offline sync")
        } catch {
            print("Error initializing Realm: \(error)")
        }
    }

    // MARK: - Message Queueing

    /**
     * Queue a message for sending when online
     */
    func queueMessage(_ message: Message) {
        guard let realm = realm else { return }

        do {
            try realm.write {
                message.status = .pending
                message.syncStatus = .pending
                realm.add(message, update: .modified)
            }

            print("Message queued for sending: \(message.messageId)")

            // Try to send immediately if online
            if isOnline() {
                sendPendingMessages()
            } else {
                // Schedule background sync
                scheduleBackgroundSync()
            }
        } catch {
            print("Error queuing message: \(error)")
        }
    }

    /**
     * Send all pending messages
     */
    func sendPendingMessages() {
        guard let realm = realm else { return }

        let pendingMessages = realm.objects(Message.self)
            .filter("syncStatus == %@", SyncStatus.pending.rawValue)

        print("Sending \(pendingMessages.count) pending messages")

        for message in pendingMessages {
            sendMessage(message)
        }
    }

    private func sendMessage(_ message: Message) {
        guard let realm = realm else { return }

        // Mark as syncing
        do {
            try realm.write {
                message.syncStatus = .syncing
            }
        } catch {
            print("Error updating message status: \(error)")
            return
        }

        // Send via WebSocket
        webSocketManager.sendMessage(message) { success in
            guard let realm = self.realm else { return }

            do {
                try realm.write {
                    if success {
                        message.status = .sent
                        message.syncStatus = .synced
                        print("Message sent successfully: \(message.messageId)")
                    } else {
                        message.syncStatus = .pending
                        print("Failed to send message: \(message.messageId)")
                    }
                }
            } catch {
                print("Error updating message after send: \(error)")
            }
        }
    }

    // MARK: - Server Synchronization

    /**
     * Sync messages from server for a thread
     */
    func syncMessages(forThread threadId: String) {
        guard let realm = realm else { return }

        // Get last synced timestamp
        let lastMessage = realm.objects(Message.self)
            .filter("threadId == %@", threadId)
            .sorted(byKeyPath: "timestamp", ascending: false)
            .first

        let lastTimestamp = lastMessage?.timestamp ?? 0

        // Fetch new messages from server
        apiService.getMessages(threadId: threadId, since: lastTimestamp) { result in
            switch result {
            case .success(let messages):
                self.saveIncomingMessages(messages)
                print("Synced \(messages.count) messages from server")

            case .failure(let error):
                print("Failed to sync messages from server: \(error)")
            }
        }
    }

    /**
     * Sync all threads
     */
    func syncAllThreads() {
        apiService.getThreads { result in
            switch result {
            case .success(let threads):
                print("Synced threads from server")

                // Sync messages for each thread
                for thread in threads {
                    self.syncMessages(forThread: thread.threadId)
                }

            case .failure(let error):
                print("Failed to sync threads from server: \(error)")
            }
        }
    }

    /**
     * Save incoming messages to local database
     */
    private func saveIncomingMessages(_ messages: [Message]) {
        guard let realm = realm else { return }

        do {
            try realm.write {
                for message in messages {
                    message.syncStatus = .synced
                    realm.add(message, update: .modified)
                }
            }

            print("Saved \(messages.count) incoming messages")
        } catch {
            print("Error saving incoming messages: \(error)")
        }
    }

    // MARK: - Message Handling

    /**
     * Handle incoming message from WebSocket
     */
    func handleIncomingMessage(_ message: Message) {
        guard let realm = realm else { return }

        do {
            try realm.write {
                message.syncStatus = .synced
                realm.add(message, update: .modified)
            }

            print("Saved incoming message: \(message.messageId)")
        } catch {
            print("Error handling incoming message: \(error)")
        }
    }

    /**
     * Update message status (delivery, read receipts)
     */
    func updateMessageStatus(messageId: String, status: MessageStatus) {
        guard let realm = realm else { return }

        if let message = realm.objects(Message.self)
            .filter("messageId == %@", messageId).first {

            do {
                try realm.write {
                    message.status = status
                }

                print("Updated message status: \(messageId) -> \(status)")
            } catch {
                print("Error updating message status: \(error)")
            }
        }
    }

    /**
     * Delete local message
     */
    func deleteMessage(messageId: String) {
        guard let realm = realm else { return }

        if let message = realm.objects(Message.self)
            .filter("messageId == %@", messageId).first {

            do {
                try realm.write {
                    realm.delete(message)
                }

                print("Deleted message: \(messageId)")
            } catch {
                print("Error deleting message: \(error)")
            }
        }
    }

    /**
     * Clear all local data (for logout)
     */
    func clearAllData() {
        guard let realm = realm else { return }

        do {
            try realm.write {
                realm.deleteAll()
            }

            print("Cleared all local data")
        } catch {
            print("Error clearing local data: \(error)")
        }
    }

    // MARK: - Failed Messages

    /**
     * Retry failed messages
     */
    func retryFailedMessages() {
        guard let realm = realm else { return }

        let failedMessages = realm.objects(Message.self)
            .filter("syncStatus == %@", SyncStatus.failed.rawValue)

        print("Retrying \(failedMessages.count) failed messages")

        do {
            try realm.write {
                for message in failedMessages {
                    message.syncStatus = .pending
                }
            }

            sendPendingMessages()
        } catch {
            print("Error retrying failed messages: \(error)")
        }
    }

    // MARK: - Sync Statistics

    /**
     * Get sync statistics
     */
    func getSyncStats() -> SyncStats {
        guard let realm = realm else {
            return SyncStats(pendingCount: 0, failedCount: 0, syncedCount: 0)
        }

        let pendingCount = realm.objects(Message.self)
            .filter("syncStatus == %@", SyncStatus.pending.rawValue)
            .count

        let failedCount = realm.objects(Message.self)
            .filter("syncStatus == %@", SyncStatus.failed.rawValue)
            .count

        let syncedCount = realm.objects(Message.self)
            .filter("syncStatus == %@", SyncStatus.synced.rawValue)
            .count

        return SyncStats(
            pendingCount: pendingCount,
            failedCount: failedCount,
            syncedCount: syncedCount
        )
    }

    // MARK: - Helper Methods

    /**
     * Check if device is online
     */
    private func isOnline() -> Bool {
        return webSocketManager.isConnected
    }

    /**
     * Schedule background sync
     */
    private func scheduleBackgroundSync() {
        // Use BackgroundTasks framework for iOS 13+
        #if canImport(BackgroundTasks)
        if #available(iOS 13.0, *) {
            import BackgroundTasks

            let request = BGAppRefreshTaskRequest(identifier: "com.tolkflip.sync")
            request.earliestBeginDate = Date(timeIntervalSinceNow: 15 * 60) // 15 minutes

            do {
                try BGTaskScheduler.shared.submit(request)
                print("Scheduled background sync")
            } catch {
                print("Could not schedule background sync: \(error)")
            }
        }
        #endif
    }
}

// MARK: - Supporting Types

struct SyncStats {
    let pendingCount: Int
    let failedCount: Int
    let syncedCount: Int
}

enum SyncStatus: String {
    case pending = "pending"
    case syncing = "syncing"
    case synced = "synced"
    case failed = "failed"
}

enum MessageStatus: String {
    case pending = "pending"
    case sent = "sent"
    case delivered = "delivered"
    case read = "read"
    case failed = "failed"
}

// Realm Message Model Extension
extension Message {
    @objc dynamic var syncStatus: String = SyncStatus.pending.rawValue

    var syncStatusEnum: SyncStatus {
        get { return SyncStatus(rawValue: syncStatus) ?? .pending }
        set { syncStatus = newValue.rawValue }
    }

    var statusEnum: MessageStatus {
        get { return MessageStatus(rawValue: status) ?? .pending }
        set { status = newValue.rawValue }
    }
}
