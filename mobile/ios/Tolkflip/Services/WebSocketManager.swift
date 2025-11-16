import Foundation
import SocketIO

protocol WebSocketManagerDelegate: AnyObject {
    func webSocketDidConnect()
    func webSocketDidDisconnect()
    func webSocketDidReceiveMessage(_ message: Message)
    func webSocketDidReceiveTranslation(messageId: String, translatedContent: String,
                                       targetLanguage: String, emotion: String?)
    func webSocketDidReceiveTyping(userId: String, threadId: String, isTyping: Bool)
    func webSocketDidReceivePresenceUpdate(userId: String, status: String, timestamp: Int64)
    func webSocketDidReceiveError(_ error: String)
}

class WebSocketManager {
    static let shared = WebSocketManager()

    private var manager: SocketManager?
    private var socket: SocketIOClient?
    weak var delegate: WebSocketManagerDelegate?

    private init() {}

    func connect(token: String) {
        #if DEBUG
        let url = URL(string: "ws://localhost:3003")!
        #else
        let url = URL(string: "wss://api.tolkflip.com/api/chat")!
        #endif

        manager = SocketManager(socketURL: url, config: [
            .log(true),
            .compress,
            .reconnects(true),
            .reconnectAttempts(-1),
            .reconnectWait(1),
            .forceWebsockets(true),
            .connectParams(["token": token])
        ])

        socket = manager?.defaultSocket

        setupEventHandlers()
        socket?.connect()
    }

    func disconnect() {
        socket?.disconnect()
        socket = nil
        manager = nil
    }

    var isConnected: Bool {
        return socket?.status == .connected
    }

    // MARK: - Event Handlers

    private func setupEventHandlers() {
        socket?.on(clientEvent: .connect) { [weak self] _, _ in
            print("WebSocket connected")
            self?.delegate?.webSocketDidConnect()
        }

        socket?.on(clientEvent: .disconnect) { [weak self] _, _ in
            print("WebSocket disconnected")
            self?.delegate?.webSocketDidDisconnect()
        }

        socket?.on(clientEvent: .error) { [weak self] data, _ in
            print("WebSocket error: \(data)")
            if let error = data.first as? String {
                self?.delegate?.webSocketDidReceiveError(error)
            }
        }

        socket?.on("new_message") { [weak self] data, _ in
            self?.handleNewMessage(data)
        }

        socket?.on("message_sent") { [weak self] data, _ in
            self?.handleMessageSent(data)
        }

        socket?.on("message_read") { [weak self] data, _ in
            self?.handleMessageRead(data)
        }

        socket?.on("user_typing") { [weak self] data, _ in
            self?.handleUserTyping(data)
        }

        socket?.on("presence_update") { [weak self] data, _ in
            self?.handlePresenceUpdate(data)
        }

        socket?.on("message_translated") { [weak self] data, _ in
            self?.handleMessageTranslated(data)
        }
    }

    // MARK: - Send Events

    func sendMessage(_ message: Message) {
        guard isConnected else {
            print("Cannot send message: not connected")
            return
        }

        var data: [String: Any] = [
            "threadId": message.threadId,
            "receiverId": message.receiverId,
            "content": message.content,
            "messageType": message.messageType.rawValue,
            "originalLanguage": message.originalLanguage,
            "isGroup": false
        ]

        if !message.mediaUrls.isEmpty {
            data["mediaUrls"] = Array(message.mediaUrls)
        }

        if let encrypted = message.encryptedContent {
            data["encryptedContent"] = encrypted.base64EncodedString()
        }

        socket?.emit("send_message", data)
    }

    func markMessageRead(threadId: String, messageId: String) {
        guard isConnected else { return }

        socket?.emit("mark_read", [
            "threadId": threadId,
            "messageId": messageId
        ])
    }

    func setTyping(threadId: String, isTyping: Bool) {
        guard isConnected else { return }

        socket?.emit("typing", [
            "threadId": threadId,
            "isTyping": isTyping
        ])
    }

    func joinThread(threadId: String) {
        guard isConnected else { return }

        socket?.emit("join_thread", ["threadId": threadId])
    }

    func leaveThread(threadId: String) {
        guard isConnected else { return }

        socket?.emit("leave_thread", ["threadId": threadId])
    }

    // MARK: - Handle Incoming Events

    private func handleNewMessage(_ data: [Any]) {
        guard let dict = data.first as? [String: Any] else { return }

        let message = Message()
        message.messageId = dict["messageId"] as? String ?? UUID().uuidString
        message.threadId = dict["threadId"] as? String ?? ""
        message.senderId = dict["senderId"] as? String ?? ""
        message.receiverId = dict["receiverId"] as? String ?? ""
        message.content = dict["content"] as? String ?? ""
        message.originalLanguage = dict["originalLanguage"] as? String ?? "en"
        message.timestamp = Date(timeIntervalSince1970: (dict["timestamp"] as? Double ?? 0) / 1000)

        if let typeStr = dict["messageType"] as? String {
            message.messageType = MessageType(rawValue: typeStr) ?? .text
        }

        if let statusStr = dict["status"] as? String {
            message.status = MessageStatus(rawValue: statusStr) ?? .sent
        }

        delegate?.webSocketDidReceiveMessage(message)
    }

    private func handleMessageSent(_ data: [Any]) {
        // Handle message sent confirmation
    }

    private func handleMessageRead(_ data: [Any]) {
        // Handle message read receipt
    }

    private func handleUserTyping(_ data: [Any]) {
        guard let dict = data.first as? [String: Any],
              let userId = dict["userId"] as? String,
              let threadId = dict["threadId"] as? String,
              let isTyping = dict["isTyping"] as? Bool else { return }

        delegate?.webSocketDidReceiveTyping(userId: userId, threadId: threadId, isTyping: isTyping)
    }

    private func handlePresenceUpdate(_ data: [Any]) {
        guard let dict = data.first as? [String: Any],
              let userId = dict["userId"] as? String,
              let status = dict["status"] as? String,
              let timestamp = dict["timestamp"] as? Int64 else { return }

        delegate?.webSocketDidReceivePresenceUpdate(userId: userId, status: status,
                                                    timestamp: timestamp)
    }

    private func handleMessageTranslated(_ data: [Any]) {
        guard let dict = data.first as? [String: Any],
              let messageId = dict["messageId"] as? String,
              let translatedContent = dict["translatedContent"] as? String,
              let targetLanguage = dict["targetLanguage"] as? String else { return }

        let emotion = dict["emotion"] as? String

        delegate?.webSocketDidReceiveTranslation(messageId: messageId,
                                                translatedContent: translatedContent,
                                                targetLanguage: targetLanguage,
                                                emotion: emotion)
    }
}
