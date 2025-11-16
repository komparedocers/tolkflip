import Foundation
import RealmSwift

enum MessageType: String, PersistableEnum, Codable {
    case text
    case image
    case video
    case audio
    case file
    case voiceNote
}

enum MessageStatus: String, PersistableEnum, Codable {
    case sending
    case sent
    case delivered
    case read
    case failed
}

class Message: Object, Codable {
    @Persisted(primaryKey: true) var messageId: String = UUID().uuidString
    @Persisted var threadId: String = ""
    @Persisted var senderId: String = ""
    @Persisted var receiverId: String = ""
    @Persisted var content: String = ""
    @Persisted var translatedContent: String?
    @Persisted var originalLanguage: String = "en"
    @Persisted var targetLanguage: String?
    @Persisted var messageType: MessageType = .text
    @Persisted var status: MessageStatus = .sending
    @Persisted var timestamp: Date = Date()
    @Persisted var isEncrypted: Bool = false
    @Persisted var encryptedContent: Data?
    @Persisted var mediaUrls: List<String>
    @Persisted var emotion: String?
    @Persisted var showOriginal: Bool = false

    var displayContent: String {
        if showOriginal || translatedContent == nil || translatedContent!.isEmpty {
            return content
        }
        return translatedContent!
    }

    enum CodingKeys: String, CodingKey {
        case messageId, threadId, senderId, receiverId, content
        case translatedContent, originalLanguage, targetLanguage
        case messageType, status, timestamp, isEncrypted
        case encryptedContent, emotion, showOriginal
    }
}

class ChatThread: Object, Codable {
    @Persisted(primaryKey: true) var threadId: String = UUID().uuidString
    @Persisted var otherUserId: String = ""
    @Persisted var otherUserName: String = ""
    @Persisted var otherUserAvatar: String = ""
    @Persisted var lastMessage: String = ""
    @Persisted var lastMessageTime: Date = Date()
    @Persisted var unreadCount: Int = 0
    @Persisted var preferredLanguage: String?
    @Persisted var showOriginal: Bool = false
    @Persisted var enableEmotionDetection: Bool = true
    @Persisted var isArchived: Bool = false
}
