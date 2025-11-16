import Foundation
import RealmSwift

class User: Object, Codable {
    @Persisted(primaryKey: true) var userId: String = ""
    @Persisted var phoneNumber: String = ""
    @Persisted var displayName: String = ""
    @Persisted var avatarUrl: String = ""
    @Persisted var primaryLanguage: String = "en"
    @Persisted var additionalLanguages: List<String>
    @Persisted var createdAt: Date = Date()
    @Persisted var lastActive: Date = Date()

    enum CodingKeys: String, CodingKey {
        case userId = "userId"
        case phoneNumber = "phoneNumber"
        case displayName = "displayName"
        case avatarUrl = "avatarUrl"
        case primaryLanguage = "primaryLanguage"
        case additionalLanguages = "additionalLanguages"
        case createdAt = "createdAt"
        case lastActive = "lastActive"
    }

    convenience init(userId: String, phoneNumber: String, displayName: String,
                    avatarUrl: String, primaryLanguage: String,
                    additionalLanguages: [String]) {
        self.init()
        self.userId = userId
        self.phoneNumber = phoneNumber
        self.displayName = displayName
        self.avatarUrl = avatarUrl
        self.primaryLanguage = primaryLanguage
        self.additionalLanguages.append(objectsIn: additionalLanguages)
    }
}
