import Foundation
import Alamofire
import SwiftyJSON

class APIService {
    static let shared = APIService()

    private let baseURL: String
    private var session: Session

    private init() {
        #if DEBUG
        baseURL = "http://localhost:3000/api"
        #else
        baseURL = "https://api.tolkflip.com/api"
        #endif

        let configuration = URLSessionConfiguration.default
        configuration.timeoutIntervalForRequest = 30
        configuration.timeoutIntervalForResource = 30

        session = Session(configuration: configuration)
    }

    private func headers() -> HTTPHeaders {
        var headers: HTTPHeaders = [
            "Content-Type": "application/json",
            "Accept": "application/json"
        ]

        if let token = TokenManager.shared.accessToken {
            headers["Authorization"] = "Bearer \(token)"
        }

        return headers
    }

    // MARK: - Authentication

    func requestVerificationCode(phoneNumber: String, completion: @escaping (Result<JSON, Error>) -> Void) {
        let parameters: [String: Any] = ["phone_number": phoneNumber]

        session.request("\(baseURL)/auth/request-code",
                       method: .post,
                       parameters: parameters,
                       encoding: JSONEncoding.default,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    func verifyCode(phoneNumber: String, code: String, displayName: String?,
                   primaryLanguage: String?, completion: @escaping (Result<JSON, Error>) -> Void) {
        var parameters: [String: Any] = [
            "phone_number": phoneNumber,
            "code": code
        ]

        if let name = displayName {
            parameters["display_name"] = name
        }

        if let lang = primaryLanguage {
            parameters["primary_language"] = lang
        }

        session.request("\(baseURL)/auth/verify",
                       method: .post,
                       parameters: parameters,
                       encoding: JSONEncoding.default,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    // MARK: - User Management

    func getUserProfile(userId: String, completion: @escaping (Result<JSON, Error>) -> Void) {
        session.request("\(baseURL)/users/profile/\(userId)",
                       method: .get,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    func updateUserProfile(displayName: String?, avatarUrl: String?,
                          primaryLanguage: String?, additionalLanguages: [String]?,
                          completion: @escaping (Result<JSON, Error>) -> Void) {
        var parameters: [String: Any] = [:]

        if let name = displayName {
            parameters["display_name"] = name
        }

        if let avatar = avatarUrl {
            parameters["avatar_url"] = avatar
        }

        if let lang = primaryLanguage {
            parameters["primary_language"] = lang
        }

        if let langs = additionalLanguages {
            parameters["additional_languages"] = langs
        }

        session.request("\(baseURL)/users/profile",
                       method: .put,
                       parameters: parameters,
                       encoding: JSONEncoding.default,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    // MARK: - Chat

    func getChatThreads(completion: @escaping (Result<JSON, Error>) -> Void) {
        session.request("\(baseURL)/chat/threads",
                       method: .get,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    func getMessages(threadId: String, limit: Int = 50, pageState: String? = nil,
                    completion: @escaping (Result<JSON, Error>) -> Void) {
        var parameters: [String: Any] = ["limit": limit]

        if let state = pageState {
            parameters["pageState"] = state
        }

        session.request("\(baseURL)/chat/threads/\(threadId)/messages",
                       method: .get,
                       parameters: parameters,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    func updateThreadSettings(threadId: String, preferredLanguage: String?,
                             showOriginal: Bool?, enableEmotionDetection: Bool?,
                             completion: @escaping (Result<JSON, Error>) -> Void) {
        var parameters: [String: Any] = [:]

        if let lang = preferredLanguage {
            parameters["preferredLanguage"] = lang
        }

        if let show = showOriginal {
            parameters["showOriginal"] = show
        }

        if let emotion = enableEmotionDetection {
            parameters["enableEmotionDetection"] = emotion
        }

        session.request("\(baseURL)/chat/threads/\(threadId)/settings",
                       method: .post,
                       parameters: parameters,
                       encoding: JSONEncoding.default,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    // MARK: - Translation

    func translateText(text: String, sourceLanguage: String, targetLanguage: String,
                      detectEmotion: Bool = true, completion: @escaping (Result<JSON, Error>) -> Void) {
        let parameters: [String: Any] = [
            "text": text,
            "source_language": sourceLanguage,
            "target_language": targetLanguage,
            "detect_emotion": detectEmotion
        ]

        session.request("\(baseURL)/translate",
                       method: .post,
                       parameters: parameters,
                       encoding: JSONEncoding.default,
                       headers: headers())
            .validate()
            .responseData { response in
                self.handleResponse(response, completion: completion)
            }
    }

    // MARK: - Media Upload

    func uploadMedia(fileData: Data, mimeType: String, threadId: String, messageId: String,
                    completion: @escaping (Result<JSON, Error>) -> Void) {
        let url = "\(baseURL)/media/upload"

        session.upload(multipartFormData: { multipartFormData in
            multipartFormData.append(fileData, withName: "file",
                                   fileName: "media.\(self.fileExtension(for: mimeType))",
                                   mimeType: mimeType)

            if let threadData = threadId.data(using: .utf8) {
                multipartFormData.append(threadData, withName: "thread_id")
            }

            if let messageData = messageId.data(using: .utf8) {
                multipartFormData.append(messageData, withName: "message_id")
            }
        }, to: url, headers: headers())
        .validate()
        .responseData { response in
            self.handleResponse(response, completion: completion)
        }
    }

    // MARK: - Helper Methods

    private func handleResponse(_ response: AFDataResponse<Data>,
                              completion: @escaping (Result<JSON, Error>) -> Void) {
        switch response.result {
        case .success(let data):
            do {
                let json = try JSON(data: data)
                completion(.success(json))
            } catch {
                completion(.failure(error))
            }
        case .failure(let error):
            completion(.failure(error))
        }
    }

    private func fileExtension(for mimeType: String) -> String {
        switch mimeType {
        case "image/jpeg": return "jpg"
        case "image/png": return "png"
        case "image/gif": return "gif"
        case "video/mp4": return "mp4"
        case "audio/mpeg": return "mp3"
        case "audio/wav": return "wav"
        default: return "bin"
        }
    }
}
