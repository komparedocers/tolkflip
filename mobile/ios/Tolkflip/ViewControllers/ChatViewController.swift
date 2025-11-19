import UIKit
import Combine

/**
 * ChatViewController - Main chat interface
 * Features: Message list, sending messages, typing indicators, translations
 */
class ChatViewController: UIViewController {

    // MARK: - UI Components

    private let tableView: UITableView = {
        let table = UITableView(frame: .zero, style: .plain)
        table.backgroundColor = Color.neutral50
        table.separatorStyle = .none
        table.allowsSelection = false
        table.translatesAutoresizingMaskIntoConstraints = false
        return table
    }()

    private let inputContainerView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOpacity = 0.1
        view.layer.shadowRadius = 4
        view.layer.shadowOffset = CGSize(width: 0, height: -2)
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }()

    private let messageTextView: UITextView = {
        let textView = UITextView()
        textView.font = Typography.bodyLarge
        textView.textColor = Color.textPrimary
        textView.backgroundColor = Color.neutral100
        textView.layer.cornerRadius = CornerRadius.md
        textView.textContainerInset = UIEdgeInsets(top: 8, left: 12, bottom: 8, right: 12)
        textView.isScrollEnabled = false
        textView.translatesAutoresizingMaskIntoConstraints = false
        return textView
    }()

    private let placeholderLabel: UILabel = {
        let label = UILabel()
        label.text = "Type a message"
        label.font = Typography.bodyLarge
        label.textColor = Color.textSecondary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let sendButton: UIButton = {
        let button = UIButton(type: .system)
        button.setImage(UIImage(systemName: "arrow.up.circle.fill"), for: .normal)
        button.tintColor = Color.primary600
        button.contentVerticalAlignment = .fill
        button.contentHorizontalAlignment = .fill
        button.translatesAutoresizingMaskIntoConstraints = false
        button.widthAnchor.constraint(equalToConstant: 40).isActive = true
        button.heightAnchor.constraint(equalToConstant: 40).isActive = true
        button.isEnabled = false
        return button
    }()

    private let attachButton: UIButton = {
        let button = UIButton(type: .system)
        button.setImage(UIImage(systemName: "paperclip"), for: .normal)
        button.tintColor = Color.textSecondary
        button.translatesAutoresizingMaskIntoConstraints = false
        button.widthAnchor.constraint(equalToConstant: 40).isActive = true
        button.heightAnchor.constraint(equalToConstant: 40).isActive = true
        return button
    }()

    private let typingIndicatorView: UIView = {
        let view = UIView()
        view.backgroundColor = Color.primary50
        view.translatesAutoresizingMaskIntoConstraints = false
        view.isHidden = true
        return view
    }()

    private let typingLabel: UILabel = {
        let label = UILabel()
        label.text = "Typing..."
        label.font = Typography.bodySmall
        label.textColor = Color.primary600
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    // MARK: - Properties

    private var cancellables = Set<AnyCancellable>()
    private var messages: [Message] = []
    private let thread: Thread
    private var inputContainerBottomConstraint: NSLayoutConstraint!
    private var messageTextViewHeightConstraint: NSLayoutConstraint!
    private var isTyping = false
    private var typingTimer: Timer?

    // MARK: - Initialization

    init(thread: Thread) {
        self.thread = thread
        super.init(nibName: nil, bundle: nil)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    // MARK: - Lifecycle

    override func viewDidLoad() {
        super.viewDidLoad()
        setupUI()
        setupTableView()
        setupActions()
        setupKeyboardObservers()
        loadMessages()
    }

    override func viewWillDisappear(_ animated: Bool) {
        super.viewWillDisappear(animated)
        stopTypingIndicator()
    }

    // MARK: - Setup

    private func setupUI() {
        title = thread.name
        view.backgroundColor = Color.neutral50

        navigationItem.rightBarButtonItems = [
            UIBarButtonItem(image: UIImage(systemName: "video"), style: .plain, target: self, action: #selector(startVideoCall)),
            UIBarButtonItem(image: UIImage(systemName: "phone"), style: .plain, target: self, action: #selector(startVoiceCall))
        ]

        view.addSubview(tableView)
        view.addSubview(typingIndicatorView)
        view.addSubview(inputContainerView)

        typingIndicatorView.addSubview(typingLabel)

        inputContainerView.addSubview(attachButton)
        inputContainerView.addSubview(messageTextView)
        inputContainerView.addSubview(placeholderLabel)
        inputContainerView.addSubview(sendButton)

        setupConstraints()
    }

    private func setupConstraints() {
        messageTextViewHeightConstraint = messageTextView.heightAnchor.constraint(equalToConstant: 40)
        inputContainerBottomConstraint = inputContainerView.bottomAnchor.constraint(equalTo: view.bottomAnchor)

        NSLayoutConstraint.activate([
            tableView.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor),
            tableView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            tableView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            tableView.bottomAnchor.constraint(equalTo: typingIndicatorView.topAnchor),

            typingIndicatorView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            typingIndicatorView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            typingIndicatorView.bottomAnchor.constraint(equalTo: inputContainerView.topAnchor),
            typingIndicatorView.heightAnchor.constraint(equalToConstant: 32),

            typingLabel.leadingAnchor.constraint(equalTo: typingIndicatorView.leadingAnchor, constant: Spacing.md),
            typingLabel.centerYAnchor.constraint(equalTo: typingIndicatorView.centerYAnchor),

            inputContainerView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            inputContainerView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            inputContainerBottomConstraint,
            inputContainerView.heightAnchor.constraint(greaterThanOrEqualToConstant: 60),

            attachButton.leadingAnchor.constraint(equalTo: inputContainerView.leadingAnchor, constant: Spacing.sm),
            attachButton.bottomAnchor.constraint(equalTo: inputContainerView.safeAreaLayoutGuide.bottomAnchor, constant: -Spacing.sm),

            messageTextView.leadingAnchor.constraint(equalTo: attachButton.trailingAnchor, constant: Spacing.sm),
            messageTextView.topAnchor.constraint(equalTo: inputContainerView.topAnchor, constant: Spacing.sm),
            messageTextView.bottomAnchor.constraint(equalTo: inputContainerView.safeAreaLayoutGuide.bottomAnchor, constant: -Spacing.sm),
            messageTextView.trailingAnchor.constraint(equalTo: sendButton.leadingAnchor, constant: -Spacing.sm),
            messageTextViewHeightConstraint,

            placeholderLabel.leadingAnchor.constraint(equalTo: messageTextView.leadingAnchor, constant: 16),
            placeholderLabel.centerYAnchor.constraint(equalTo: messageTextView.centerYAnchor),

            sendButton.trailingAnchor.constraint(equalTo: inputContainerView.trailingAnchor, constant: -Spacing.sm),
            sendButton.bottomAnchor.constraint(equalTo: inputContainerView.safeAreaLayoutGuide.bottomAnchor, constant: -Spacing.sm)
        ])
    }

    private func setupTableView() {
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(MessageSentCell.self, forCellReuseIdentifier: "MessageSentCell")
        tableView.register(MessageReceivedCell.self, forCellReuseIdentifier: "MessageReceivedCell")
        tableView.estimatedRowHeight = 80
        tableView.rowHeight = UITableView.automaticDimension
    }

    private func setupActions() {
        messageTextView.delegate = self

        sendButton.addTarget(self, action: #selector(handleSend), for: .touchUpInside)
        attachButton.addTarget(self, action: #selector(handleAttach), for: .touchUpInside)
    }

    private func setupKeyboardObservers() {
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillShow), name: UIResponder.keyboardWillShowNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(keyboardWillHide), name: UIResponder.keyboardWillHideNotification, object: nil)
    }

    // MARK: - Data Loading

    private func loadMessages() {
        // TODO: Load messages from database/API
        // For demo, empty array
        messages = []

        tableView.reloadData()
        scrollToBottom(animated: false)
    }

    // MARK: - Actions

    @objc private func handleSend() {
        guard let text = messageTextView.text, !text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty else {
            return
        }

        let message = Message(
            id: UUID().uuidString,
            threadId: thread.id,
            senderId: "currentUserId",  // TODO: Get from session
            content: text,
            timestamp: Date(),
            status: .sending
        )

        messages.append(message)
        tableView.reloadData()
        scrollToBottom(animated: true)

        // Clear input
        messageTextView.text = ""
        updateSendButton()
        updatePlaceholder()
        updateTextViewHeight()

        // TODO: Send message via API/WebSocket
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) { [weak self] in
            if let index = self?.messages.firstIndex(where: { $0.id == message.id }) {
                self?.messages[index].status = .sent
                self?.tableView.reloadRows(at: [IndexPath(row: index, section: 0)], with: .none)
            }
        }

        stopTypingIndicator()
    }

    @objc private func handleAttach() {
        let alert = UIAlertController(title: "Attach", message: "Choose attachment type", preferredStyle: .actionSheet)

        alert.addAction(UIAlertAction(title: "Photo", style: .default) { [weak self] _ in
            self?.showImagePicker()
        })

        alert.addAction(UIAlertAction(title: "Camera", style: .default) { [weak self] _ in
            self?.showCamera()
        })

        alert.addAction(UIAlertAction(title: "Document", style: .default) { [weak self] _ in
            self?.showDocumentPicker()
        })

        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))

        present(alert, animated: true)
    }

    @objc private func startVoiceCall() {
        // TODO: Implement voice call
        let alert = UIAlertController(title: "Voice Call", message: "Voice call feature coming soon", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }

    @objc private func startVideoCall() {
        // TODO: Implement video call
        let alert = UIAlertController(title: "Video Call", message: "Video call feature coming soon", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }

    private func showImagePicker() {
        // TODO: Implement image picker
    }

    private func showCamera() {
        // TODO: Implement camera
    }

    private func showDocumentPicker() {
        // TODO: Implement document picker
    }

    private func updateSendButton() {
        let hasText = !(messageTextView.text.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty)
        sendButton.isEnabled = hasText
        sendButton.tintColor = hasText ? Color.primary600 : Color.textSecondary
    }

    private func updatePlaceholder() {
        placeholderLabel.isHidden = !messageTextView.text.isEmpty
    }

    private func updateTextViewHeight() {
        let size = messageTextView.sizeThatFits(CGSize(width: messageTextView.frame.width, height: .infinity))
        let newHeight = min(max(40, size.height), 120)

        if messageTextViewHeightConstraint.constant != newHeight {
            messageTextViewHeightConstraint.constant = newHeight
            UIView.animate(withDuration: 0.2) {
                self.view.layoutIfNeeded()
            }
        }
    }

    private func scrollToBottom(animated: Bool) {
        guard !messages.isEmpty else { return }

        let indexPath = IndexPath(row: messages.count - 1, section: 0)
        tableView.scrollToRow(at: indexPath, at: .bottom, animated: animated)
    }

    private func startTypingIndicator() {
        guard !isTyping else { return }

        isTyping = true
        // TODO: Send typing indicator to server

        // Auto-stop after 3 seconds
        typingTimer?.invalidate()
        typingTimer = Timer.scheduledTimer(withTimeInterval: 3.0, repeats: false) { [weak self] _ in
            self?.stopTypingIndicator()
        }
    }

    private func stopTypingIndicator() {
        guard isTyping else { return }

        isTyping = false
        typingTimer?.invalidate()
        // TODO: Send stop typing to server
    }

    // MARK: - Keyboard Handling

    @objc private func keyboardWillShow(_ notification: Notification) {
        guard let keyboardFrame = notification.userInfo?[UIResponder.keyboardFrameEndUserInfoKey] as? CGRect else { return }
        guard let duration = notification.userInfo?[UIResponder.keyboardAnimationDurationUserInfoKey] as? Double else { return }

        inputContainerBottomConstraint.constant = -keyboardFrame.height

        UIView.animate(withDuration: duration) {
            self.view.layoutIfNeeded()
        }

        scrollToBottom(animated: true)
    }

    @objc private func keyboardWillHide(_ notification: Notification) {
        guard let duration = notification.userInfo?[UIResponder.keyboardAnimationDurationUserInfoKey] as? Double else { return }

        inputContainerBottomConstraint.constant = 0

        UIView.animate(withDuration: duration) {
            self.view.layoutIfNeeded()
        }
    }
}

// MARK: - UITableViewDelegate & UITableViewDataSource

extension ChatViewController: UITableViewDelegate, UITableViewDataSource {

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return messages.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let message = messages[indexPath.row]

        if message.senderId == "currentUserId" {  // TODO: Compare with actual current user
            let cell = tableView.dequeueReusableCell(withIdentifier: "MessageSentCell", for: indexPath) as! MessageSentCell
            cell.configure(with: message)
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: "MessageReceivedCell", for: indexPath) as! MessageReceivedCell
            cell.configure(with: message)
            return cell
        }
    }
}

// MARK: - UITextViewDelegate

extension ChatViewController: UITextViewDelegate {

    func textViewDidChange(_ textView: UITextView) {
        updateSendButton()
        updatePlaceholder()
        updateTextViewHeight()

        if !textView.text.isEmpty {
            startTypingIndicator()
        } else {
            stopTypingIndicator()
        }
    }
}

// MARK: - Message Models

struct Message {
    let id: String
    let threadId: String
    let senderId: String
    var receiverId: String?
    let content: String
    var messageType: MessageType = .text
    var originalLanguage: String = "en"
    var translations: [String: String] = [:]
    let timestamp: Date
    var status: MessageStatus

    enum MessageType: String {
        case text, image, video, audio, file
    }

    enum MessageStatus: String {
        case sending, sent, delivered, read, failed
    }
}

struct Thread {
    let id: String
    let name: String
    var avatarURL: URL?
    var lastMessagePreview: String
    var lastMessageTime: Date
    var unreadCount: Int
    var isOnline: Bool
    var isMuted: Bool

    init(id: String, name: String, lastMessagePreview: String = "", lastMessageTime: Date = Date(), unreadCount: Int = 0, isOnline: Bool = false, isMuted: Bool = false) {
        self.id = id
        self.name = name
        self.lastMessagePreview = lastMessagePreview
        self.lastMessageTime = lastMessageTime
        self.unreadCount = unreadCount
        self.isOnline = isOnline
        self.isMuted = isMuted
    }
}
