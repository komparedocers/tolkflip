import UIKit
import Combine

/**
 * ChatListViewController - Conversation list
 * Features: Thread list, search, unread counts, swipe actions
 */
class ChatListViewController: UIViewController {

    // MARK: - UI Components

    private let searchBar: UISearchBar = {
        let searchBar = UISearchBar()
        searchBar.placeholder = "Search conversations"
        searchBar.searchBarStyle = .minimal
        searchBar.translatesAutoresizingMaskIntoConstraints = false
        return searchBar
    }()

    private let tableView: UITableView = {
        let table = UITableView(frame: .zero, style: .plain)
        table.backgroundColor = Color.neutral50
        table.separatorStyle = .singleLine
        table.separatorColor = Color.neutral200
        table.translatesAutoresizingMaskIntoConstraints = false
        return table
    }()

    private let emptyStateLabel: UILabel = {
        let label = UILabel()
        label.text = "No conversations yet\nStart chatting to see your messages here"
        label.font = Typography.bodyLarge
        label.textColor = Color.textSecondary
        label.textAlignment = .center
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        label.isHidden = true
        return label
    }()

    private let newChatButton: UIButton = {
        let button = UIButton(type: .system)
        button.backgroundColor = Color.primary600
        button.setImage(UIImage(systemName: "plus"), for: .normal)
        button.tintColor = .white
        button.layer.cornerRadius = 28
        button.layer.shadowColor = UIColor.black.cgColor
        button.layer.shadowOpacity = 0.3
        button.layer.shadowRadius = Elevation.lg
        button.layer.shadowOffset = CGSize(width: 0, height: 4)
        button.translatesAutoresizingMaskIntoConstraints = false
        button.widthAnchor.constraint(equalToConstant: 56).isActive = true
        button.heightAnchor.constraint(equalToConstant: 56).isActive = true
        return button
    }()

    private let refreshControl = UIRefreshControl()

    // MARK: - Properties

    private var cancellables = Set<AnyCancellable>()
    private var threads: [Thread] = []
    private var filteredThreads: [Thread] = []
    private var isSearching = false

    // MARK: - Lifecycle

    override func viewDidLoad() {
        super.viewDidLoad()
        setupUI()
        setupTableView()
        setupActions()
        loadThreads()
    }

    override func viewWillAppear(_ animated: Bool) {
        super.viewWillAppear(animated)
        refreshThreads()
    }

    // MARK: - Setup

    private func setupUI() {
        title = "Tolkflip"
        view.backgroundColor = Color.neutral50

        navigationItem.leftBarButtonItem = UIBarButtonItem(
            image: UIImage(systemName: "gearshape"),
            style: .plain,
            target: self,
            action: #selector(showSettings)
        )

        navigationItem.rightBarButtonItem = UIBarButtonItem(
            image: UIImage(systemName: "person.circle"),
            style: .plain,
            target: self,
            action: #selector(showProfile)
        )

        view.addSubview(searchBar)
        view.addSubview(tableView)
        view.addSubview(emptyStateLabel)
        view.addSubview(newChatButton)

        tableView.refreshControl = refreshControl

        setupConstraints()
    }

    private func setupConstraints() {
        NSLayoutConstraint.activate([
            searchBar.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor),
            searchBar.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            searchBar.trailingAnchor.constraint(equalTo: view.trailingAnchor),

            tableView.topAnchor.constraint(equalTo: searchBar.bottomAnchor),
            tableView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            tableView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor),

            emptyStateLabel.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            emptyStateLabel.centerYAnchor.constraint(equalTo: view.centerYAnchor),

            newChatButton.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -Spacing.lg),
            newChatButton.bottomAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor, constant: -Spacing.lg)
        ])
    }

    private func setupTableView() {
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(ThreadCell.self, forCellReuseIdentifier: "ThreadCell")
        tableView.rowHeight = 80
    }

    private func setupActions() {
        searchBar.delegate = self

        newChatButton.addTarget(self, action: #selector(showNewChat), for: .touchUpInside)

        refreshControl.addTarget(self, action: #selector(refreshThreads), for: .valueChanged)
    }

    // MARK: - Data Loading

    private func loadThreads() {
        // TODO: Load from database/API
        // For demo, create sample data
        threads = []

        updateUI()
    }

    @objc private func refreshThreads() {
        // TODO: Fetch latest threads from API
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            self?.refreshControl.endRefreshing()
            self?.loadThreads()
        }
    }

    private func updateUI() {
        let displayThreads = isSearching ? filteredThreads : threads

        if displayThreads.isEmpty {
            emptyStateLabel.isHidden = false
            tableView.isHidden = true
        } else {
            emptyStateLabel.isHidden = true
            tableView.isHidden = false
            tableView.reloadData()
        }
    }

    // MARK: - Actions

    @objc private func showSettings() {
        let settingsVC = SettingsViewController()
        let navController = UINavigationController(rootViewController: settingsVC)
        present(navController, animated: true)
    }

    @objc private func showProfile() {
        let profileVC = ProfileViewController()
        navigationController?.pushViewController(profileVC, animated: true)
    }

    @objc private func showNewChat() {
        // TODO: Implement contact picker
        let alert = UIAlertController(title: "New Chat", message: "Contact picker coming soon", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }

    private func openChat(with thread: Thread) {
        let chatVC = ChatViewController(thread: thread)
        navigationController?.pushViewController(chatVC, animated: true)
    }

    private func archiveThread(_ thread: Thread) {
        // TODO: Archive thread in database/API
        if let index = threads.firstIndex(where: { $0.id == thread.id }) {
            threads.remove(at: index)
            updateUI()
        }
    }

    private func deleteThread(_ thread: Thread) {
        // TODO: Delete thread from database/API
        if let index = threads.firstIndex(where: { $0.id == thread.id }) {
            threads.remove(at: index)
            updateUI()
        }
    }

    private func muteThread(_ thread: Thread) {
        // TODO: Mute thread in database/API
    }
}

// MARK: - UITableViewDelegate & UITableViewDataSource

extension ChatListViewController: UITableViewDelegate, UITableViewDataSource {

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return isSearching ? filteredThreads.count : threads.count
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCell(withIdentifier: "ThreadCell", for: indexPath) as! ThreadCell
        let thread = isSearching ? filteredThreads[indexPath.row] : threads[indexPath.row]
        cell.configure(with: thread)
        return cell
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: true)
        let thread = isSearching ? filteredThreads[indexPath.row] : threads[indexPath.row]
        openChat(with: thread)
    }

    func tableView(_ tableView: UITableView, trailingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let thread = threads[indexPath.row]

        let deleteAction = UIContextualAction(style: .destructive, title: "Delete") { [weak self] (_, _, completion) in
            self?.deleteThread(thread)
            completion(true)
        }
        deleteAction.backgroundColor = Color.error

        let archiveAction = UIContextualAction(style: .normal, title: "Archive") { [weak self] (_, _, completion) in
            self?.archiveThread(thread)
            completion(true)
        }
        archiveAction.backgroundColor = Color.warning

        let configuration = UISwipeActionsConfiguration(actions: [deleteAction, archiveAction])
        return configuration
    }

    func tableView(_ tableView: UITableView, leadingSwipeActionsConfigurationForRowAt indexPath: IndexPath) -> UISwipeActionsConfiguration? {
        let thread = threads[indexPath.row]

        let muteAction = UIContextualAction(style: .normal, title: thread.isMuted ? "Unmute" : "Mute") { [weak self] (_, _, completion) in
            self?.muteThread(thread)
            completion(true)
        }
        muteAction.backgroundColor = Color.primary600

        let configuration = UISwipeActionsConfiguration(actions: [muteAction])
        return configuration
    }
}

// MARK: - UISearchBarDelegate

extension ChatListViewController: UISearchBarDelegate {

    func searchBar(_ searchBar: UISearchBar, textDidChange searchText: String) {
        if searchText.isEmpty {
            isSearching = false
            filteredThreads = []
        } else {
            isSearching = true
            filteredThreads = threads.filter { thread in
                thread.name.lowercased().contains(searchText.lowercased()) ||
                thread.lastMessagePreview.lowercased().contains(searchText.lowercased())
            }
        }

        updateUI()
    }

    func searchBarCancelButtonClicked(_ searchBar: UISearchBar) {
        searchBar.text = ""
        searchBar.resignFirstResponder()
        isSearching = false
        filteredThreads = []
        updateUI()
    }
}

// MARK: - ThreadCell

class ThreadCell: UITableViewCell {

    private let avatarImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFill
        imageView.layer.cornerRadius = 28
        imageView.clipsToBounds = true
        imageView.backgroundColor = Color.neutral200
        imageView.translatesAutoresizingMaskIntoConstraints = false
        imageView.widthAnchor.constraint(equalToConstant: 56).isActive = true
        imageView.heightAnchor.constraint(equalToConstant: 56).isActive = true
        return imageView
    }()

    private let nameLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.titleMedium
        label.textColor = Color.textPrimary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let previewLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyMedium
        label.textColor = Color.textSecondary
        label.numberOfLines = 2
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let timeLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodySmall
        label.textColor = Color.textSecondary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let unreadBadge: UILabel = {
        let label = UILabel()
        label.font = Typography.labelSmall
        label.textColor = .white
        label.textAlignment = .center
        label.backgroundColor = Color.primary600
        label.layer.cornerRadius = 12
        label.clipsToBounds = true
        label.translatesAutoresizingMaskIntoConstraints = false
        label.widthAnchor.constraint(greaterThanOrEqualToConstant: 24).isActive = true
        label.heightAnchor.constraint(equalToConstant: 24).isActive = true
        label.isHidden = true
        return label
    }()

    private let onlineIndicator: UIView = {
        let view = UIView()
        view.backgroundColor = Color.success
        view.layer.cornerRadius = 6
        view.layer.borderWidth = 2
        view.layer.borderColor = UIColor.white.cgColor
        view.translatesAutoresizingMaskIntoConstraints = false
        view.widthAnchor.constraint(equalToConstant: 12).isActive = true
        view.heightAnchor.constraint(equalToConstant: 12).isActive = true
        view.isHidden = true
        return view
    }()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupUI() {
        contentView.addSubview(avatarImageView)
        contentView.addSubview(nameLabel)
        contentView.addSubview(previewLabel)
        contentView.addSubview(timeLabel)
        contentView.addSubview(unreadBadge)
        contentView.addSubview(onlineIndicator)

        NSLayoutConstraint.activate([
            avatarImageView.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: Spacing.md),
            avatarImageView.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),

            nameLabel.topAnchor.constraint(equalTo: contentView.topAnchor, constant: Spacing.md),
            nameLabel.leadingAnchor.constraint(equalTo: avatarImageView.trailingAnchor, constant: Spacing.md),
            nameLabel.trailingAnchor.constraint(equalTo: timeLabel.leadingAnchor, constant: -Spacing.sm),

            previewLabel.topAnchor.constraint(equalTo: nameLabel.bottomAnchor, constant: Spacing.xs),
            previewLabel.leadingAnchor.constraint(equalTo: avatarImageView.trailingAnchor, constant: Spacing.md),
            previewLabel.trailingAnchor.constraint(equalTo: unreadBadge.leadingAnchor, constant: -Spacing.sm),
            previewLabel.bottomAnchor.constraint(lessThanOrEqualTo: contentView.bottomAnchor, constant: -Spacing.md),

            timeLabel.topAnchor.constraint(equalTo: contentView.topAnchor, constant: Spacing.md),
            timeLabel.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -Spacing.md),

            unreadBadge.centerYAnchor.constraint(equalTo: previewLabel.centerYAnchor),
            unreadBadge.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -Spacing.md),

            onlineIndicator.trailingAnchor.constraint(equalTo: avatarImageView.trailingAnchor),
            onlineIndicator.bottomAnchor.constraint(equalTo: avatarImageView.bottomAnchor)
        ])
    }

    func configure(with thread: Thread) {
        nameLabel.text = thread.name
        previewLabel.text = thread.lastMessagePreview
        timeLabel.text = formatTime(thread.lastMessageTime)

        if thread.unreadCount > 0 {
            unreadBadge.isHidden = false
            unreadBadge.text = thread.unreadCount > 99 ? "99+" : "\(thread.unreadCount)"
            nameLabel.font = Typography.titleMediumBold
            previewLabel.font = Typography.bodyMediumBold
        } else {
            unreadBadge.isHidden = true
            nameLabel.font = Typography.titleMedium
            previewLabel.font = Typography.bodyMedium
        }

        onlineIndicator.isHidden = !thread.isOnline

        // TODO: Load avatar image
        // if let avatarURL = thread.avatarURL {
        //     avatarImageView.load(url: avatarURL)
        // }
    }

    private func formatTime(_ timestamp: Date) -> String {
        let now = Date()
        let calendar = Calendar.current

        if calendar.isDateInToday(timestamp) {
            let formatter = DateFormatter()
            formatter.timeStyle = .short
            return formatter.string(from: timestamp)
        } else if calendar.isDateInYesterday(timestamp) {
            return "Yesterday"
        } else if timestamp.timeIntervalSinceNow > -604800 {
            let formatter = DateFormatter()
            formatter.dateFormat = "EEE"
            return formatter.string(from: timestamp)
        } else {
            let formatter = DateFormatter()
            formatter.dateFormat = "MMM dd"
            return formatter.string(from: timestamp)
        }
    }
}
