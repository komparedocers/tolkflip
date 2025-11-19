import UIKit

/**
 * SettingsViewController - App settings
 * Features: Notifications, privacy, language, about
 */
class SettingsViewController: UIViewController {

    // MARK: - UI Components

    private let tableView: UITableView = {
        let table = UITableView(frame: .zero, style: .insetGrouped)
        table.translatesAutoresizingMaskIntoConstraints = false
        return table
    }()

    // MARK: - Properties

    private var sections: [SettingsSection] = []

    // MARK: - Lifecycle

    override func viewDidLoad() {
        super.viewDidLoad()
        setupUI()
        setupTableView()
        loadSettings()
    }

    // MARK: - Setup

    private func setupUI() {
        title = "Settings"
        view.backgroundColor = Color.neutral50

        navigationItem.leftBarButtonItem = UIBarButtonItem(
            barButtonSystemItem: .close,
            target: self,
            action: #selector(dismissView)
        )

        view.addSubview(tableView)

        NSLayoutConstraint.activate([
            tableView.topAnchor.constraint(equalTo: view.topAnchor),
            tableView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            tableView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            tableView.bottomAnchor.constraint(equalTo: view.bottomAnchor)
        ])
    }

    private func setupTableView() {
        tableView.delegate = self
        tableView.dataSource = self
        tableView.register(SettingCell.self, forCellReuseIdentifier: "SettingCell")
        tableView.register(SettingSwitchCell.self, forCellReuseIdentifier: "SettingSwitchCell")
    }

    private func loadSettings() {
        sections = [
            SettingsSection(title: "Notifications", items: [
                SettingItem(title: "Enable Notifications", type: .toggle, isEnabled: true),
                SettingItem(title: "Message Notifications", type: .toggle, isEnabled: true),
                SettingItem(title: "Call Notifications", type: .toggle, isEnabled: true),
                SettingItem(title: "Group Notifications", type: .toggle, isEnabled: false)
            ]),
            SettingsSection(title: "Privacy", items: [
                SettingItem(title: "Last Seen", type: .disclosure),
                SettingItem(title: "Profile Photo", type: .disclosure),
                SettingItem(title: "About", type: .disclosure),
                SettingItem(title: "Blocked Contacts", type: .disclosure)
            ]),
            SettingsSection(title: "Chat Settings", items: [
                SettingItem(title: "Auto-Translate Messages", type: .toggle, isEnabled: true),
                SettingItem(title: "Save Media to Gallery", type: .toggle, isEnabled: false),
                SettingItem(title: "Font Size", type: .disclosure)
            ]),
            SettingsSection(title: "About", items: [
                SettingItem(title: "Version", type: .value, value: "1.0.0"),
                SettingItem(title: "Privacy Policy", type: .disclosure),
                SettingItem(title: "Terms of Service", type: .disclosure),
                SettingItem(title: "Help Center", type: .disclosure)
            ])
        ]

        tableView.reloadData()
    }

    // MARK: - Actions

    @objc private func dismissView() {
        dismiss(animated: true)
    }

    private func handleSettingTap(_ item: SettingItem) {
        switch item.title {
        case "Last Seen":
            showLastSeenSettings()
        case "Privacy Policy":
            showPrivacyPolicy()
        case "Terms of Service":
            showTermsOfService()
        case "Help Center":
            showHelpCenter()
        default:
            break
        }
    }

    private func handleToggle(_ item: SettingItem, isOn: Bool) {
        // TODO: Save setting to UserDefaults/Database
        print("\(item.title) toggled: \(isOn)")
    }

    private func showLastSeenSettings() {
        let alert = UIAlertController(title: "Last Seen", message: "Choose who can see your last seen", preferredStyle: .actionSheet)

        alert.addAction(UIAlertAction(title: "Everyone", style: .default))
        alert.addAction(UIAlertAction(title: "My Contacts", style: .default))
        alert.addAction(UIAlertAction(title: "Nobody", style: .default))
        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))

        present(alert, animated: true)
    }

    private func showPrivacyPolicy() {
        // TODO: Show privacy policy web view
        let alert = UIAlertController(title: "Privacy Policy", message: "Privacy policy coming soon", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }

    private func showTermsOfService() {
        // TODO: Show terms of service web view
        let alert = UIAlertController(title: "Terms of Service", message: "Terms of service coming soon", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }

    private func showHelpCenter() {
        // TODO: Show help center
        let alert = UIAlertController(title: "Help Center", message: "Help center coming soon", preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }
}

// MARK: - UITableViewDelegate & UITableViewDataSource

extension SettingsViewController: UITableViewDelegate, UITableViewDataSource {

    func numberOfSections(in tableView: UITableView) -> Int {
        return sections.count
    }

    func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return sections[section].items.count
    }

    func tableView(_ tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        return sections[section].title
    }

    func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
        let item = sections[indexPath.section].items[indexPath.row]

        if item.type == .toggle {
            let cell = tableView.dequeueReusableCell(withIdentifier: "SettingSwitchCell", for: indexPath) as! SettingSwitchCell
            cell.configure(with: item) { [weak self] isOn in
                self?.handleToggle(item, isOn: isOn)
            }
            return cell
        } else {
            let cell = tableView.dequeueReusableCell(withIdentifier: "SettingCell", for: indexPath) as! SettingCell
            cell.configure(with: item)
            return cell
        }
    }

    func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
        tableView.deselectRow(at: indexPath, animated: true)
        let item = sections[indexPath.section].items[indexPath.row]

        if item.type != .toggle {
            handleSettingTap(item)
        }
    }
}

// MARK: - Models

struct SettingsSection {
    let title: String
    let items: [SettingItem]
}

struct SettingItem {
    let title: String
    let type: SettingType
    var value: String?
    var isEnabled: Bool?

    enum SettingType {
        case disclosure, value, toggle
    }
}

// MARK: - Custom Cells

class SettingCell: UITableViewCell {

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: .value1, reuseIdentifier: reuseIdentifier)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    func configure(with item: SettingItem) {
        textLabel?.text = item.title
        textLabel?.font = Typography.bodyLarge

        switch item.type {
        case .disclosure:
            accessoryType = .disclosureIndicator
            detailTextLabel?.text = nil
        case .value:
            accessoryType = .none
            detailTextLabel?.text = item.value
            detailTextLabel?.font = Typography.bodyMedium
            detailTextLabel?.textColor = Color.textSecondary
        default:
            break
        }
    }
}

class SettingSwitchCell: UITableViewCell {

    private let titleLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyLarge
        label.textColor = Color.textPrimary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let switchControl: UISwitch = {
        let switchControl = UISwitch()
        switchControl.onTintColor = Color.primary600
        switchControl.translatesAutoresizingMaskIntoConstraints = false
        return switchControl
    }()

    private var onToggle: ((Bool) -> Void)?

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupUI() {
        contentView.addSubview(titleLabel)
        contentView.addSubview(switchControl)

        switchControl.addTarget(self, action: #selector(handleToggle), for: .valueChanged)

        NSLayoutConstraint.activate([
            titleLabel.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: Spacing.md),
            titleLabel.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),

            switchControl.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -Spacing.md),
            switchControl.centerYAnchor.constraint(equalTo: contentView.centerYAnchor),
            switchControl.leadingAnchor.constraint(greaterThanOrEqualTo: titleLabel.trailingAnchor, constant: Spacing.md)
        ])
    }

    func configure(with item: SettingItem, onToggle: @escaping (Bool) -> Void) {
        titleLabel.text = item.title
        switchControl.isOn = item.isEnabled ?? false
        self.onToggle = onToggle
    }

    @objc private func handleToggle() {
        onToggle?(switchControl.isOn)
    }
}
