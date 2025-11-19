import UIKit
import Combine

/**
 * ProfileViewController - User profile view and editing
 * Features: Avatar, display name, bio, languages, logout
 */
class ProfileViewController: UIViewController {

    // MARK: - UI Components

    private let scrollView: UIScrollView = {
        let scrollView = UIScrollView()
        scrollView.translatesAutoresizingMaskIntoConstraints = false
        return scrollView
    }()

    private let contentView: UIView = {
        let view = UIView()
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }()

    private let avatarImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.contentMode = .scaleAspectFill
        imageView.layer.cornerRadius = 60
        imageView.clipsToBounds = true
        imageView.backgroundColor = Color.neutral200
        imageView.translatesAutoresizingMaskIntoConstraints = false
        imageView.widthAnchor.constraint(equalToConstant: 120).isActive = true
        imageView.heightAnchor.constraint(equalToConstant: 120).isActive = true
        return imageView
    }()

    private let changeAvatarButton: UIButton = {
        let button = UIButton(type: .system)
        button.setTitle("Change Avatar", for: .normal)
        button.titleLabel?.font = Typography.bodyMedium
        button.setTitleColor(Color.primary600, for: .normal)
        button.translatesAutoresizingMaskIntoConstraints = false
        return button
    }()

    private let displayNameTextField: UITextField = {
        let textField = UITextField()
        textField.placeholder = "Display Name"
        textField.font = Typography.bodyLarge
        textField.borderStyle = .roundedRect
        textField.backgroundColor = .white
        textField.translatesAutoresizingMaskIntoConstraints = false
        return textField
    }()

    private let phoneNumberLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyMedium
        label.textColor = Color.textSecondary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let bioTextView: UITextView = {
        let textView = UITextView()
        textView.font = Typography.bodyLarge
        textView.layer.cornerRadius = CornerRadius.md
        textView.layer.borderWidth = 1
        textView.layer.borderColor = Color.neutral300.cgColor
        textView.backgroundColor = .white
        textView.textContainerInset = UIEdgeInsets(top: 12, left: 12, bottom: 12, right: 12)
        textView.translatesAutoresizingMaskIntoConstraints = false
        textView.heightAnchor.constraint(equalToConstant: 100).isActive = true
        return textView
    }()

    private let bioCounterLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.labelSmall
        label.textColor = Color.textSecondary
        label.textAlignment = .right
        label.text = "200 characters remaining"
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let languagesLabel: UILabel = {
        let label = UILabel()
        label.text = "Languages"
        label.font = Typography.titleMedium
        label.textColor = Color.textPrimary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let primaryLanguageButton: UIButton = {
        let button = UIButton(type: .system)
        button.setTitle("🌐 English (Primary)", for: .normal)
        button.setTitleColor(Color.textPrimary, for: .normal)
        button.backgroundColor = Color.primary100
        button.layer.cornerRadius = CornerRadius.md
        button.contentEdgeInsets = UIEdgeInsets(top: 12, left: 16, bottom: 12, right: 16)
        button.translatesAutoresizingMaskIntoConstraints = false
        return button
    }()

    private let saveButton = PrimaryButton(title: "Save Changes", action: {})
    private let logoutButton: UIButton = {
        let button = UIButton(type: .system)
        button.setTitle("Logout", for: .normal)
        button.setTitleColor(Color.error, for: .normal)
        button.titleLabel?.font = Typography.titleMedium
        button.translatesAutoresizingMaskIntoConstraints = false
        return button
    }()

    private let loadingIndicator: UIActivityIndicatorView = {
        let indicator = UIActivityIndicatorView(style: .large)
        indicator.color = Color.primary600
        indicator.translatesAutoresizingMaskIntoConstraints = false
        indicator.hidesWhenStopped = true
        return indicator
    }()

    // MARK: - Properties

    private var cancellables = Set<AnyCancellable>()
    private let maxBioLength = 200

    // MARK: - Lifecycle

    override func viewDidLoad() {
        super.viewDidLoad()
        setupUI()
        setupActions()
        loadUserData()
    }

    // MARK: - Setup

    private func setupUI() {
        title = "Profile"
        view.backgroundColor = Color.neutral50

        view.addSubview(scrollView)
        view.addSubview(loadingIndicator)

        scrollView.addSubview(contentView)

        contentView.addSubview(avatarImageView)
        contentView.addSubview(changeAvatarButton)
        contentView.addSubview(createSection(title: "Display Name", view: displayNameTextField))
        contentView.addSubview(createSection(title: "Phone Number", view: phoneNumberLabel))
        contentView.addSubview(createSection(title: "Bio", view: bioTextView))
        contentView.addSubview(bioCounterLabel)
        contentView.addSubview(languagesLabel)
        contentView.addSubview(primaryLanguageButton)
        contentView.addSubview(saveButton)
        contentView.addSubview(logoutButton)

        setupConstraints()
    }

    private func createSection(title: String, view: UIView) -> UIView {
        let container = UIView()
        container.translatesAutoresizingMaskIntoConstraints = false

        let titleLabel = UILabel()
        titleLabel.text = title
        titleLabel.font = Typography.labelLarge
        titleLabel.textColor = Color.textSecondary
        titleLabel.translatesAutoresizingMaskIntoConstraints = false

        container.addSubview(titleLabel)
        container.addSubview(view)

        NSLayoutConstraint.activate([
            titleLabel.topAnchor.constraint(equalTo: container.topAnchor),
            titleLabel.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            titleLabel.trailingAnchor.constraint(equalTo: container.trailingAnchor),

            view.topAnchor.constraint(equalTo: titleLabel.bottomAnchor, constant: Spacing.sm),
            view.leadingAnchor.constraint(equalTo: container.leadingAnchor),
            view.trailingAnchor.constraint(equalTo: container.trailingAnchor),
            view.bottomAnchor.constraint(equalTo: container.bottomAnchor)
        ])

        return container
    }

    private func setupConstraints() {
        NSLayoutConstraint.activate([
            scrollView.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor),
            scrollView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            scrollView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            scrollView.bottomAnchor.constraint(equalTo: view.bottomAnchor),

            contentView.topAnchor.constraint(equalTo: scrollView.topAnchor),
            contentView.leadingAnchor.constraint(equalTo: scrollView.leadingAnchor, constant: Spacing.lg),
            contentView.trailingAnchor.constraint(equalTo: scrollView.trailingAnchor, constant: -Spacing.lg),
            contentView.bottomAnchor.constraint(equalTo: scrollView.bottomAnchor),
            contentView.widthAnchor.constraint(equalTo: scrollView.widthAnchor, constant: -2 * Spacing.lg),

            avatarImageView.topAnchor.constraint(equalTo: contentView.topAnchor, constant: Spacing.lg),
            avatarImageView.centerXAnchor.constraint(equalTo: contentView.centerXAnchor),

            changeAvatarButton.topAnchor.constraint(equalTo: avatarImageView.bottomAnchor, constant: Spacing.md),
            changeAvatarButton.centerXAnchor.constraint(equalTo: contentView.centerXAnchor),

            loadingIndicator.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            loadingIndicator.centerYAnchor.constraint(equalTo: view.centerYAnchor)
        ])

        // Layout sections vertically
        var previousView: UIView = changeAvatarButton
        let sections = contentView.subviews.filter { $0 != avatarImageView && $0 != changeAvatarButton && $0 != loadingIndicator }

        for section in sections {
            NSLayoutConstraint.activate([
                section.topAnchor.constraint(equalTo: previousView.bottomAnchor, constant: Spacing.lg),
                section.leadingAnchor.constraint(equalTo: contentView.leadingAnchor),
                section.trailingAnchor.constraint(equalTo: contentView.trailingAnchor)
            ])
            previousView = section
        }

        contentView.bottomAnchor.constraint(equalTo: previousView.bottomAnchor, constant: Spacing.xxl).isActive = true
    }

    private func setupActions() {
        changeAvatarButton.addTarget(self, action: #selector(handleChangeAvatar), for: .touchUpInside)
        primaryLanguageButton.addTarget(self, action: #selector(showLanguagePicker), for: .touchUpInside)

        saveButton.action = { [weak self] in
            self?.handleSave()
        }

        logoutButton.addTarget(self, action: #selector(handleLogout), for: .touchUpInside)

        bioTextView.delegate = self
    }

    // MARK: - Data Loading

    private func loadUserData() {
        // TODO: Load user from database/API
        displayNameTextField.text = "User Name"
        phoneNumberLabel.text = "+1 555 0100"
        bioTextView.text = ""
    }

    // MARK: - Actions

    @objc private func handleChangeAvatar() {
        let alert = UIAlertController(title: "Change Avatar", message: "Choose source", preferredStyle: .actionSheet)

        alert.addAction(UIAlertAction(title: "Photo Library", style: .default) { [weak self] _ in
            self?.showImagePicker()
        })

        alert.addAction(UIAlertAction(title: "Camera", style: .default) { [weak self] _ in
            self?.showCamera()
        })

        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))

        present(alert, animated: true)
    }

    @objc private func showLanguagePicker() {
        // TODO: Implement language picker
        let alert = UIAlertController(title: "Select Language", message: nil, preferredStyle: .actionSheet)

        let languages = ["English", "Spanish", "French", "German", "Chinese", "Japanese"]

        for language in languages {
            alert.addAction(UIAlertAction(title: language, style: .default) { [weak self] _ in
                self?.primaryLanguageButton.setTitle("🌐 \(language) (Primary)", for: .normal)
            })
        }

        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))
        present(alert, animated: true)
    }

    private func handleSave() {
        guard let displayName = displayNameTextField.text, !displayName.isEmpty else {
            showAlert(message: "Display name cannot be empty")
            return
        }

        loadingIndicator.startAnimating()

        // TODO: Save to database/API
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            self?.loadingIndicator.stopAnimating()
            self?.showAlert(message: "Profile updated successfully")
        }
    }

    @objc private func handleLogout() {
        let alert = UIAlertController(title: "Logout", message: "Are you sure you want to logout?", preferredStyle: .alert)

        alert.addAction(UIAlertAction(title: "Logout", style: .destructive) { [weak self] _ in
            // TODO: Clear session and navigate to login
            self?.dismiss(animated: true)
        })

        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))

        present(alert, animated: true)
    }

    private func showImagePicker() {
        // TODO: Implement image picker
    }

    private func showCamera() {
        // TODO: Implement camera
    }

    private func showAlert(message: String) {
        let alert = UIAlertController(title: nil, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }
}

// MARK: - UITextViewDelegate

extension ProfileViewController: UITextViewDelegate {
    func textView(_ textView: UITextView, shouldChangeTextIn range: NSRange, replacementText text: String) -> Bool {
        let currentText = textView.text ?? ""
        guard let stringRange = Range(range, in: currentText) else { return false }
        let updatedText = currentText.replacingCharacters(in: stringRange, with: text)

        bioCounterLabel.text = "\(maxBioLength - updatedText.count) characters remaining"
        bioCounterLabel.textColor = updatedText.count > maxBioLength ? Color.error : Color.textSecondary

        return updatedText.count <= maxBioLength
    }
}
