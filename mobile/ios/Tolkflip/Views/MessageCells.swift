import UIKit

/**
 * Message cell views for chat interface
 * Sent and Received message bubbles with translations
 */

// MARK: - MessageSentCell

class MessageSentCell: UITableViewCell {

    private let bubbleView: UIView = {
        let view = UIView()
        view.backgroundColor = Color.primary600
        view.layer.cornerRadius = CornerRadius.lg
        view.layer.maskedCorners = [.layerMinXMinYCorner, .layerMinXMaxYCorner, .layerMaxXMinYCorner]
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }()

    private let messageLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyLarge
        label.textColor = .white
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let translationView: UIView = {
        let view = UIView()
        view.backgroundColor = UIColor.white.withAlphaComponent(0.2)
        view.layer.cornerRadius = CornerRadius.sm
        view.translatesAutoresizingMaskIntoConstraints = false
        view.isHidden = true
        return view
    }()

    private let translationLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyMedium
        label.textColor = .white
        label.alpha = 0.9
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let timeLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.labelSmall
        label.textColor = .white
        label.alpha = 0.8
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let statusLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.labelSmall
        label.textColor = .white
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupUI() {
        backgroundColor = .clear
        selectionStyle = .none

        contentView.addSubview(bubbleView)

        bubbleView.addSubview(messageLabel)
        bubbleView.addSubview(translationView)
        bubbleView.addSubview(timeLabel)
        bubbleView.addSubview(statusLabel)

        translationView.addSubview(translationLabel)

        setupConstraints()
    }

    private func setupConstraints() {
        NSLayoutConstraint.activate([
            bubbleView.topAnchor.constraint(equalTo: contentView.topAnchor, constant: Spacing.xs),
            bubbleView.trailingAnchor.constraint(equalTo: contentView.trailingAnchor, constant: -Spacing.md),
            bubbleView.bottomAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -Spacing.xs),
            bubbleView.widthAnchor.constraint(lessThanOrEqualToConstant: 280),

            messageLabel.topAnchor.constraint(equalTo: bubbleView.topAnchor, constant: Spacing.sm),
            messageLabel.leadingAnchor.constraint(equalTo: bubbleView.leadingAnchor, constant: Spacing.md),
            messageLabel.trailingAnchor.constraint(equalTo: bubbleView.trailingAnchor, constant: -Spacing.md),

            translationView.topAnchor.constraint(equalTo: messageLabel.bottomAnchor, constant: Spacing.sm),
            translationView.leadingAnchor.constraint(equalTo: bubbleView.leadingAnchor, constant: Spacing.md),
            translationView.trailingAnchor.constraint(equalTo: bubbleView.trailingAnchor, constant: -Spacing.md),

            translationLabel.topAnchor.constraint(equalTo: translationView.topAnchor, constant: Spacing.xs),
            translationLabel.leadingAnchor.constraint(equalTo: translationView.leadingAnchor, constant: Spacing.sm),
            translationLabel.trailingAnchor.constraint(equalTo: translationView.trailingAnchor, constant: -Spacing.sm),
            translationLabel.bottomAnchor.constraint(equalTo: translationView.bottomAnchor, constant: -Spacing.xs),

            timeLabel.topAnchor.constraint(equalTo: translationView.bottomAnchor, constant: Spacing.xs),
            timeLabel.leadingAnchor.constraint(equalTo: bubbleView.leadingAnchor, constant: Spacing.md),
            timeLabel.bottomAnchor.constraint(equalTo: bubbleView.bottomAnchor, constant: -Spacing.sm),

            statusLabel.centerYAnchor.constraint(equalTo: timeLabel.centerYAnchor),
            statusLabel.leadingAnchor.constraint(equalTo: timeLabel.trailingAnchor, constant: Spacing.xs),
            statusLabel.trailingAnchor.constraint(lessThanOrEqualTo: bubbleView.trailingAnchor, constant: -Spacing.md)
        ])
    }

    func configure(with message: Message) {
        messageLabel.text = message.content
        timeLabel.text = formatTime(message.timestamp)

        // Status icon
        switch message.status {
        case .sending:
            statusLabel.text = "⏱"
        case .sent:
            statusLabel.text = "✓"
        case .delivered:
            statusLabel.text = "✓✓"
        case .read:
            statusLabel.text = "✓✓"
            statusLabel.textColor = Color.primary200
        case .failed:
            statusLabel.text = "❌"
        }

        // Translation
        if let translation = message.translations["en"], !translation.isEmpty, message.originalLanguage != "en" {
            translationLabel.text = "🌐 \(translation)"
            translationView.isHidden = false
        } else {
            translationView.isHidden = true
        }
    }

    private func formatTime(_ date: Date) -> String {
        let formatter = DateFormatter()
        formatter.timeStyle = .short
        return formatter.string(from: date)
    }
}

// MARK: - MessageReceivedCell

class MessageReceivedCell: UITableViewCell {

    private let bubbleView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        view.layer.cornerRadius = CornerRadius.lg
        view.layer.maskedCorners = [.layerMaxXMinYCorner, .layerMinXMaxYCorner, .layerMaxXMaxYCorner]
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOpacity = 0.05
        view.layer.shadowRadius = 4
        view.layer.shadowOffset = CGSize(width: 0, height: 2)
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }()

    private let messageLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyLarge
        label.textColor = Color.textPrimary
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let translationView: UIView = {
        let view = UIView()
        view.backgroundColor = Color.primary50
        view.layer.cornerRadius = CornerRadius.sm
        view.translatesAutoresizingMaskIntoConstraints = false
        view.isHidden = true
        return view
    }()

    private let translationLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.bodyMedium
        label.textColor = Color.primary700
        label.numberOfLines = 0
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let timeLabel: UILabel = {
        let label = UILabel()
        label.font = Typography.labelSmall
        label.textColor = Color.textSecondary
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    override init(style: UITableViewCell.CellStyle, reuseIdentifier: String?) {
        super.init(style: style, reuseIdentifier: reuseIdentifier)
        setupUI()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    private func setupUI() {
        backgroundColor = .clear
        selectionStyle = .none

        contentView.addSubview(bubbleView)

        bubbleView.addSubview(messageLabel)
        bubbleView.addSubview(translationView)
        bubbleView.addSubview(timeLabel)

        translationView.addSubview(translationLabel)

        setupConstraints()
    }

    private func setupConstraints() {
        NSLayoutConstraint.activate([
            bubbleView.topAnchor.constraint(equalTo: contentView.topAnchor, constant: Spacing.xs),
            bubbleView.leadingAnchor.constraint(equalTo: contentView.leadingAnchor, constant: Spacing.md),
            bubbleView.bottomAnchor.constraint(equalTo: contentView.bottomAnchor, constant: -Spacing.xs),
            bubbleView.widthAnchor.constraint(lessThanOrEqualToConstant: 280),

            messageLabel.topAnchor.constraint(equalTo: bubbleView.topAnchor, constant: Spacing.sm),
            messageLabel.leadingAnchor.constraint(equalTo: bubbleView.leadingAnchor, constant: Spacing.md),
            messageLabel.trailingAnchor.constraint(equalTo: bubbleView.trailingAnchor, constant: -Spacing.md),

            translationView.topAnchor.constraint(equalTo: messageLabel.bottomAnchor, constant: Spacing.sm),
            translationView.leadingAnchor.constraint(equalTo: bubbleView.leadingAnchor, constant: Spacing.md),
            translationView.trailingAnchor.constraint(equalTo: bubbleView.trailingAnchor, constant: -Spacing.md),

            translationLabel.topAnchor.constraint(equalTo: translationView.topAnchor, constant: Spacing.xs),
            translationLabel.leadingAnchor.constraint(equalTo: translationView.leadingAnchor, constant: Spacing.sm),
            translationLabel.trailingAnchor.constraint(equalTo: translationView.trailingAnchor, constant: -Spacing.sm),
            translationLabel.bottomAnchor.constraint(equalTo: translationView.bottomAnchor, constant: -Spacing.xs),

            timeLabel.topAnchor.constraint(equalTo: translationView.bottomAnchor, constant: Spacing.xs),
            timeLabel.leadingAnchor.constraint(equalTo: bubbleView.leadingAnchor, constant: Spacing.md),
            timeLabel.trailingAnchor.constraint(lessThanOrEqualTo: bubbleView.trailingAnchor, constant: -Spacing.md),
            timeLabel.bottomAnchor.constraint(equalTo: bubbleView.bottomAnchor, constant: -Spacing.sm)
        ])
    }

    func configure(with message: Message) {
        messageLabel.text = message.content
        timeLabel.text = formatTime(message.timestamp)

        // Translation
        if let translation = message.translations["en"], !translation.isEmpty, message.originalLanguage != "en" {
            translationLabel.text = "🌐 \(translation)"
            translationView.isHidden = false
        } else {
            translationView.isHidden = true
        }
    }

    private func formatTime(_ date: Date) -> String {
        let formatter = DateFormatter()
        formatter.timeStyle = .short
        return formatter.string(from: date)
    }
}
