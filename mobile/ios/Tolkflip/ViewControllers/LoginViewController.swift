import UIKit
import Combine

/**
 * LoginViewController - Phone-based authentication
 * Features: Phone input, OTP verification, registration
 */
class LoginViewController: UIViewController {

    // MARK: - UI Components

    private let logoImageView: UIImageView = {
        let imageView = UIImageView()
        imageView.image = UIImage(named: "app_logo")
        imageView.contentMode = .scaleAspectFit
        imageView.translatesAutoresizingMaskIntoConstraints = false
        return imageView
    }()

    private let titleLabel: UILabel = {
        let label = UILabel()
        label.text = "Tolkflip"
        label.font = Typography.displayLarge
        label.textColor = Color.primary600
        label.textAlignment = .center
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let subtitleLabel: UILabel = {
        let label = UILabel()
        label.text = "Connect with anyone in any language"
        label.font = Typography.bodyLarge
        label.textColor = Color.textSecondary
        label.textAlignment = .center
        label.numberOfLines = 2
        label.translatesAutoresizingMaskIntoConstraints = false
        return label
    }()

    private let phoneInputView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        view.layer.cornerRadius = CornerRadius.md
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOpacity = 0.1
        view.layer.shadowRadius = Elevation.lg
        view.layer.shadowOffset = CGSize(width: 0, height: 4)
        view.translatesAutoresizingMaskIntoConstraints = false
        return view
    }()

    private let countryCodeButton: UIButton = {
        let button = UIButton(type: .system)
        button.setTitle("🇺🇸 +1", for: .normal)
        button.setTitleColor(Color.textPrimary, for: .normal)
        button.titleLabel?.font = Typography.bodyLarge
        button.contentHorizontalAlignment = .left
        button.translatesAutoresizingMaskIntoConstraints = false
        return button
    }()

    private let phoneTextField: UITextField = {
        let textField = UITextField()
        textField.placeholder = "Phone number"
        textField.font = Typography.bodyLarge
        textField.keyboardType = .phonePad
        textField.textColor = Color.textPrimary
        textField.translatesAutoresizingMaskIntoConstraints = false
        return textField
    }()

    private let continueButton = PrimaryButton(title: "Continue", action: {})

    private let otpStackView: UIStackView = {
        let stack = UIStackView()
        stack.axis = .horizontal
        stack.spacing = Spacing.md
        stack.distribution = .fillEqually
        stack.translatesAutoresizingMaskIntoConstraints = false
        stack.isHidden = true
        return stack
    }()

    private let verifyButton = PrimaryButton(title: "Verify", action: {})
    private let resendButton: UIButton = {
        let button = UIButton(type: .system)
        button.setTitle("Resend code", for: .normal)
        button.setTitleColor(Color.primary600, for: .normal)
        button.titleLabel?.font = Typography.bodyMedium
        button.translatesAutoresizingMaskIntoConstraints = false
        button.isHidden = true
        return button
    }()

    private let registrationView: UIView = {
        let view = UIView()
        view.backgroundColor = .white
        view.layer.cornerRadius = CornerRadius.md
        view.layer.shadowColor = UIColor.black.cgColor
        view.layer.shadowOpacity = 0.1
        view.layer.shadowRadius = Elevation.lg
        view.layer.shadowOffset = CGSize(width: 0, height: 4)
        view.translatesAutoresizingMaskIntoConstraints = false
        view.isHidden = true
        return view
    }()

    private let displayNameTextField: UITextField = {
        let textField = UITextField()
        textField.placeholder = "Display Name"
        textField.font = Typography.bodyLarge
        textField.textColor = Color.textPrimary
        textField.borderStyle = .roundedRect
        textField.translatesAutoresizingMaskIntoConstraints = false
        return textField
    }()

    private let registerButton = PrimaryButton(title: "Register", action: {})

    private let loadingIndicator: UIActivityIndicatorView = {
        let indicator = UIActivityIndicatorView(style: .large)
        indicator.color = Color.primary600
        indicator.translatesAutoresizingMaskIntoConstraints = false
        indicator.hidesWhenStopped = true
        return indicator
    }()

    // MARK: - Properties

    private var cancellables = Set<AnyCancellable>()
    private var otpTextFields: [UITextField] = []
    private var currentStep: Step = .phone
    private var countryCode = "+1"
    private var phoneNumber = ""
    private var verificationCode = ""

    private enum Step {
        case phone, verify, register
    }

    // MARK: - Lifecycle

    override func viewDidLoad() {
        super.viewDidLoad()
        setupUI()
        setupActions()
    }

    // MARK: - Setup

    private func setupUI() {
        view.backgroundColor = Color.neutral50

        view.addSubview(logoImageView)
        view.addSubview(titleLabel)
        view.addSubview(subtitleLabel)
        view.addSubview(phoneInputView)
        view.addSubview(continueButton)
        view.addSubview(otpStackView)
        view.addSubview(verifyButton)
        view.addSubview(resendButton)
        view.addSubview(registrationView)
        view.addSubview(loadingIndicator)

        phoneInputView.addSubview(countryCodeButton)
        phoneInputView.addSubview(phoneTextField)

        registrationView.addSubview(displayNameTextField)
        registrationView.addSubview(registerButton)

        setupOTPFields()
        setupConstraints()
    }

    private func setupOTPFields() {
        for _ in 0..<6 {
            let textField = createOTPTextField()
            otpTextFields.append(textField)
            otpStackView.addArrangedSubview(textField)
        }
    }

    private func createOTPTextField() -> UITextField {
        let textField = UITextField()
        textField.font = Typography.titleLarge
        textField.textAlignment = .center
        textField.keyboardType = .numberPad
        textField.textColor = Color.textPrimary
        textField.backgroundColor = .white
        textField.layer.cornerRadius = CornerRadius.md
        textField.layer.borderWidth = 2
        textField.layer.borderColor = Color.neutral300.cgColor
        textField.delegate = self
        textField.translatesAutoresizingMaskIntoConstraints = false
        textField.widthAnchor.constraint(equalToConstant: 50).isActive = true
        textField.heightAnchor.constraint(equalToConstant: 56).isActive = true
        return textField
    }

    private func setupConstraints() {
        NSLayoutConstraint.activate([
            logoImageView.topAnchor.constraint(equalTo: view.safeAreaLayoutGuide.topAnchor, constant: Spacing.xxl),
            logoImageView.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            logoImageView.widthAnchor.constraint(equalToConstant: 80),
            logoImageView.heightAnchor.constraint(equalToConstant: 80),

            titleLabel.topAnchor.constraint(equalTo: logoImageView.bottomAnchor, constant: Spacing.lg),
            titleLabel.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: Spacing.lg),
            titleLabel.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -Spacing.lg),

            subtitleLabel.topAnchor.constraint(equalTo: titleLabel.bottomAnchor, constant: Spacing.sm),
            subtitleLabel.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: Spacing.lg),
            subtitleLabel.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -Spacing.lg),

            phoneInputView.topAnchor.constraint(equalTo: subtitleLabel.bottomAnchor, constant: Spacing.xxl),
            phoneInputView.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: Spacing.lg),
            phoneInputView.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -Spacing.lg),
            phoneInputView.heightAnchor.constraint(equalToConstant: 60),

            countryCodeButton.leadingAnchor.constraint(equalTo: phoneInputView.leadingAnchor, constant: Spacing.md),
            countryCodeButton.centerYAnchor.constraint(equalTo: phoneInputView.centerYAnchor),
            countryCodeButton.widthAnchor.constraint(equalToConstant: 80),

            phoneTextField.leadingAnchor.constraint(equalTo: countryCodeButton.trailingAnchor, constant: Spacing.md),
            phoneTextField.trailingAnchor.constraint(equalTo: phoneInputView.trailingAnchor, constant: -Spacing.md),
            phoneTextField.centerYAnchor.constraint(equalTo: phoneInputView.centerYAnchor),

            continueButton.topAnchor.constraint(equalTo: phoneInputView.bottomAnchor, constant: Spacing.lg),
            continueButton.leadingAnchor.constraint(equalTo: view.leadingAnchor, constant: Spacing.lg),
            continueButton.trailingAnchor.constraint(equalTo: view.trailingAnchor, constant: -Spacing.lg),

            loadingIndicator.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            loadingIndicator.centerYAnchor.constraint(equalTo: view.centerYAnchor)
        ])
    }

    private func setupActions() {
        continueButton.action = { [weak self] in
            self?.handleContinue()
        }

        verifyButton.action = { [weak self] in
            self?.handleVerify()
        }

        registerButton.action = { [weak self] in
            self?.handleRegister()
        }

        countryCodeButton.addTarget(self, action: #selector(showCountryPicker), for: .touchUpInside)
        resendButton.addTarget(self, action: #selector(handleResend), for: .touchUpInside)
    }

    // MARK: - Actions

    private func handleContinue() {
        guard let phone = phoneTextField.text, phone.count >= 10 else {
            showAlert(message: "Please enter a valid phone number")
            return
        }

        phoneNumber = phone
        loadingIndicator.startAnimating()

        // TODO: Call API to request verification code
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            self?.loadingIndicator.stopAnimating()
            self?.showVerificationStep()
        }
    }

    private func handleVerify() {
        verificationCode = otpTextFields.map { $0.text ?? "" }.joined()

        guard verificationCode.count == 6 else {
            showAlert(message: "Please enter the complete 6-digit code")
            return
        }

        loadingIndicator.startAnimating()

        // TODO: Call API to verify code and check if user exists
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            self?.loadingIndicator.stopAnimating()

            // For demo, assume user doesn't exist
            let userExists = false

            if userExists {
                self?.navigateToChatList()
            } else {
                self?.showRegistrationStep()
            }
        }
    }

    private func handleRegister() {
        guard let displayName = displayNameTextField.text, !displayName.isEmpty else {
            showAlert(message: "Please enter your display name")
            return
        }

        loadingIndicator.startAnimating()

        // TODO: Call API to register user
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            self?.loadingIndicator.stopAnimating()
            self?.navigateToChatList()
        }
    }

    @objc private func handleResend() {
        handleContinue()
    }

    @objc private func showCountryPicker() {
        // TODO: Implement country picker
        let alert = UIAlertController(title: "Select Country", message: nil, preferredStyle: .actionSheet)

        let countries = [
            ("🇺🇸 United States", "+1"),
            ("🇬🇧 United Kingdom", "+44"),
            ("🇫🇷 France", "+33"),
            ("🇩🇪 Germany", "+49"),
            ("🇪🇸 Spain", "+34")
        ]

        for (name, code) in countries {
            alert.addAction(UIAlertAction(title: "\(name) (\(code))", style: .default) { [weak self] _ in
                self?.countryCode = code
                self?.countryCodeButton.setTitle("\(name.prefix(2)) \(code)", for: .normal)
            })
        }

        alert.addAction(UIAlertAction(title: "Cancel", style: .cancel))
        present(alert, animated: true)
    }

    // MARK: - Navigation

    private func showVerificationStep() {
        currentStep = .verify

        UIView.animate(withDuration: 0.3) {
            self.phoneInputView.isHidden = true
            self.continueButton.isHidden = true
            self.otpStackView.isHidden = false
            self.verifyButton.isHidden = false
            self.resendButton.isHidden = false
        }

        otpTextFields.first?.becomeFirstResponder()
    }

    private func showRegistrationStep() {
        currentStep = .register

        UIView.animate(withDuration: 0.3) {
            self.otpStackView.isHidden = true
            self.verifyButton.isHidden = true
            self.resendButton.isHidden = true
            self.registrationView.isHidden = false
        }
    }

    private func navigateToChatList() {
        // TODO: Navigate to ChatListViewController
        let chatListVC = ChatListViewController()
        let navController = UINavigationController(rootViewController: chatListVC)
        navController.modalPresentationStyle = .fullScreen
        present(navController, animated: true)
    }

    private func showAlert(message: String) {
        let alert = UIAlertController(title: nil, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }
}

// MARK: - UITextFieldDelegate

extension LoginViewController: UITextFieldDelegate {
    func textField(_ textField: UITextField, shouldChangeCharactersIn range: NSRange, replacementString string: String) -> Bool {
        guard let index = otpTextFields.firstIndex(of: textField) else { return true }

        if string.isEmpty {
            // Backspace
            textField.text = ""
            if index > 0 {
                otpTextFields[index - 1].becomeFirstResponder()
            }
            return false
        }

        if string.count == 1 && string.rangeOfCharacter(from: CharacterSet.decimalDigits.inverted) == nil {
            textField.text = string
            if index < otpTextFields.count - 1 {
                otpTextFields[index + 1].becomeFirstResponder()
            } else {
                textField.resignFirstResponder()
            }
            return false
        }

        return false
    }
}
