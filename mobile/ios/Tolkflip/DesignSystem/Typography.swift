import SwiftUI

/// Professional Typography System for Tolkflip
extension Font {
    // MARK: - Display Fonts (Headlines)
    static let displayLarge = Font.system(size: 36, weight: .bold, design: .rounded)
    static let displayMedium = Font.system(size: 28, weight: .bold, design: .rounded)
    static let displaySmall = Font.system(size: 24, weight: .semibold, design: .rounded)

    // MARK: - Title Fonts
    static let titleLarge = Font.system(size: 22, weight: .semibold)
    static let titleMedium = Font.system(size: 18, weight: .semibold)
    static let titleSmall = Font.system(size: 16, weight: .medium)

    // MARK: - Body Fonts
    static let bodyLarge = Font.system(size: 16, weight: .regular)
    static let bodyMedium = Font.system(size: 15, weight: .regular)
    static let bodySmall = Font.system(size: 14, weight: .regular)

    // MARK: - Label Fonts
    static let labelLarge = Font.system(size: 14, weight: .medium)
    static let labelMedium = Font.system(size: 12, weight: .medium)
    static let labelSmall = Font.system(size: 11, weight: .medium)

    // MARK: - Custom Modifiers
    static func custom(size: CGFloat, weight: Font.Weight = .regular) -> Font {
        return Font.system(size: size, weight: weight)
    }
}

// MARK: - Text Styles
extension Text {
    func displayLarge() -> some View {
        self.font(.displayLarge)
            .foregroundColor(.textPrimary)
    }

    func displayMedium() -> some View {
        self.font(.displayMedium)
            .foregroundColor(.textPrimary)
    }

    func titleLarge() -> some View {
        self.font(.titleLarge)
            .foregroundColor(.textPrimary)
    }

    func titleMedium() -> some View {
        self.font(.titleMedium)
            .foregroundColor(.textPrimary)
    }

    func bodyLarge() -> some View {
        self.font(.bodyLarge)
            .foregroundColor(.textPrimary)
    }

    func bodyMedium() -> some View {
        self.font(.bodyMedium)
            .foregroundColor(.textSecondary)
    }

    func labelMedium() -> some View {
        self.font(.labelMedium)
            .foregroundColor(.textSecondary)
    }
}
