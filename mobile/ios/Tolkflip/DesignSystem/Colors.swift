import SwiftUI

/// Professional Color System for Tolkflip
/// Modern purple/blue gradient with vibrant accents
extension Color {
    // MARK: - Primary Brand Colors
    static let primary50 = Color(hex: "F0F4FF")
    static let primary100 = Color(hex: "E0E7FF")
    static let primary200 = Color(hex: "C7D2FE")
    static let primary300 = Color(hex: "A5B4FC")
    static let primary400 = Color(hex: "818CF8")
    static let primary500 = Color(hex: "6366F1")
    static let primary600 = Color(hex: "4F46E5")
    static let primary700 = Color(hex: "4338CA")
    static let primary800 = Color(hex: "3730A3")
    static let primary900 = Color(hex: "312E81")

    // MARK: - Secondary Colors
    static let secondary50 = Color(hex: "FDF4FF")
    static let secondary100 = Color(hex: "FAE8FF")
    static let secondary200 = Color(hex: "F5D0FE")
    static let secondary300 = Color(hex: "F0ABFC")
    static let secondary400 = Color(hex: "E879F9")
    static let secondary500 = Color(hex: "D946EF")
    static let secondary600 = Color(hex: "C026D3")
    static let secondary700 = Color(hex: "A21CAF")
    static let secondary800 = Color(hex: "86198F")
    static let secondary900 = Color(hex: "701A75")

    // MARK: - Neutral Grays
    static let gray50 = Color(hex: "F9FAFB")
    static let gray100 = Color(hex: "F3F4F6")
    static let gray200 = Color(hex: "E5E7EB")
    static let gray300 = Color(hex: "D1D5DB")
    static let gray400 = Color(hex: "9CA3AF")
    static let gray500 = Color(hex: "6B7280")
    static let gray600 = Color(hex: "4B5563")
    static let gray700 = Color(hex: "374151")
    static let gray800 = Color(hex: "1F2937")
    static let gray900 = Color(hex: "111827")

    // MARK: - Semantic Colors
    static let success = Color(hex: "10B981")
    static let successLight = Color(hex: "D1FAE5")
    static let errorRed = Color(hex: "EF4444")
    static let errorLight = Color(hex: "FEE2E2")
    static let warning = Color(hex: "F59E0B")
    static let warningLight = Color(hex: "FEF3C7")
    static let info = Color(hex: "3B82F6")
    static let infoLight = Color(hex: "DBEAFE")

    // MARK: - Background Colors
    static let bgPrimary = Color.white
    static let bgSecondary = Color(hex: "F9FAFB")
    static let bgTertiary = Color(hex: "F3F4F6")

    // MARK: - Text Colors
    static let textPrimary = Color(hex: "111827")
    static let textSecondary = Color(hex: "6B7280")
    static let textTertiary = Color(hex: "9CA3AF")
    static let textInverse = Color.white

    // MARK: - Chat Bubble Colors
    static let chatBubbleSent = LinearGradient(
        colors: [Color.primary600, Color.primary700],
        startPoint: .topLeading,
        endPoint: .bottomTrailing
    )
    static let chatBubbleReceived = Color.white

    // MARK: - Helper Function
    init(hex: String) {
        let hex = hex.trimmingCharacters(in: CharacterSet.alphanumerics.inverted)
        var int: UInt64 = 0
        Scanner(string: hex).scanHexInt64(&int)
        let a, r, g, b: UInt64
        switch hex.count {
        case 3: // RGB (12-bit)
            (a, r, g, b) = (255, (int >> 8) * 17, (int >> 4 & 0xF) * 17, (int & 0xF) * 17)
        case 6: // RGB (24-bit)
            (a, r, g, b) = (255, int >> 16, int >> 8 & 0xFF, int & 0xFF)
        case 8: // ARGB (32-bit)
            (a, r, g, b) = (int >> 24, int >> 16 & 0xFF, int >> 8 & 0xFF, int & 0xFF)
        default:
            (a, r, g, b) = (255, 0, 0, 0)
        }
        self.init(
            .sRGB,
            red: Double(r) / 255,
            green: Double(g) / 255,
            blue: Double(b) / 255,
            opacity: Double(a) / 255
        )
    }
}

// MARK: - Color Scheme Extensions
extension Color {
    /// Returns appropriate text color for a given background
    func contrastingTextColor() -> Color {
        let components = UIColor(self).cgColor.components ?? [0, 0, 0, 1]
        let r = components[0]
        let g = components[1]
        let b = components[2]

        // Calculate relative luminance
        let luminance = 0.2126 * r + 0.7152 * g + 0.0722 * b

        return luminance > 0.5 ? .textPrimary : .textInverse
    }
}
