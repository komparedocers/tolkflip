/**
 * Tolkflip Login Page - Authentication Flow
 * Handles phone verification and user registration/login
 */

// API Configuration
const API_BASE_URL = 'http://localhost:3000/api/v1';

// State Management
const state = {
    phoneNumber: '',
    countryCode: '+1',
    verificationCode: '',
    displayName: '',
    primaryLanguage: 'en',
    additionalLanguages: [],
    currentStep: 'phone', // 'phone', 'verify', 'register'
    isExistingUser: false,
    authToken: null,
    userId: null
};

// DOM Elements
let elements = {};

// Initialize on DOM load
document.addEventListener('DOMContentLoaded', () => {
    initializeElements();
    attachEventListeners();
    loadCountrySelector();
    checkExistingSession();
});

/**
 * Initialize DOM element references
 */
function initializeElements() {
    elements = {
        // Phone Step
        phoneStep: document.getElementById('phone-step'),
        countryCodeSelect: document.getElementById('country-code'),
        phoneInput: document.getElementById('phone-number'),
        continueBtn: document.getElementById('continue-btn'),

        // Verification Step
        verifyStep: document.getElementById('verify-step'),
        codeInputs: document.querySelectorAll('.code-input'),
        verifyBtn: document.getElementById('verify-btn'),
        resendBtn: document.getElementById('resend-btn'),
        backToPhoneBtn: document.getElementById('back-to-phone'),

        // Registration Step
        registerStep: document.getElementById('register-step'),
        displayNameInput: document.getElementById('display-name'),
        primaryLangSelect: document.getElementById('primary-language'),
        additionalLangsSelect: document.getElementById('additional-languages'),
        registerBtn: document.getElementById('register-btn'),

        // Loading overlay
        loadingOverlay: document.getElementById('loading-overlay'),
        errorMessage: document.getElementById('error-message')
    };
}

/**
 * Attach event listeners
 */
function attachEventListeners() {
    // Phone step
    elements.phoneInput.addEventListener('input', validatePhoneInput);
    elements.continueBtn.addEventListener('click', handleContinue);

    // Verification step
    elements.codeInputs.forEach((input, index) => {
        input.addEventListener('input', (e) => handleCodeInput(e, index));
        input.addEventListener('keydown', (e) => handleCodeKeydown(e, index));
        input.addEventListener('paste', handleCodePaste);
    });
    elements.verifyBtn.addEventListener('click', handleVerify);
    elements.resendBtn.addEventListener('click', handleResend);
    elements.backToPhoneBtn.addEventListener('click', () => showStep('phone'));

    // Registration step
    elements.displayNameInput.addEventListener('input', validateRegistrationForm);
    elements.primaryLangSelect.addEventListener('change', validateRegistrationForm);
    elements.registerBtn.addEventListener('click', handleRegister);

    // Enter key handlers
    elements.phoneInput.addEventListener('keypress', (e) => {
        if (e.key === 'Enter') handleContinue();
    });
}

/**
 * Load country code selector with flags
 */
function loadCountrySelector() {
    const countries = [
        { code: '+1', name: 'United States', flag: '🇺🇸' },
        { code: '+44', name: 'United Kingdom', flag: '🇬🇧' },
        { code: '+33', name: 'France', flag: '🇫🇷' },
        { code: '+49', name: 'Germany', flag: '🇩🇪' },
        { code: '+81', name: 'Japan', flag: '🇯🇵' },
        { code: '+86', name: 'China', flag: '🇨🇳' },
        { code: '+91', name: 'India', flag: '🇮🇳' },
        { code: '+34', name: 'Spain', flag: '🇪🇸' },
        { code: '+39', name: 'Italy', flag: '🇮🇹' },
        { code: '+55', name: 'Brazil', flag: '🇧🇷' }
    ];

    elements.countryCodeSelect.innerHTML = countries.map(country =>
        `<option value="${country.code}">${country.flag} ${country.code} ${country.name}</option>`
    ).join('');
}

/**
 * Check for existing session
 */
function checkExistingSession() {
    const token = localStorage.getItem('authToken');
    const userId = localStorage.getItem('userId');

    if (token && userId) {
        // Redirect to chat
        window.location.href = '/chat.html';
    }
}

/**
 * Validate phone number input
 */
function validatePhoneInput() {
    const phone = elements.phoneInput.value.replace(/\D/g, '');
    elements.phoneInput.value = phone;
    elements.continueBtn.disabled = phone.length < 10;
}

/**
 * Handle Continue button - Request verification code
 */
async function handleContinue() {
    state.countryCode = elements.countryCodeSelect.value;
    state.phoneNumber = elements.phoneInput.value.replace(/\D/g, '');

    if (state.phoneNumber.length < 10) {
        showError('Please enter a valid phone number');
        return;
    }

    showLoading(true);

    try {
        const response = await fetch(`${API_BASE_URL}/auth/request-code`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                phone_number: state.countryCode + state.phoneNumber
            })
        });

        const data = await response.json();

        if (data.success) {
            // For development, show the code in console
            if (data.code) {
                console.log('Verification code:', data.code);
            }
            showStep('verify');
            startResendTimer();
        } else {
            showError(data.error || 'Failed to send verification code');
        }
    } catch (error) {
        console.error('Request code error:', error);
        showError('Network error. Please try again.');
    } finally {
        showLoading(false);
    }
}

/**
 * Handle code input with auto-focus
 */
function handleCodeInput(e, index) {
    const value = e.target.value;

    if (value.length === 1 && index < elements.codeInputs.length - 1) {
        elements.codeInputs[index + 1].focus();
    }

    // Update verify button state
    const code = getVerificationCode();
    elements.verifyBtn.disabled = code.length !== 6;
    state.verificationCode = code;
}

/**
 * Handle code input keydown (backspace navigation)
 */
function handleCodeKeydown(e, index) {
    if (e.key === 'Backspace' && !e.target.value && index > 0) {
        elements.codeInputs[index - 1].focus();
    }
}

/**
 * Handle paste into code inputs
 */
function handleCodePaste(e) {
    e.preventDefault();
    const pastedData = e.clipboardData.getData('text').replace(/\D/g, '').slice(0, 6);

    pastedData.split('').forEach((digit, index) => {
        if (elements.codeInputs[index]) {
            elements.codeInputs[index].value = digit;
        }
    });

    if (pastedData.length === 6) {
        elements.verifyBtn.disabled = false;
        state.verificationCode = pastedData;
    }
}

/**
 * Get verification code from inputs
 */
function getVerificationCode() {
    return Array.from(elements.codeInputs).map(input => input.value).join('');
}

/**
 * Handle Verify button - Verify code and login/register
 */
async function handleVerify() {
    const code = getVerificationCode();

    if (code.length !== 6) {
        showError('Please enter the complete 6-digit code');
        return;
    }

    showLoading(true);

    try {
        // First verify the code
        const verifyResponse = await fetch(`${API_BASE_URL}/auth/verify-code`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                phone_number: state.countryCode + state.phoneNumber,
                code: code
            })
        });

        const verifyData = await verifyResponse.json();

        if (!verifyData.success) {
            showError(verifyData.error || 'Invalid verification code');
            showLoading(false);
            return;
        }

        // Try to login (check if user exists)
        const loginResponse = await fetch(`${API_BASE_URL}/auth/login`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                phone_number: state.countryCode + state.phoneNumber
            })
        });

        const loginData = await loginResponse.json();

        if (loginData.success) {
            // Existing user - save token and redirect
            saveSession(loginData);
            window.location.href = '/chat.html';
        } else {
            // New user - show registration form
            state.isExistingUser = false;
            showStep('register');
        }
    } catch (error) {
        console.error('Verify error:', error);
        showError('Network error. Please try again.');
    } finally {
        showLoading(false);
    }
}

/**
 * Handle Resend button
 */
async function handleResend() {
    elements.resendBtn.disabled = true;
    await handleContinue();
}

/**
 * Start resend timer (60 seconds)
 */
function startResendTimer() {
    let seconds = 60;
    elements.resendBtn.disabled = true;
    elements.resendBtn.textContent = `Resend code in ${seconds}s`;

    const timer = setInterval(() => {
        seconds--;
        if (seconds > 0) {
            elements.resendBtn.textContent = `Resend code in ${seconds}s`;
        } else {
            clearInterval(timer);
            elements.resendBtn.disabled = false;
            elements.resendBtn.textContent = 'Resend code';
        }
    }, 1000);
}

/**
 * Validate registration form
 */
function validateRegistrationForm() {
    const displayName = elements.displayNameInput.value.trim();
    const primaryLang = elements.primaryLangSelect.value;

    elements.registerBtn.disabled = !displayName || !primaryLang;
}

/**
 * Handle Register button - Create new user
 */
async function handleRegister() {
    state.displayName = elements.displayNameInput.value.trim();
    state.primaryLanguage = elements.primaryLangSelect.value;
    state.additionalLanguages = Array.from(elements.additionalLangsSelect.selectedOptions)
        .map(option => option.value);

    if (!state.displayName) {
        showError('Please enter your display name');
        return;
    }

    showLoading(true);

    try {
        const response = await fetch(`${API_BASE_URL}/auth/register`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                phone_number: state.countryCode + state.phoneNumber,
                display_name: state.displayName,
                primary_language: state.primaryLanguage,
                additional_languages: state.additionalLanguages
            })
        });

        const data = await response.json();

        if (data.success) {
            saveSession(data);
            window.location.href = '/chat.html';
        } else {
            showError(data.error || 'Registration failed');
        }
    } catch (error) {
        console.error('Register error:', error);
        showError('Network error. Please try again.');
    } finally {
        showLoading(false);
    }
}

/**
 * Save session to localStorage
 */
function saveSession(data) {
    localStorage.setItem('authToken', data.access_token);
    localStorage.setItem('refreshToken', data.refresh_token);
    localStorage.setItem('userId', data.user_id);
    localStorage.setItem('phoneNumber', state.countryCode + state.phoneNumber);
}

/**
 * Show specific step and hide others
 */
function showStep(step) {
    elements.phoneStep.classList.toggle('active', step === 'phone');
    elements.verifyStep.classList.toggle('active', step === 'verify');
    elements.registerStep.classList.toggle('active', step === 'register');

    state.currentStep = step;

    // Clear error message
    hideError();

    // Focus appropriate input
    if (step === 'phone') {
        elements.phoneInput.focus();
    } else if (step === 'verify') {
        elements.codeInputs[0].focus();
    } else if (step === 'register') {
        elements.displayNameInput.focus();
    }
}

/**
 * Show/hide loading overlay
 */
function showLoading(show) {
    if (elements.loadingOverlay) {
        elements.loadingOverlay.style.display = show ? 'flex' : 'none';
    }
}

/**
 * Show error message
 */
function showError(message) {
    if (elements.errorMessage) {
        elements.errorMessage.textContent = message;
        elements.errorMessage.style.display = 'block';

        // Auto-hide after 5 seconds
        setTimeout(() => hideError(), 5000);
    }
}

/**
 * Hide error message
 */
function hideError() {
    if (elements.errorMessage) {
        elements.errorMessage.style.display = 'none';
    }
}
