package com.tolkflip.activities;

import android.content.Intent;
import android.os.Bundle;
import android.widget.Button;
import android.widget.EditText;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.tolkflip.R;
import com.tolkflip.network.ApiClient;
import com.tolkflip.utils.TokenManager;

import java.util.HashMap;
import java.util.Map;

import retrofit2.Call;
import retrofit2.Callback;
import retrofit2.Response;

public class AuthActivity extends AppCompatActivity {

    private EditText phoneInput;
    private EditText codeInput;
    private Button sendCodeButton;
    private Button verifyButton;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_auth);

        phoneInput = findViewById(R.id.phone_input);
        codeInput = findViewById(R.id.code_input);
        sendCodeButton = findViewById(R.id.send_code_button);
        verifyButton = findViewById(R.id.verify_button);

        sendCodeButton.setOnClickListener(v -> requestCode());
        verifyButton.setOnClickListener(v -> verifyCode());
    }

    private void requestCode() {
        String phoneNumber = phoneInput.getText().toString().trim();
        if (phoneNumber.isEmpty()) {
            Toast.makeText(this, "Enter phone number", Toast.LENGTH_SHORT).show();
            return;
        }

        Map<String, String> body = new HashMap<>();
        body.put("phone_number", phoneNumber);

        ApiClient.getApiService().requestVerificationCode(body).enqueue(new Callback<Map<String, Object>>() {
            @Override
            public void onResponse(Call<Map<String, Object>> call, Response<Map<String, Object>> response) {
                if (response.isSuccessful()) {
                    Toast.makeText(AuthActivity.this, "Code sent!", Toast.LENGTH_SHORT).show();
                    codeInput.setEnabled(true);
                    verifyButton.setEnabled(true);
                } else {
                    Toast.makeText(AuthActivity.this, "Failed to send code", Toast.LENGTH_SHORT).show();
                }
            }

            @Override
            public void onFailure(Call<Map<String, Object>> call, Throwable t) {
                Toast.makeText(AuthActivity.this, "Error: " + t.getMessage(), Toast.LENGTH_SHORT).show();
            }
        });
    }

    private void verifyCode() {
        String phoneNumber = phoneInput.getText().toString().trim();
        String code = codeInput.getText().toString().trim();

        if (code.isEmpty()) {
            Toast.makeText(this, "Enter verification code", Toast.LENGTH_SHORT).show();
            return;
        }

        Map<String, String> body = new HashMap<>();
        body.put("phone_number", phoneNumber);
        body.put("code", code);

        ApiClient.getApiService().verifyCode(body).enqueue(new Callback<Map<String, Object>>() {
            @Override
            public void onResponse(Call<Map<String, Object>> call, Response<Map<String, Object>> response) {
                if (response.isSuccessful() && response.body() != null) {
                    Map<String, Object> data = response.body();
                    String token = (String) data.get("token");
                    String refreshToken = (String) data.get("refreshToken");
                    Map<String, Object> user = (Map<String, Object>) data.get("user");
                    String userId = (String) user.get("userId");

                    TokenManager.getInstance(AuthActivity.this)
                            .saveTokens(token, refreshToken, userId);

                    startActivity(new Intent(AuthActivity.this, MainActivity.class));
                    finish();
                } else {
                    Toast.makeText(AuthActivity.this, "Invalid code", Toast.LENGTH_SHORT).show();
                }
            }

            @Override
            public void onFailure(Call<Map<String, Object>> call, Throwable t) {
                Toast.makeText(AuthActivity.this, "Error: " + t.getMessage(), Toast.LENGTH_SHORT).show();
            }
        });
    }
}
