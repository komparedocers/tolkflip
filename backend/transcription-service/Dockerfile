FROM python:3.10-slim

WORKDIR /app

# Install system dependencies including ffmpeg
RUN apt-get update && apt-get install -y \
    ffmpeg \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Download Whisper model
RUN python -c "import whisper; whisper.load_model('base')"

# Copy application code
COPY src/ ./src/

# Expose port
EXPOSE 3005

# Run with gunicorn
CMD ["gunicorn", "--bind", "0.0.0.0:3005", "--workers", "2", "--timeout", "300", "src.app:app"]
