FROM python:3.10-slim

WORKDIR /app

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    && rm -rf /var/lib/apt/lists/*

# Copy requirements
COPY requirements.txt .

# Install Python dependencies
RUN pip install --no-cache-dir -r requirements.txt

# Download common MarianMT models
RUN python -c "from transformers import MarianMTModel, MarianTokenizer; \
    models = ['en-de', 'de-en', 'en-fr', 'fr-en', 'en-es', 'es-en', 'en-zh', 'zh-en', 'en-ja', 'ja-en', 'en-ar', 'ar-en', 'en-ru', 'ru-en', 'en-hi', 'hi-en']; \
    [MarianTokenizer.from_pretrained(f'Helsinki-NLP/opus-mt-{m}') for m in models]; \
    [MarianMTModel.from_pretrained(f'Helsinki-NLP/opus-mt-{m}') for m in models]"

# Copy application code
COPY src/ ./src/
COPY ../shared/ ./shared/

# Expose port
EXPOSE 3004

# Run with gunicorn
CMD ["gunicorn", "--bind", "0.0.0.0:3004", "--workers", "4", "--timeout", "120", "src.app:app"]
