#!/usr/bin/env python3
"""
Translation Service using MarianNMT
Provides multilingual translation with emotion detection
"""

import os
import json
import logging
from flask import Flask, request, jsonify
from transformers import MarianMTModel, MarianTokenizer
import torch
from textblob import TextBlob
from langdetect import detect
import redis
from cassandra.cluster import Cluster
from cassandra.auth import PlainTextAuthProvider
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

app = Flask(__name__)

# Redis connection
redis_client = redis.Redis(
    host=os.getenv('REDIS_HOST', 'localhost'),
    port=int(os.getenv('REDIS_PORT', 6379)),
    decode_responses=True
)

# Cassandra connection
cassandra_hosts = os.getenv('CASSANDRA_HOSTS', '127.0.0.1').split(',')
cluster = Cluster(cassandra_hosts)
cassandra_session = cluster.connect('tolkflip')

# Load language configuration
with open('../shared/config/languages.json', 'r') as f:
    language_config = json.load(f)

# Model cache
model_cache = {}
tokenizer_cache = {}

# Device configuration
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
logger.info(f"Using device: {device}")


def get_model_name(source_lang, target_lang):
    """Get the appropriate MarianMT model name for language pair"""
    # MarianMT model naming convention
    return f"Helsinki-NLP/opus-mt-{source_lang}-{target_lang}"


def load_model(source_lang, target_lang):
    """Load or retrieve cached MarianMT model"""
    model_key = f"{source_lang}-{target_lang}"

    if model_key in model_cache:
        return model_cache[model_key], tokenizer_cache[model_key]

    try:
        model_name = get_model_name(source_lang, target_lang)
        logger.info(f"Loading model: {model_name}")

        tokenizer = MarianTokenizer.from_pretrained(model_name)
        model = MarianMTModel.from_pretrained(model_name).to(device)

        model_cache[model_key] = model
        tokenizer_cache[model_key] = tokenizer

        return model, tokenizer
    except Exception as e:
        # Try reverse direction
        try:
            model_name = get_model_name(target_lang, source_lang)
            logger.info(f"Trying reverse model: {model_name}")

            tokenizer = MarianTokenizer.from_pretrained(model_name)
            model = MarianMTModel.from_pretrained(model_name).to(device)

            return model, tokenizer
        except Exception as e2:
            logger.error(f"Failed to load model: {e2}")
            raise


def translate_text(text, source_lang, target_lang):
    """Translate text from source to target language"""
    try:
        # Check cache first
        cache_key = f"translation:{source_lang}:{target_lang}:{text[:100]}"
        cached = redis_client.get(cache_key)

        if cached:
            logger.info("Translation found in cache")
            return json.loads(cached)

        # Load model
        model, tokenizer = load_model(source_lang, target_lang)

        # Tokenize
        inputs = tokenizer(text, return_tensors="pt", padding=True, truncation=True, max_length=512)
        inputs = {k: v.to(device) for k, v in inputs.items()}

        # Translate
        with torch.no_grad():
            translated_tokens = model.generate(**inputs)

        translated_text = tokenizer.decode(translated_tokens[0], skip_special_tokens=True)

        # Calculate confidence (simplified)
        confidence = 0.95  # In production, use proper confidence scoring

        result = {
            'translated_text': translated_text,
            'source_language': source_lang,
            'target_language': target_lang,
            'confidence': confidence
        }

        # Cache result (TTL: 1 hour)
        redis_client.setex(cache_key, 3600, json.dumps(result))

        # Store in Cassandra for analytics
        try:
            query = """
                INSERT INTO translation_cache (source_text, source_lang, target_lang,
                                              translated_text, confidence, cached_at)
                VALUES (%s, %s, %s, %s, %s, toTimestamp(now()))
                USING TTL 2592000
            """
            cassandra_session.execute(
                query,
                (text[:500], source_lang, target_lang, translated_text, confidence)
            )
        except Exception as db_error:
            logger.error(f"Failed to cache in Cassandra: {db_error}")

        return result
    except Exception as e:
        logger.error(f"Translation error: {e}")
        raise


def detect_emotion(text):
    """Detect emotion/sentiment from text"""
    try:
        blob = TextBlob(text)
        polarity = blob.sentiment.polarity

        # Simple emotion classification
        if polarity > 0.5:
            return "very_positive"
        elif polarity > 0.1:
            return "positive"
        elif polarity < -0.5:
            return "very_negative"
        elif polarity < -0.1:
            return "negative"
        else:
            return "neutral"
    except Exception as e:
        logger.error(f"Emotion detection error: {e}")
        return "neutral"


def auto_detect_language(text):
    """Auto-detect source language"""
    try:
        return detect(text)
    except Exception as e:
        logger.error(f"Language detection error: {e}")
        return "en"  # Default to English


@app.route('/health', methods=['GET'])
def health():
    """Health check endpoint"""
    return jsonify({
        'status': 'healthy',
        'service': 'translation',
        'models_loaded': len(model_cache),
        'device': str(device)
    })


@app.route('/translate', methods=['POST'])
def translate():
    """Translate text endpoint"""
    try:
        data = request.get_json()

        text = data.get('text', '')
        source_lang = data.get('source_language', '')
        target_lang = data.get('target_language', 'en')
        detect_emotions = data.get('detect_emotion', True)

        if not text:
            return jsonify({'error': 'Text is required'}), 400

        # Auto-detect source language if not provided
        if not source_lang:
            source_lang = auto_detect_language(text)
            logger.info(f"Auto-detected language: {source_lang}")

        # Don't translate if same language
        if source_lang == target_lang:
            result = {
                'translated_text': text,
                'source_language': source_lang,
                'target_language': target_lang,
                'confidence': 1.0
            }
        else:
            # Perform translation
            result = translate_text(text, source_lang, target_lang)

        # Add emotion detection if requested
        if detect_emotions:
            result['emotion'] = detect_emotion(text)

        return jsonify(result)

    except Exception as e:
        logger.error(f"Translation request failed: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/translate/batch', methods=['POST'])
def translate_batch():
    """Translate multiple texts in batch"""
    try:
        data = request.get_json()

        texts = data.get('texts', [])
        source_lang = data.get('source_language', '')
        target_lang = data.get('target_language', 'en')

        if not texts:
            return jsonify({'error': 'Texts array is required'}), 400

        results = []
        for text in texts:
            if not source_lang:
                detected_lang = auto_detect_language(text)
            else:
                detected_lang = source_lang

            if detected_lang == target_lang:
                results.append({
                    'translated_text': text,
                    'source_language': detected_lang,
                    'target_language': target_lang,
                    'confidence': 1.0
                })
            else:
                result = translate_text(text, detected_lang, target_lang)
                results.append(result)

        return jsonify({'translations': results})

    except Exception as e:
        logger.error(f"Batch translation failed: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/languages', methods=['GET'])
def get_supported_languages():
    """Get list of supported languages"""
    return jsonify(language_config)


@app.route('/detect-language', methods=['POST'])
def detect_language():
    """Detect language of given text"""
    try:
        data = request.get_json()
        text = data.get('text', '')

        if not text:
            return jsonify({'error': 'Text is required'}), 400

        detected_lang = auto_detect_language(text)

        return jsonify({
            'detected_language': detected_lang,
            'text': text
        })

    except Exception as e:
        logger.error(f"Language detection failed: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/emotion', methods=['POST'])
def analyze_emotion():
    """Analyze emotion/sentiment of text"""
    try:
        data = request.get_json()
        text = data.get('text', '')

        if not text:
            return jsonify({'error': 'Text is required'}), 400

        emotion = detect_emotion(text)

        blob = TextBlob(text)

        return jsonify({
            'emotion': emotion,
            'polarity': blob.sentiment.polarity,
            'subjectivity': blob.sentiment.subjectivity
        })

    except Exception as e:
        logger.error(f"Emotion analysis failed: {e}")
        return jsonify({'error': str(e)}), 500


if __name__ == '__main__':
    port = int(os.getenv('PORT', 3004))
    app.run(host='0.0.0.0', port=port, debug=False)
