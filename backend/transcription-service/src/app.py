#!/usr/bin/env python3
"""
Transcription Service using OpenAI Whisper
Transcribes audio messages to text with language detection
"""

import os
import io
import logging
import tempfile
from flask import Flask, request, jsonify
import whisper
import torch
import redis
from cassandra.cluster import Cluster
from dotenv import load_dotenv
import requests

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

# Load Whisper model
device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
logger.info(f"Using device: {device}")

# Load model (using 'base' for balance between speed and accuracy)
# Options: tiny, base, small, medium, large
model_size = os.getenv('WHISPER_MODEL_SIZE', 'base')
logger.info(f"Loading Whisper model: {model_size}")
whisper_model = whisper.load_model(model_size, device=device)
logger.info("Whisper model loaded successfully")

# Translation service URL
TRANSLATION_SERVICE_URL = os.getenv('TRANSLATION_SERVICE_URL', 'http://localhost:3004')


@app.route('/health', methods=['GET'])
def health():
    """Health check endpoint"""
    return jsonify({
        'status': 'healthy',
        'service': 'transcription',
        'model': model_size,
        'device': str(device)
    })


@app.route('/transcribe', methods=['POST'])
def transcribe():
    """Transcribe audio file to text"""
    try:
        # Check if file is present
        if 'audio' not in request.files:
            return jsonify({'error': 'No audio file provided'}), 400

        audio_file = request.files['audio']
        target_language = request.form.get('target_language', None)
        translate_to = request.form.get('translate_to', None)

        if audio_file.filename == '':
            return jsonify({'error': 'No file selected'}), 400

        # Save temporary file
        with tempfile.NamedTemporaryFile(delete=False, suffix='.wav') as temp_audio:
            audio_file.save(temp_audio.name)
            temp_path = temp_audio.name

        try:
            # Transcribe with Whisper
            logger.info("Starting transcription...")
            result = whisper_model.transcribe(
                temp_path,
                language=target_language,
                task='transcribe'
            )

            transcribed_text = result['text']
            detected_language = result['language']

            logger.info(f"Transcription complete. Detected language: {detected_language}")

            response_data = {
                'text': transcribed_text,
                'language': detected_language,
                'segments': []
            }

            # Add segment information if available
            if 'segments' in result:
                response_data['segments'] = [
                    {
                        'start': seg['start'],
                        'end': seg['end'],
                        'text': seg['text']
                    }
                    for seg in result['segments']
                ]

            # Translate if requested
            if translate_to and translate_to != detected_language:
                try:
                    logger.info(f"Translating from {detected_language} to {translate_to}")
                    translation_response = requests.post(
                        f"{TRANSLATION_SERVICE_URL}/translate",
                        json={
                            'text': transcribed_text,
                            'source_language': detected_language,
                            'target_language': translate_to,
                            'detect_emotion': True
                        }
                    )

                    if translation_response.status_code == 200:
                        translation_data = translation_response.json()
                        response_data['translated_text'] = translation_data['translated_text']
                        response_data['translation_confidence'] = translation_data['confidence']
                        response_data['emotion'] = translation_data.get('emotion', 'neutral')
                    else:
                        logger.error(f"Translation failed: {translation_response.text}")

                except Exception as trans_error:
                    logger.error(f"Translation error: {trans_error}")
                    response_data['translation_error'] = str(trans_error)

            return jsonify(response_data)

        finally:
            # Clean up temporary file
            if os.path.exists(temp_path):
                os.remove(temp_path)

    except Exception as e:
        logger.error(f"Transcription failed: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/transcribe/url', methods=['POST'])
def transcribe_url():
    """Transcribe audio from URL"""
    try:
        data = request.get_json()
        audio_url = data.get('url', '')
        target_language = data.get('target_language', None)
        translate_to = data.get('translate_to', None)

        if not audio_url:
            return jsonify({'error': 'Audio URL is required'}), 400

        # Download audio file
        logger.info(f"Downloading audio from: {audio_url}")
        response = requests.get(audio_url, stream=True)

        if response.status_code != 200:
            return jsonify({'error': 'Failed to download audio'}), 400

        # Save to temporary file
        with tempfile.NamedTemporaryFile(delete=False, suffix='.wav') as temp_audio:
            for chunk in response.iter_content(chunk_size=8192):
                temp_audio.write(chunk)
            temp_path = temp_audio.name

        try:
            # Transcribe
            logger.info("Starting transcription...")
            result = whisper_model.transcribe(
                temp_path,
                language=target_language,
                task='transcribe'
            )

            transcribed_text = result['text']
            detected_language = result['language']

            response_data = {
                'text': transcribed_text,
                'language': detected_language,
                'segments': [
                    {
                        'start': seg['start'],
                        'end': seg['end'],
                        'text': seg['text']
                    }
                    for seg in result.get('segments', [])
                ]
            }

            # Translate if requested
            if translate_to and translate_to != detected_language:
                try:
                    translation_response = requests.post(
                        f"{TRANSLATION_SERVICE_URL}/translate",
                        json={
                            'text': transcribed_text,
                            'source_language': detected_language,
                            'target_language': translate_to,
                            'detect_emotion': True
                        }
                    )

                    if translation_response.status_code == 200:
                        translation_data = translation_response.json()
                        response_data['translated_text'] = translation_data['translated_text']
                        response_data['emotion'] = translation_data.get('emotion', 'neutral')

                except Exception as trans_error:
                    logger.error(f"Translation error: {trans_error}")

            return jsonify(response_data)

        finally:
            # Clean up
            if os.path.exists(temp_path):
                os.remove(temp_path)

    except Exception as e:
        logger.error(f"Transcription from URL failed: {e}")
        return jsonify({'error': str(e)}), 500


@app.route('/languages', methods=['GET'])
def get_supported_languages():
    """Get list of languages supported by Whisper"""
    languages = {
        'en': 'English',
        'zh': 'Chinese',
        'de': 'German',
        'es': 'Spanish',
        'ru': 'Russian',
        'ko': 'Korean',
        'fr': 'French',
        'ja': 'Japanese',
        'pt': 'Portuguese',
        'tr': 'Turkish',
        'pl': 'Polish',
        'ca': 'Catalan',
        'nl': 'Dutch',
        'ar': 'Arabic',
        'sv': 'Swedish',
        'it': 'Italian',
        'id': 'Indonesian',
        'hi': 'Hindi',
        'fi': 'Finnish',
        'vi': 'Vietnamese',
        'he': 'Hebrew',
        'uk': 'Ukrainian',
        'el': 'Greek',
        'ms': 'Malay',
        'cs': 'Czech',
        'ro': 'Romanian',
        'da': 'Danish',
        'hu': 'Hungarian',
        'ta': 'Tamil',
        'no': 'Norwegian',
        'th': 'Thai',
        'ur': 'Urdu',
        'hr': 'Croatian',
        'bg': 'Bulgarian',
        'lt': 'Lithuanian',
        'la': 'Latin',
        'mi': 'Maori',
        'ml': 'Malayalam',
        'cy': 'Welsh',
        'sk': 'Slovak',
        'te': 'Telugu',
        'fa': 'Persian',
        'lv': 'Latvian',
        'bn': 'Bengali',
        'sr': 'Serbian',
        'az': 'Azerbaijani',
        'sl': 'Slovenian',
        'kn': 'Kannada',
        'et': 'Estonian',
        'mk': 'Macedonian',
        'br': 'Breton',
        'eu': 'Basque',
        'is': 'Icelandic',
        'hy': 'Armenian',
        'ne': 'Nepali',
        'mn': 'Mongolian',
        'bs': 'Bosnian',
        'kk': 'Kazakh',
        'sq': 'Albanian',
        'sw': 'Swahili',
        'gl': 'Galician',
        'mr': 'Marathi',
        'pa': 'Punjabi',
        'si': 'Sinhala',
        'km': 'Khmer',
        'sn': 'Shona',
        'yo': 'Yoruba',
        'so': 'Somali',
        'af': 'Afrikaans',
        'oc': 'Occitan',
        'ka': 'Georgian',
        'be': 'Belarusian',
        'tg': 'Tajik',
        'sd': 'Sindhi',
        'gu': 'Gujarati',
        'am': 'Amharic',
        'yi': 'Yiddish',
        'lo': 'Lao',
        'uz': 'Uzbek',
        'fo': 'Faroese',
        'ht': 'Haitian Creole',
        'ps': 'Pashto',
        'tk': 'Turkmen',
        'nn': 'Nynorsk',
        'mt': 'Maltese',
        'sa': 'Sanskrit',
        'lb': 'Luxembourgish',
        'my': 'Myanmar',
        'bo': 'Tibetan',
        'tl': 'Tagalog',
        'mg': 'Malagasy',
        'as': 'Assamese',
        'tt': 'Tatar',
        'haw': 'Hawaiian',
        'ln': 'Lingala',
        'ha': 'Hausa',
        'ba': 'Bashkir',
        'jw': 'Javanese',
        'su': 'Sundanese'
    }

    return jsonify({'languages': languages})


if __name__ == '__main__':
    port = int(os.getenv('PORT', 3005))
    app.run(host='0.0.0.0', port=port, debug=False)
