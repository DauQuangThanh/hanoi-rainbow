# AI/ML Services

## Table of Contents
- [Watson Assistant](#watson-assistant)
- [Watson Studio](#watson-studio)
- [Watson Machine Learning](#watson-machine-learning)
- [Watson Natural Language Processing](#watson-natural-language-processing)
- [Watson Speech Services](#watson-speech-services)
- [Watson Vision](#watson-vision)

---

## Watson Assistant

### Overview

Build conversational interfaces with AI-powered chatbots.

### Create Watson Assistant

```bash
# Create Watson Assistant instance
ibmcloud resource service-instance-create my-watson-assistant \
  conversation plus us-south

# Create credentials
ibmcloud resource service-key-create my-wa-creds \
  Manager --instance-name my-watson-assistant
```

### Build Chatbot

**Node.js SDK:**
```javascript
const AssistantV2 = require('ibm-watson/assistant/v2');
const { IamAuthenticator } = require('ibm-watson/auth');

const assistant = new AssistantV2({
  version: '2023-06-15',
  authenticator: new IamAuthenticator({
    apikey: '<API-KEY>',
  }),
  serviceUrl: '<SERVICE-URL>',
});

// Create session
const session = await assistant.createSession({
  assistantId: '<ASSISTANT-ID>',
});

// Send message
const response = await assistant.message({
  assistantId: '<ASSISTANT-ID>',
  sessionId: session.result.session_id,
  input: {
    message_type: 'text',
    text: 'Hello, I need help with my order',
  },
});

console.log(JSON.stringify(response.result, null, 2));
```

---

## Watson Studio

### Overview

Collaborative platform for data science and ML model development.

### Create Project

```bash
# Create Watson Studio instance
ibmcloud resource service-instance-create my-watson-studio \
  data-science-experience free-v1 us-south
```

### Jupyter Notebooks

Access via Watson Studio web UI:
- Python, R, Scala notebooks
- GPU acceleration
- Collaborative editing
- Version control integration

---

## Watson Machine Learning

### Overview

Deploy and manage machine learning models at scale.

### Train and Deploy Model

**Python SDK:**
```python
from ibm_watson_machine_learning import APIClient

# Initialize client
wml_credentials = {
    "url": "<SERVICE-URL>",
    "apikey": "<API-KEY>"
}
client = APIClient(wml_credentials)

# Set space
client.set.default_space('<SPACE-ID>')

# Train model
import sklearn
from sklearn.linear_model import LogisticRegression

model = LogisticRegression()
model.fit(X_train, y_train)

# Store model
model_props = {
    client.repository.ModelMetaNames.NAME: "My Model",
    client.repository.ModelMetaNames.TYPE: "scikit-learn_1.1",
    client.repository.ModelMetaNames.SOFTWARE_SPEC_UID: client.software_specifications.get_uid_by_name("default_py3.10")
}

model_details = client.repository.store_model(
    model=model,
    meta_props=model_props,
    training_data=training_data
)

# Deploy model
deployment = client.deployments.create(
    model_details['metadata']['id'],
    meta_props={
        client.deployments.ConfigurationMetaNames.NAME: "My Deployment",
        client.deployments.ConfigurationMetaNames.ONLINE: {}
    }
)

# Score
scoring_payload = {
    "input_data": [{
        "values": [[1.0, 2.0, 3.0, 4.0]]
    }]
}

predictions = client.deployments.score(
    deployment['metadata']['id'],
    scoring_payload
)
print(predictions)
```

---

## Watson Natural Language Processing

### Natural Language Understanding

**Python:**
```python
from ibm_watson import NaturalLanguageUnderstandingV1
from ibm_watson.natural_language_understanding_v1 import Features, EntitiesOptions, KeywordsOptions, SentimentOptions

nlu = NaturalLanguageUnderstandingV1(
    version='2022-04-07',
    authenticator=IAMAuthenticator('<API-KEY>')
)
nlu.set_service_url('<SERVICE-URL>')

# Analyze text
response = nlu.analyze(
    text='IBM is an American multinational technology company with headquarters in Armonk, New York.',
    features=Features(
        entities=EntitiesOptions(sentiment=True, limit=2),
        keywords=KeywordsOptions(sentiment=True, limit=2),
        sentiment=SentimentOptions()
    )
).get_result()

print(json.dumps(response, indent=2))
```

### Language Translator

**Python:**
```python
from ibm_watson import LanguageTranslatorV3

translator = LanguageTranslatorV3(
    version='2018-05-01',
    authenticator=IAMAuthenticator('<API-KEY>')
)
translator.set_service_url('<SERVICE-URL>')

# Translate text
translation = translator.translate(
    text='Hello, how are you?',
    source='en',
    target='es'
).get_result()

print(translation['translations'][0]['translation'])
```

---

## Watson Speech Services

### Speech to Text

**Python:**
```python
from ibm_watson import SpeechToTextV1

stt = SpeechToTextV1(
    authenticator=IAMAuthenticator('<API-KEY>')
)
stt.set_service_url('<SERVICE-URL>')

# Transcribe audio
with open('audio.mp3', 'rb') as audio_file:
    response = stt.recognize(
        audio=audio_file,
        content_type='audio/mp3',
        model='en-US_BroadbandModel'
    ).get_result()

print(response['results'][0]['alternatives'][0]['transcript'])
```

### Text to Speech

**Python:**
```python
from ibm_watson import TextToSpeechV1

tts = TextToSpeechV1(
    authenticator=IAMAuthenticator('<API-KEY>')
)
tts.set_service_url('<SERVICE-URL>')

# Synthesize speech
with open('output.mp3', 'wb') as audio_file:
    response = tts.synthesize(
        text='Hello, welcome to IBM Cloud Watson services',
        voice='en-US_AllisonV3Voice',
        accept='audio/mp3'
    ).get_result()
    audio_file.write(response.content)
```

---

## Watson Vision

### Visual Recognition

**Python:**
```python
from ibm_watson import VisualRecognitionV4

vr = VisualRecognitionV4(
    version='2019-02-11',
    authenticator=IAMAuthenticator('<API-KEY>')
)
vr.set_service_url('<SERVICE-URL>')

# Analyze image
with open('image.jpg', 'rb') as image_file:
    response = vr.analyze(
        collection_ids=['<COLLECTION-ID>'],
        features=['objects'],
        images_file=[image_file]
    ).get_result()

print(json.dumps(response, indent=2))
```

---

## Best Practices

### Model Development

1. **Data Quality**: Clean, labeled training data
2. **Feature Engineering**: Extract meaningful features
3. **Model Selection**: Choose appropriate algorithms
4. **Hyperparameter Tuning**: Optimize model parameters
5. **Cross-Validation**: Evaluate generalization

### Deployment

1. **Monitoring**: Track model performance
2. **Versioning**: Maintain model versions
3. **A/B Testing**: Compare model variants
4. **Scaling**: Auto-scale inference endpoints
5. **Security**: Secure API endpoints with IAM

### Cost Optimization

1. **Batch Inference**: Use batch for bulk predictions
2. **Model Caching**: Cache frequent predictions
3. **Right-Size**: Match compute to workload
4. **Free Tiers**: Leverage lite plans for development
5. **Monitoring**: Track API usage and costs
