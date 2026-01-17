# Integration Examples

## Spring Boot Integration

### Maven Dependency

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-oauth2-resource-server</artifactId>
</dependency>
```

### Application Configuration

```yaml
# application.yml
spring:
  security:
    oauth2:
      client:
        registration:
          keycloak:
            client-id: spring-app
            client-secret: {client-secret}
            authorization-grant-type: authorization_code
            scope: openid, profile, email
        provider:
          keycloak:
            issuer-uri: https://keycloak.example.com/realms/my-realm
```

### Security Configuration

```java
@Configuration
@EnableWebSecurity
public class SecurityConfig {
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
            .authorizeHttpRequests(authz -> authz
                .requestMatchers("/public/**").permitAll()
                .anyRequest().authenticated()
            )
            .oauth2Login()
            .and()
            .oauth2ResourceServer().jwt();
        return http.build();
    }
}
```

## Node.js/Express Integration

### Installation

```bash
npm install keycloak-connect express-session
```

### Configuration

```javascript
const Keycloak = require('keycloak-connect');
const session = require('express-session');

const memoryStore = new session.MemoryStore();

app.use(session({
  secret: 'some-secret',
  resave: false,
  saveUninitialized: true,
  store: memoryStore
}));

const keycloak = new Keycloak({
  store: memoryStore
}, {
  realm: 'my-realm',
  'auth-server-url': 'https://keycloak.example.com/',
  'ssl-required': 'external',
  resource: 'node-app',
  credentials: {
    secret: 'client-secret'
  }
});

app.use(keycloak.middleware());

// Protected route
app.get('/protected', keycloak.protect(), (req, res) => {
  res.send('Protected resource');
});

// Role-based protection
app.get('/admin', keycloak.protect('admin'), (req, res) => {
  res.send('Admin resource');
});
```

## React/SPA Integration

### Installation

```bash
npm install keycloak-js
```

### Configuration

```javascript
import Keycloak from 'keycloak-js';

const keycloak = new Keycloak({
  url: 'https://keycloak.example.com/',
  realm: 'my-realm',
  clientId: 'react-app'
});

keycloak.init({
  onLoad: 'login-required',
  checkLoginIframe: false,
  pkceMethod: 'S256'
}).then(authenticated => {
  if (authenticated) {
    console.log('Access Token:', keycloak.token);
    console.log('User Info:', keycloak.tokenParsed);
    
    // Refresh token before expiration
    setInterval(() => {
      keycloak.updateToken(70).then((refreshed) => {
        if (refreshed) {
          console.log('Token refreshed');
        }
      }).catch(() => {
        console.log('Failed to refresh token');
      });
    }, 60000);
  } else {
    console.log('Not authenticated');
  }
}).catch(() => {
  console.log('Failed to initialize');
});
```

### API Calls with Token

```javascript
const fetchData = async () => {
  const response = await fetch('https://api.example.com/data', {
    headers: {
      'Authorization': `Bearer ${keycloak.token}`
    }
  });
  return response.json();
};
```

## Python/Flask Integration

### Installation

```bash
pip install flask-oidc
```

### Configuration

```python
from flask import Flask, g
from flask_oidc import OpenIDConnect

app = Flask(__name__)
app.config.update({
    'SECRET_KEY': 'your-secret-key',
    'OIDC_CLIENT_SECRETS': 'client_secrets.json',
    'OIDC_ID_TOKEN_COOKIE_SECURE': False,
    'OIDC_REQUIRE_VERIFIED_EMAIL': False,
    'OIDC_OPENID_REALM': 'my-realm'
})

oidc = OpenIDConnect(app)

@app.route('/protected')
@oidc.require_login
def protected():
    user_info = oidc.user_getinfo(['email', 'sub'])
    return f'Hello {user_info.get("email")}'

@app.route('/api/data')
@oidc.accept_token(require_token=True)
def api_data():
    return {'message': 'Protected API data'}
```

### client_secrets.json

```json
{
  "web": {
    "issuer": "https://keycloak.example.com/realms/my-realm",
    "auth_uri": "https://keycloak.example.com/realms/my-realm/protocol/openid-connect/auth",
    "client_id": "flask-app",
    "client_secret": "client-secret",
    "redirect_uris": [
      "http://localhost:5000/oidc_callback"
    ],
    "userinfo_uri": "https://keycloak.example.com/realms/my-realm/protocol/openid-connect/userinfo",
    "token_uri": "https://keycloak.example.com/realms/my-realm/protocol/openid-connect/token",
    "token_introspection_uri": "https://keycloak.example.com/realms/my-realm/protocol/openid-connect/token/introspect"
  }
}
```

## Docker Compose Example

```yaml
version: '3'

services:
  postgres:
    image: postgres:14
    environment:
      POSTGRES_DB: keycloak
      POSTGRES_USER: keycloak
      POSTGRES_PASSWORD: password
    volumes:
      - postgres_data:/var/lib/postgresql/data

  keycloak:
    image: quay.io/keycloak/keycloak:latest
    environment:
      KC_DB: postgres
      KC_DB_URL: jdbc:postgresql://postgres/keycloak
      KC_DB_USERNAME: keycloak
      KC_DB_PASSWORD: password
      KC_HOSTNAME: localhost
      KEYCLOAK_ADMIN: admin
      KEYCLOAK_ADMIN_PASSWORD: admin
    ports:
      - 8080:8080
    depends_on:
      - postgres
    command: start-dev

volumes:
  postgres_data:
```

## Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: keycloak
spec:
  replicas: 2
  selector:
    matchLabels:
      app: keycloak
  template:
    metadata:
      labels:
        app: keycloak
    spec:
      containers:
      - name: keycloak
        image: quay.io/keycloak/keycloak:latest
        args: ["start"]
        env:
        - name: KC_DB
          value: postgres
        - name: KC_DB_URL
          value: jdbc:postgresql://postgres/keycloak
        - name: KC_DB_USERNAME
          valueFrom:
            secretKeyRef:
              name: keycloak-db-secret
              key: username
        - name: KC_DB_PASSWORD
          valueFrom:
            secretKeyRef:
              name: keycloak-db-secret
              key: password
        - name: KC_HOSTNAME
          value: keycloak.example.com
        - name: KEYCLOAK_ADMIN
          valueFrom:
            secretKeyRef:
              name: keycloak-admin-secret
              key: username
        - name: KEYCLOAK_ADMIN_PASSWORD
          valueFrom:
            secretKeyRef:
              name: keycloak-admin-secret
              key: password
        ports:
        - containerPort: 8080
        readinessProbe:
          httpGet:
            path: /health/ready
            port: 8080
        livenessProbe:
          httpGet:
            path: /health/live
            port: 8080
---
apiVersion: v1
kind: Service
metadata:
  name: keycloak
spec:
  selector:
    app: keycloak
  ports:
  - port: 8080
    targetPort: 8080
  type: LoadBalancer
```
