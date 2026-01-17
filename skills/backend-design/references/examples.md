# Examples


### Example 1: E-Commerce Backend

**Architecture**: Microservices with event-driven communication

**Services**:
- User Service (authentication, profiles)
- Product Service (catalog, search)
- Order Service (cart, checkout, orders)
- Payment Service (Stripe integration)
- Inventory Service (stock management)
- Notification Service (emails, SMS)

**Technology Stack**:
- Backend: Node.js/Express, Python/FastAPI
- Database: PostgreSQL (relational), MongoDB (product catalog), Redis (cache)
- Message Queue: RabbitMQ
- Search: Elasticsearch
- Infrastructure: Docker, Kubernetes, AWS

**Key Patterns**:
- API Gateway (Kong)
- Event-driven architecture
- CQRS for orders
- Saga pattern for distributed transactions

### Example 2: Real-Time Chat Application

**Architecture**: Monolithic with WebSocket support

**Features**:
- User authentication
- One-on-one messaging
- Group chats
- Message history
- Online presence
- Typing indicators
- File sharing

**Technology Stack**:
- Backend: Node.js with Socket.IO
- Database: PostgreSQL (users, messages), Redis (presence, cache)
- Storage: AWS S3 (files)
- Infrastructure: Docker, AWS ECS

**Key Patterns**:
- WebSocket for real-time communication
- Redis pub/sub for message distribution
- Message queue for async processing (notifications)
