# Examples


### Example 1: E-Commerce Platform

**Requirements**: Build a scalable e-commerce platform handling 100K daily active users

**Architecture Design**:
- **Style**: Microservices with event-driven communication
- **Services**: Product Catalog, User Service, Order Management, Payment Processing, Inventory
- **Technology**: Node.js/Express, React, PostgreSQL, MongoDB, Redis, Kafka
- **Infrastructure**: AWS with EKS, RDS, ElastiCache, S3, CloudFront
- **Key Patterns**: API Gateway, CQRS for orders, Event Sourcing for inventory

### Example 2: Real-Time Analytics Dashboard

**Requirements**: Process and visualize millions of events per minute

**Architecture Design**:
- **Style**: Event-driven, Lambda architecture
- **Components**: Event ingestion, stream processing, batch processing, query layer
- **Technology**: Kafka, Apache Flink, Elasticsearch, React, WebSocket
- **Infrastructure**: AWS with MSK, EMR, OpenSearch, Lambda
- **Key Patterns**: Event streaming, CQRS, real-time aggregation
