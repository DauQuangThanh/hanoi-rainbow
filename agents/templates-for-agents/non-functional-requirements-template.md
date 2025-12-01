# Non-Functional Requirements (NFR) Template

## Performance

- **API Response Time**: p95 < 500ms, p99 < 1s
- **Page Load Time**: FCP < 1.5s, TTI < 3s
- **Throughput**: 10,000 requests/sec at peak
- **Database Queries**: p95 < 100ms
- **Batch Processing**: Complete within [X] hours

## Scalability

- **Concurrent Users**: 100,000
- **Data Volume**: 10TB with sub-second queries
- **Growth Rate**: [X%] per year
- **Geographic Distribution**: [Regions]
- **Horizontal Scaling**: Stateless services, auto-scaling policies

## Availability

- **Uptime SLA**: 99.9% (8.76 hrs downtime/year)
- **Fault Tolerance**: No single point of failure
- **Disaster Recovery**:
  - RPO (Recovery Point Objective): 1 hour
  - RTO (Recovery Time Objective): 4 hours
- **Maintenance Windows**: [Schedule]

## Security

- **Authentication**: OAuth2/OIDC, MFA for admin access
- **Authorization**: RBAC, least privilege principle
- **Encryption**:
  - In Transit: TLS 1.3
  - At Rest: AES-256
- **Compliance**: GDPR, HIPAA, PCI-DSS Level 1, SOC2
- **Vulnerability Management**: Monthly scans, 90-day patching cycle
- **Audit Logging**: All access to sensitive data logged

## Maintainability

- **Code Quality**: SonarQube score > 8.0
- **Test Coverage**: > 80%
- **Documentation**: API docs, architecture diagrams, runbooks
- **Technical Debt**: < [X]% of development capacity
- **Deployment Frequency**: [Daily/Weekly]

## Usability

- **Accessibility**: WCAG 2.1 AA compliance
- **Browser Support**: Latest 2 versions of Chrome, Firefox, Safari, Edge
- **Mobile Support**: iOS 15+, Android 12+
- **Internationalization**: Support for [Languages]
- **Response Time (User-facing)**: < 2 seconds for 95% of actions

## Reliability

- **Mean Time Between Failures (MTBF)**: > 720 hours
- **Mean Time to Recovery (MTTR)**: < 30 minutes
- **Error Rate**: < 0.1% of all transactions
- **Data Durability**: 99.999999999% (11 nines)

## Observability

- **Logging**: Centralized logging (ELK Stack, Splunk)
- **Metrics**: Real-time dashboards (Prometheus, Grafana)
- **Tracing**: Distributed tracing (Jaeger, Zipkin)
- **Alerting**: PagerDuty/Opsgenie integration
- **SLI/SLO Monitoring**: [Service-specific metrics]

## Capacity

- **Storage**: [Initial/Growth projections]
- **Network Bandwidth**: [Requirements]
- **Compute Resources**: [CPU/Memory requirements]
- **Cost Budget**: [Monthly/Annual limits]
