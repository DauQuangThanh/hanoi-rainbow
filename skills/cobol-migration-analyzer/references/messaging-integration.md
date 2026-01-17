# Messaging Integration Patterns

Guide for migrating mainframe messaging patterns (MQ, CICS transient data) to modern Java messaging.

## Overview

Mainframe systems use IBM MQ, CICS queues, and transient data for asynchronous communication. This guide shows how to migrate these patterns to modern Java messaging frameworks.

## Common Mainframe Patterns

### 1. IBM MQ Messages

**COBOL Example**:
```cobol
EXEC CICS WRITEQ TS
    QUEUE('ORDERQ')
    FROM(ORDER-MESSAGE)
    LENGTH(ORDER-LENGTH)
END-EXEC.

EXEC CICS READQ TS
    QUEUE('ORDERQ')
    INTO(ORDER-MESSAGE)
    LENGTH(ORDER-LENGTH)
END-EXEC.
```

### 2. CICS Transient Data

```cobol
EXEC CICS WRITEQ TD
    QUEUE('LOGG')
    FROM(LOG-MESSAGE)
    LENGTH(LOG-LENGTH)
END-EXEC.
```

## Java Messaging Patterns

### Pattern 1: Spring JMS with ActiveMQ

**Configuration**:
```java
@Configuration
@EnableJms
public class JmsConfig {
    
    @Bean
    public ConnectionFactory connectionFactory() {
        return new ActiveMQConnectionFactory("tcp://localhost:61616");
    }
    
    @Bean
    public JmsTemplate jmsTemplate(ConnectionFactory connectionFactory) {
        JmsTemplate template = new JmsTemplate(connectionFactory);
        template.setDefaultDestinationName("order.queue");
        return template;
    }
}
```

**Sender** (equivalent to WRITEQ):
```java
@Service
public class OrderMessageSender {
    
    @Autowired
    private JmsTemplate jmsTemplate;
    
    public void sendOrder(Order order) {
        jmsTemplate.convertAndSend("order.queue", order, message -> {
            message.setStringProperty("orderType", order.getType());
            message.setStringProperty("priority", order.getPriority());
            return message;
        });
    }
}
```

**Receiver** (equivalent to READQ):
```java
@Service
public class OrderMessageReceiver {
    
    @JmsListener(destination = "order.queue")
    public void receiveOrder(Order order) {
        log.info("Received order: {}", order.getId());
        processOrder(order);
    }
    
    // Manual receive (blocking)
    public Order receiveOrderManually() {
        return (Order) jmsTemplate.receiveAndConvert("order.queue");
    }
}
```

### Pattern 2: Spring AMQP with RabbitMQ

**Configuration**:
```java
@Configuration
public class RabbitConfig {
    
    @Bean
    public Queue orderQueue() {
        return new Queue("order.queue", true); // durable
    }
    
    @Bean
    public Exchange orderExchange() {
        return new TopicExchange("order.exchange");
    }
    
    @Bean
    public Binding binding(Queue queue, Exchange exchange) {
        return BindingBuilder
            .bind(queue)
            .to(exchange)
            .with("order.#")
            .noargs();
    }
    
    @Bean
    public RabbitTemplate rabbitTemplate(ConnectionFactory connectionFactory) {
        RabbitTemplate template = new RabbitTemplate(connectionFactory);
        template.setMessageConverter(new Jackson2JsonMessageConverter());
        return template;
    }
}
```

**Sender**:
```java
@Service
public class RabbitOrderSender {
    
    @Autowired
    private RabbitTemplate rabbitTemplate;
    
    public void sendOrder(Order order) {
        rabbitTemplate.convertAndSend(
            "order.exchange",
            "order.new",
            order
        );
    }
}
```

**Receiver**:
```java
@Service
public class RabbitOrderReceiver {
    
    @RabbitListener(queues = "order.queue")
    public void handleOrder(Order order) {
        processOrder(order);
    }
}
```

### Pattern 3: Spring Kafka

**Configuration**:
```java
@Configuration
public class KafkaConfig {
    
    @Bean
    public ProducerFactory<String, Order> producerFactory() {
        Map<String, Object> config = new HashMap<>();
        config.put(ProducerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        config.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
        config.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, JsonSerializer.class);
        return new DefaultKafkaProducerFactory<>(config);
    }
    
    @Bean
    public KafkaTemplate<String, Order> kafkaTemplate() {
        return new KafkaTemplate<>(producerFactory());
    }
    
    @Bean
    public ConsumerFactory<String, Order> consumerFactory() {
        Map<String, Object> config = new HashMap<>();
        config.put(ConsumerConfig.BOOTSTRAP_SERVERS_CONFIG, "localhost:9092");
        config.put(ConsumerConfig.GROUP_ID_CONFIG, "order-processor");
        config.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
        config.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, JsonDeserializer.class);
        config.put(JsonDeserializer.TRUSTED_PACKAGES, "*");
        return new DefaultKafkaConsumerFactory<>(config);
    }
}
```

**Producer**:
```java
@Service
public class KafkaOrderProducer {
    
    @Autowired
    private KafkaTemplate<String, Order> kafkaTemplate;
    
    public void sendOrder(Order order) {
        kafkaTemplate.send("order-topic", order.getId(), order)
            .addCallback(
                success -> log.info("Order sent: {}", order.getId()),
                failure -> log.error("Failed to send order", failure)
            );
    }
}
```

**Consumer**:
```java
@Service
public class KafkaOrderConsumer {
    
    @KafkaListener(topics = "order-topic", groupId = "order-processor")
    public void consume(Order order) {
        log.info("Consumed order: {}", order.getId());
        processOrder(order);
    }
}
```

## Migration Patterns

### Pattern 1: Request-Reply

**COBOL/MQ**:
```cobol
* Send request
MOVE 'TEMP-REPLY-Q' TO MQMD-REPLYTOQ
CALL 'MQPUT' USING ...

* Wait for reply
CALL 'MQGET' USING MQMD MQGMO REPLYQ-NAME ...
```

**Java/JMS**:
```java
@Service
public class RequestReplyService {
    
    @Autowired
    private JmsTemplate jmsTemplate;
    
    public Response sendRequest(Request request) {
        return (Response) jmsTemplate.convertSendAndReceive(
            "request.queue",
            request,
            message -> {
                message.setJMSReplyTo(
                    new ActiveMQQueue("reply.queue")
                );
                return message;
            }
        );
    }
}
```

### Pattern 2: Dead Letter Queue

**Configuration**:
```java
@Bean
public ReplyingKafkaTemplate<String, Order, OrderResult> replyingKafkaTemplate(
        ProducerFactory<String, Order> pf,
        KafkaMessageListenerContainer<String, OrderResult> container) {
    return new ReplyingKafkaTemplate<>(pf, container);
}

@Bean
public DeadLetterPublishingRecoverer deadLetterPublishingRecoverer(
        KafkaTemplate<String, Order> template) {
    return new DeadLetterPublishingRecoverer(template);
}
```

**Usage**:
```java
@KafkaListener(topics = "order-topic")
public void processOrder(Order order) {
    try {
        validateAndProcess(order);
    } catch (ValidationException e) {
        // Will be sent to DLQ automatically
        throw new RuntimeException("Invalid order", e);
    }
}
```

### Pattern 3: Message Retry

**Configuration**:
```java
@Configuration
public class RetryConfig {
    
    @Bean
    public ErrorHandler errorHandler(KafkaTemplate<String, Order> template) {
        // Retry 3 times with exponential backoff
        DefaultErrorHandler handler = new DefaultErrorHandler(
            new DeadLetterPublishingRecoverer(template),
            new FixedBackOff(1000L, 3L)
        );
        
        handler.addNotRetryableExceptions(ValidationException.class);
        return handler;
    }
}
```

## Message Format Conversion

### COBOL Fixed-Length to JSON

**COBOL Message**:
```cobol
01  ORDER-MESSAGE.
    05  ORDER-ID        PIC X(10).
    05  CUSTOMER-ID     PIC X(10).
    05  AMOUNT          PIC 9(7)V99 COMP-3.
    05  ORDER-DATE      PIC 9(8).
```

**Java DTO**:
```java
@Data
public class OrderMessage {
    private String orderId;
    private String customerId;
    private BigDecimal amount;
    private LocalDate orderDate;
    
    // Converter from COBOL format
    public static OrderMessage fromCobolBytes(byte[] bytes) {
        // Parse fixed-length format
        String orderId = new String(bytes, 0, 10).trim();
        String customerId = new String(bytes, 10, 10).trim();
        BigDecimal amount = CobolConverter.fromPackedDecimal(bytes, 20, 5);
        LocalDate orderDate = CobolConverter.fromCobolDate(bytes, 25, 8);
        
        return new OrderMessage(orderId, customerId, amount, orderDate);
    }
}
```

## Transaction Management

### XA Transactions with Messaging

```java
@Configuration
@EnableTransactionManagement
public class XAMessagingConfig {
    
    @Bean
    public JmsTransactionManager transactionManager(
            ConnectionFactory connectionFactory) {
        return new JmsTransactionManager(connectionFactory);
    }
}

@Service
public class TransactionalMessagingService {
    
    @Transactional
    public void processWithTransaction(Order order) {
        // Save to database
        orderRepository.save(order);
        
        // Send message (part of same transaction)
        jmsTemplate.convertAndSend("order.processed", order);
        
        // Both commit or rollback together
    }
}
```

## Monitoring and Error Handling

### Message Metrics

```java
@Component
public class MessagingMetrics {
    
    private final Counter messagesReceived;
    private final Counter messagesFailed;
    private final Timer processingTime;
    
    public MessagingMetrics(MeterRegistry registry) {
        this.messagesReceived = registry.counter("messages.received");
        this.messagesFailed = registry.counter("messages.failed");
        this.processingTime = registry.timer("messages.processing.time");
    }
    
    @Around("@annotation(JmsListener)")
    public Object monitorMessageProcessing(ProceedingJoinPoint pjp) throws Throwable {
        messagesReceived.increment();
        Timer.Sample sample = Timer.start();
        
        try {
            Object result = pjp.proceed();
            sample.stop(processingTime);
            return result;
        } catch (Exception e) {
            messagesFailed.increment();
            throw e;
        }
    }
}
```

### Error Handling

```java
@Service
public class MessagingErrorHandler {
    
    @JmsListener(destination = "order.queue")
    public void handleOrder(Order order, @Header("JMSRedelivered") boolean redelivered) {
        try {
            processOrder(order);
        } catch (Exception e) {
            if (redelivered) {
                // Already retried, send to DLQ
                sendToDeadLetterQueue(order, e);
            } else {
                // First failure, throw to trigger redelivery
                throw new RuntimeException("Processing failed", e);
            }
        }
    }
}
```

## Migration Checklist

- [ ] Identify all MQ queue usages
- [ ] Map CICS transient data queues
- [ ] Choose appropriate messaging technology (JMS/AMQP/Kafka)
- [ ] Design message formats (JSON vs. binary)
- [ ] Implement message converters
- [ ] Configure error handling and DLQ
- [ ] Set up monitoring and alerting
- [ ] Test message ordering guarantees
- [ ] Verify transaction behavior
- [ ] Performance test under load
- [ ] Document migration patterns
