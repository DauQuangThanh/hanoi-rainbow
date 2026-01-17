# Performance Patterns for Migrated Code

This document provides guidance on optimizing Java code migrated from COBOL, addressing common performance challenges.

## Overview

COBOL programs often process large volumes of data efficiently using mainframe-optimized patterns. When migrating to Java, it's important to maintain or improve performance while adapting to modern architectures.

## Common Performance Patterns

### 1. Batch Processing with Streams

**COBOL Pattern**: Sequential file processing
**Java Solution**: Use Java Streams for efficient batch processing

```java
// Instead of loading all records into memory
List<Record> records = loadAllRecords(); // DON'T

// Use streaming
try (Stream<String> lines = Files.lines(path)) {
    lines.map(this::parseRecord)
         .filter(this::isValid)
         .forEach(this::processRecord);
}
```

### 2. Database Batch Operations

**COBOL Pattern**: Cursor processing with commits every N records
**Java Solution**: JDBC batch updates

```java
try (PreparedStatement pstmt = conn.prepareStatement(sql)) {
    for (Record record : records) {
        pstmt.setString(1, record.getId());
        pstmt.setString(2, record.getName());
        pstmt.addBatch();
        
        if (++count % 1000 == 0) {
            pstmt.executeBatch();
            conn.commit();
        }
    }
    pstmt.executeBatch();
    conn.commit();
}
```

### 3. Memory Management

**Challenge**: COBOL's fixed memory model vs. Java's heap
**Solution**: Process in chunks, use pagination

```java
public void processLargeFile(Path file) {
    int batchSize = 1000;
    List<Record> batch = new ArrayList<>(batchSize);
    
    try (Stream<String> lines = Files.lines(file)) {
        lines.forEach(line -> {
            batch.add(parseRecord(line));
            if (batch.size() >= batchSize) {
                processBatch(new ArrayList<>(batch));
                batch.clear();
            }
        });
        if (!batch.isEmpty()) {
            processBatch(batch);
        }
    }
}
```

### 4. Parallel Processing

**COBOL Pattern**: Single-threaded sequential processing
**Java Solution**: Parallel streams for CPU-bound operations

```java
// For independent record processing
records.parallelStream()
       .map(this::transform)
       .forEach(this::save);

// Control parallelism
ForkJoinPool customPool = new ForkJoinPool(4);
customPool.submit(() ->
    records.parallelStream()
           .forEach(this::process)
).get();
```

### 5. String Operations

**COBOL Pattern**: Fixed-length strings with spaces
**Java Solution**: Efficient string handling

```java
// Avoid creating many temporary strings
StringBuilder sb = new StringBuilder();
for (Record r : records) {
    sb.append(r.getId()).append('|')
      .append(r.getName()).append('\n');
}
String output = sb.toString();

// For fixed-length COBOL fields
String padded = String.format("%-20s", value); // Left-padded
String numeric = String.format("%010d", number); // Zero-padded
```

### 6. Caching Lookup Tables

**COBOL Pattern**: In-memory tables loaded at startup
**Java Solution**: Use efficient caching

```java
// Simple cache
private final Map<String, RateEntry> rateCache = new HashMap<>();

// Caffeine cache with eviction
LoadingCache<String, RateEntry> cache = Caffeine.newBuilder()
    .maximumSize(10_000)
    .expireAfterWrite(1, TimeUnit.HOURS)
    .build(key -> loadRate(key));
```

### 7. File I/O Optimization

**COBOL Pattern**: Blocked records for efficient I/O
**Java Solution**: Buffered I/O with appropriate buffer sizes

```java
// Reading
try (BufferedReader reader = new BufferedReader(
        new FileReader(file), 8192 * 4)) { // 32KB buffer
    String line;
    while ((line = reader.readLine()) != null) {
        processLine(line);
    }
}

// Writing
try (BufferedWriter writer = new BufferedWriter(
        new FileWriter(file), 8192 * 4)) {
    for (Record r : records) {
        writer.write(r.toLine());
        writer.newLine();
    }
}
```

## Performance Testing

### Benchmark Template

```java
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
public class MigrationBenchmark {
    
    @Param({"1000", "10000", "100000"})
    private int recordCount;
    
    @Setup
    public void setup() {
        // Initialize test data
    }
    
    @Benchmark
    public void testProcessing() {
        // Your processing logic
    }
}
```

## Best Practices

1. **Profile Before Optimizing**: Use JProfiler, YourKit, or JFR
2. **Set Realistic Goals**: Match or exceed COBOL performance
3. **Test with Production Volumes**: Use representative data sizes
4. **Monitor JVM Metrics**: Heap, GC, thread pools
5. **Use Appropriate Data Structures**: ArrayList vs. LinkedList, HashMap vs. TreeMap
6. **Minimize Object Creation**: Reuse objects in tight loops
7. **Consider Parallel Processing**: But measure - not always faster
8. **Optimize Database Access**: Use connection pooling, prepared statements

## Common Pitfalls

❌ **DON'T**: Load entire files into memory
✅ **DO**: Stream and process incrementally

❌ **DON'T**: Use `+` for string concatenation in loops
✅ **DO**: Use `StringBuilder` or `StringJoiner`

❌ **DON'T**: Create new `SimpleDateFormat` in loops (not thread-safe)
✅ **DO**: Use `DateTimeFormatter` (thread-safe) or ThreadLocal

❌ **DON'T**: Ignore connection pooling
✅ **DO**: Use HikariCP or similar

❌ **DON'T**: Assume parallel is always faster
✅ **DO**: Benchmark and measure

## Monitoring Migration Performance

```java
// Add metrics
public class ProcessingService {
    private final Timer processingTimer;
    private final Counter recordCounter;
    
    public void processRecords(List<Record> records) {
        Timer.Sample sample = Timer.start();
        try {
            records.forEach(this::process);
            recordCounter.increment(records.size());
        } finally {
            sample.stop(processingTimer);
        }
    }
}
```
