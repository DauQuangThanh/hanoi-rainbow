# Investigation Commands and Tools

## Version Control Investigation

### Git Commands for Bug Investigation

**Find when a bug was introduced:**
```bash
# Binary search to find problematic commit
git bisect start
git bisect bad                    # Current version has bug
git bisect good v1.2.0           # Known good version
# Test each commit git shows
git bisect good/bad              # Mark each commit
git bisect reset                 # When done

# Find when specific line was changed
git blame path/to/file.js

# Show changes to specific file
git log -p path/to/file.js

# Find commits mentioning specific text
git log --all --grep="bug keyword"

# Show commits in date range
git log --since="2024-01-01" --until="2024-01-15"

# Show commits by author
git log --author="john@example.com"

# Show file history with renames
git log --follow path/to/file.js

# Compare branches
git diff main..feature-branch path/to/file.js

# Show what changed in a commit
git show abc123def

# Find who changed specific lines
git log -S "function_name" -p path/to/file.js
```

---

## Log Analysis

### Searching Logs

**Basic grep patterns:**
```bash
# Find errors in logs
grep -i "error" /var/log/app.log

# Case-insensitive search
grep -i "exception" app.log

# Show context (5 lines before and after)
grep -A 5 -B 5 "error" app.log

# Recursive search in directory
grep -r "NullPointerException" /var/log/

# Count occurrences
grep "error" app.log | wc -l

# Show only matching part
grep -o "Error: [^$]*" app.log

# Multiple patterns
grep -E "error|exception|fatal" app.log

# Exclude patterns
grep "error" app.log | grep -v "IgnoredError"

# Search with line numbers
grep -n "error" app.log
```

**Advanced log analysis:**
```bash
# Find errors in last hour
grep "$(date -d '1 hour ago' '+%Y-%m-%d %H')" app.log | grep "ERROR"

# Sort and count error types
grep "ERROR" app.log | sort | uniq -c | sort -rn

# Extract timestamps of errors
grep "ERROR" app.log | cut -d' ' -f1-2

# Find correlations between errors
grep -A 10 "DatabaseError" app.log | grep "ConnectionTimeout"

# Analyze error patterns over time
for hour in {00..23}; do
  echo "Hour $hour: $(grep "2024-01-15 $hour:" app.log | grep ERROR | wc -l)"
done
```

### Log Parsing with awk

```bash
# Extract specific fields
awk '{print $1, $4, $5}' access.log

# Filter by condition
awk '$9 >= 400 {print $0}' access.log

# Calculate statistics
awk '{sum+=$10; count++} END {print "Average:", sum/count}' access.log

# Group and count
awk '{print $1}' access.log | sort | uniq -c | sort -rn | head -10
```

### Monitoring Logs in Real-time

```bash
# Tail logs with follow
tail -f /var/log/app.log

# Tail multiple files
tail -f /var/log/app.log /var/log/error.log

# Tail with grep
tail -f app.log | grep --line-buffered "ERROR"

# Tail last 100 lines
tail -n 100 app.log

# Watch log file size
watch -n 5 'ls -lh /var/log/app.log'
```

---

## Database Investigation

### PostgreSQL

```sql
-- Find slow queries
SELECT query, calls, total_time, mean_time
FROM pg_stat_statements
ORDER BY mean_time DESC
LIMIT 20;

-- Check table sizes
SELECT schemaname, tablename, 
       pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC
LIMIT 10;

-- Check index usage
SELECT schemaname, tablename, indexname, idx_scan
FROM pg_stat_user_indexes
WHERE idx_scan = 0
ORDER BY pg_relation_size(indexrelid) DESC;

-- Find missing indexes
SELECT schemaname, tablename, seq_scan, seq_tup_read,
       idx_scan, seq_tup_read / seq_scan AS avg_tuples
FROM pg_stat_user_tables
WHERE seq_scan > 0
ORDER BY seq_tup_read DESC
LIMIT 10;

-- Check database connections
SELECT datname, usename, application_name, client_addr, state, query
FROM pg_stat_activity
WHERE state != 'idle';

-- Find blocking queries
SELECT blocked_locks.pid AS blocked_pid,
       blocking_locks.pid AS blocking_pid,
       blocked_activity.query AS blocked_statement,
       blocking_activity.query AS blocking_statement
FROM pg_catalog.pg_locks blocked_locks
JOIN pg_catalog.pg_stat_activity blocked_activity ON blocked_activity.pid = blocked_locks.pid
JOIN pg_catalog.pg_locks blocking_locks ON blocking_locks.locktype = blocked_locks.locktype
JOIN pg_catalog.pg_stat_activity blocking_activity ON blocking_activity.pid = blocking_locks.pid
WHERE NOT blocked_locks.granted;

-- Check for deadlocks
SELECT * FROM pg_stat_database WHERE datname = 'your_db';

-- Analyze table statistics
ANALYZE tablename;

-- Check vacuum status
SELECT schemaname, tablename, last_vacuum, last_autovacuum
FROM pg_stat_user_tables;
```

### MySQL

```sql
-- Show slow queries
SELECT * FROM mysql.slow_log ORDER BY query_time DESC LIMIT 20;

-- Current queries
SHOW FULL PROCESSLIST;

-- Table sizes
SELECT table_schema, table_name,
       ROUND((data_length + index_length) / 1024 / 1024, 2) AS size_mb
FROM information_schema.tables
ORDER BY (data_length + index_length) DESC
LIMIT 10;

-- Index usage
SELECT * FROM sys.schema_unused_indexes;

-- Deadlocks
SHOW ENGINE INNODB STATUS;

-- Connection count
SHOW STATUS LIKE 'Threads_connected';
```

### MongoDB

```javascript
// Find slow queries
db.system.profile.find({millis: {$gt: 100}}).sort({millis: -1}).limit(10)

// Collection stats
db.collection.stats()

// Index usage
db.collection.aggregate([{$indexStats: {}}])

// Current operations
db.currentOp()

// Kill long-running operation
db.killOp(operationId)

// Database statistics
db.stats()
```

---

## System Resource Monitoring

### Linux/macOS

**Process monitoring:**
```bash
# Top processes by CPU
top
# Interactive mode: Press '1' for per-CPU, 'M' for memory sort

# Process list with details
ps aux | sort -rk 3 | head -10  # By CPU
ps aux | sort -rk 4 | head -10  # By memory

# Monitor specific process
watch -n 1 'ps aux | grep processname'

# Process tree
pstree -p

# Detailed process info
lsof -p <PID>  # Open files
strace -p <PID>  # System calls
```

**Memory analysis:**
```bash
# Memory usage
free -h

# Detailed memory info
cat /proc/meminfo

# Memory by process
ps aux --sort=-%mem | head

# Check for memory leaks
valgrind --leak-check=full ./program

# Memory map of process
pmap <PID>
```

**Disk analysis:**
```bash
# Disk usage
df -h

# Directory sizes
du -sh */ | sort -rh | head -10

# Find large files
find /path -type f -size +100M -exec ls -lh {} \;

# Disk I/O
iostat -x 1

# Who is using disk
iotop
```

**Network analysis:**
```bash
# Network connections
netstat -tulpn  # Listening ports
netstat -an | grep ESTABLISHED  # Active connections

# Network traffic
tcpdump -i eth0 port 80
tcpdump -i eth0 -w capture.pcap

# Bandwidth usage
iftop -i eth0

# DNS lookup
nslookup domain.com
dig domain.com

# Trace route
traceroute domain.com

# Check port connectivity
telnet hostname port
nc -zv hostname port

# HTTP request debugging
curl -v https://api.example.com
```

---

## Application-Specific Tools

### Node.js

**Debugging:**
```bash
# Run with debugger
node --inspect app.js
node --inspect-brk app.js  # Break on first line

# Memory profiling
node --inspect --expose-gc app.js

# CPU profiling
node --prof app.js
node --prof-process isolate-0x*.log

# Heap snapshot
node --heapsnapshot-signal=SIGUSR2 app.js
# Then: kill -SIGUSR2 <PID>
```

**Package debugging:**
```bash
# List installed packages
npm ls

# Check for vulnerabilities
npm audit

# Check for updates
npm outdated

# View package info
npm info package-name

# Check peer dependencies
npm ls package-name
```

### Python

**Debugging:**
```bash
# Run with debugger
python -m pdb script.py

# Profile execution
python -m cProfile -s cumtime script.py

# Memory profiling
python -m memory_profiler script.py

# Line profiling
kernprof -l script.py
python -m line_profiler script.py.lprof
```

**Package debugging:**
```bash
# List installed packages
pip list

# Show package details
pip show package-name

# Check for updates
pip list --outdated

# Verify dependencies
pip check
```

### Java

**Debugging:**
```bash
# Heap dump
jmap -dump:format=b,file=heap.bin <PID>

# Thread dump
jstack <PID> > threads.txt

# GC monitoring
jstat -gc <PID> 1000

# Java process info
jps -lvm

# Flight recorder
jcmd <PID> JFR.start duration=60s filename=recording.jfr
```

### Docker

**Container debugging:**
```bash
# Container logs
docker logs container_name
docker logs -f --tail 100 container_name

# Execute command in container
docker exec -it container_name bash
docker exec -it container_name sh

# Container stats
docker stats container_name

# Inspect container
docker inspect container_name

# Check container processes
docker top container_name

# Copy files from container
docker cp container_name:/path/to/file ./local/path

# Container events
docker events --filter container=container_name
```

### Kubernetes

**Pod debugging:**
```bash
# Get pod logs
kubectl logs pod-name
kubectl logs -f pod-name  # Follow
kubectl logs --previous pod-name  # Previous instance

# Execute command in pod
kubectl exec -it pod-name -- bash

# Describe pod (check events)
kubectl describe pod pod-name

# Get pod details
kubectl get pod pod-name -o yaml

# Port forward for debugging
kubectl port-forward pod-name 8080:80

# Debug with ephemeral container
kubectl debug pod-name -it --image=busybox

# Check pod resources
kubectl top pods

# Get events
kubectl get events --sort-by=.metadata.creationTimestamp
```

---

## Performance Analysis Tools

### Web Performance

**Browser DevTools:**
- Network tab: Request timing, headers, payload
- Performance tab: Timeline, CPU/memory usage
- Lighthouse: Performance audit

**Command-line tools:**
```bash
# Load testing
ab -n 1000 -c 10 https://example.com/api

# Advanced load testing
wrk -t4 -c100 -d30s https://example.com

# HTTP benchmarking
siege -c 50 -t 1M https://example.com

# DNS timing
time nslookup example.com

# SSL handshake timing
openssl s_time -connect example.com:443
```

### APM Tools

Common Application Performance Monitoring tools:
- **New Relic**: Transaction tracing, error tracking
- **Datadog**: Metrics, logs, traces correlation
- **AppDynamics**: Business transaction monitoring
- **Dynatrace**: AI-powered root cause analysis
- **Elastic APM**: Open-source APM with ELK stack

### Profilers

- **Node.js**: node --prof, clinic.js, 0x
- **Python**: cProfile, py-spy, austin
- **Java**: JProfiler, YourKit, VisualVM
- **Ruby**: ruby-prof, stackprof
- **.NET**: dotTrace, PerfView

---

## Automation Scripts

### Bug Reproduction Script Template

```bash
#!/bin/bash
# Bug reproduction script for BUG-XXXX

set -e  # Exit on error

echo "Setting up environment..."
# Setup steps
export ENV_VAR=value
cd /path/to/project

echo "Starting services..."
# Start required services
docker-compose up -d

echo "Waiting for services..."
sleep 5

echo "Running reproduction steps..."
# Step 1
curl -X POST https://api.example.com/endpoint -d '{"data":"value"}'

# Step 2
./run_script.sh

# Step 3
# Check for bug
if grep -q "ERROR" /var/log/app.log; then
    echo "BUG REPRODUCED: Error found in logs"
    grep "ERROR" /var/log/app.log
    exit 1
else
    echo "Bug not reproduced"
    exit 0
fi
```

### Log Collection Script

```bash
#!/bin/bash
# Collect logs and diagnostic info for bug report

BUG_ID="BUG-XXXX"
OUTPUT_DIR="bug_report_${BUG_ID}_$(date +%Y%m%d_%H%M%S)"

mkdir -p "$OUTPUT_DIR"

echo "Collecting logs for $BUG_ID..."

# Application logs
cp /var/log/app/*.log "$OUTPUT_DIR/"

# System info
uname -a > "$OUTPUT_DIR/system_info.txt"
df -h > "$OUTPUT_DIR/disk_usage.txt"
free -h > "$OUTPUT_DIR/memory_usage.txt"

# Process info
ps aux > "$OUTPUT_DIR/processes.txt"

# Network info
netstat -tulpn > "$OUTPUT_DIR/network.txt"

# Docker info (if applicable)
if command -v docker &> /dev/null; then
    docker ps > "$OUTPUT_DIR/docker_containers.txt"
    docker logs app_container > "$OUTPUT_DIR/docker_app.log"
fi

# Package to send
tar -czf "${OUTPUT_DIR}.tar.gz" "$OUTPUT_DIR"
echo "Logs collected in ${OUTPUT_DIR}.tar.gz"
```

---

## Cheat Sheet

### Quick Diagnosis Commands

```bash
# Is the service running?
systemctl status service_name

# What's using all the CPU?
top -bn1 | head -20

# What's using all the memory?
ps aux --sort=-%mem | head -10

# What's using all the disk?
du -sh /* | sort -rh | head -10

# What's the network doing?
netstat -tunlp

# What's in the logs?
tail -100 /var/log/app.log | grep -i error

# What changed recently?
git log --since="1 day ago" --oneline

# What's the error rate?
grep ERROR /var/log/app.log | wc -l
```
