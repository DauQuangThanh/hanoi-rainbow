# System Configuration Migration


### Configuration Backup and Restore

```bash
# Linux System Configuration Backup

# 1. System files
tar czf system-config.tar.gz \
  /etc/ \
  /var/spool/cron/ \
  /home/*/.ssh/ \
  /root/.ssh/

# 2. Package list
rpm -qa > installed-packages.txt    # RHEL/CentOS
dpkg -l > installed-packages.txt    # Debian/Ubuntu

# 3. Services
systemctl list-unit-files --state=enabled > enabled-services.txt

# 4. Firewall rules
iptables-save > iptables-rules
# or for firewalld
firewall-cmd --list-all-zones > firewall-config.txt

# 5. SELinux/AppArmor
getenforce > selinux-mode.txt
sestatus > selinux-status.txt

# 6. Network configuration
ip addr > ip-config.txt
ip route > routing-table.txt

# Restore on target system (carefully review each file)
```

### Automation Scripts

```bash
# Create migration script
#!/bin/bash
# system-migrate.sh

set -e  # Exit on error

SOURCE_IP="source-server-ip"
TARGET_IP="target-server-ip"

echo "=== System Migration ==="

# 1. Backup source system
echo "Backing up source system..."
ssh root@$SOURCE_IP 'tar czf /tmp/config-backup.tar.gz /etc/'
scp root@$SOURCE_IP:/tmp/config-backup.tar.gz ./

# 2. Copy to target
echo "Copying to target..."
scp config-backup.tar.gz root@$TARGET_IP:/tmp/

# 3. Install packages on target
echo "Installing packages..."
ssh root@$TARGET_IP 'yum install -y httpd mariadb-server'

# 4. Restore configurations
echo "Restoring configurations..."
ssh root@$TARGET_IP 'cd /tmp && tar xzf config-backup.tar.gz'

# 5. Start services
echo "Starting services..."
ssh root@$TARGET_IP 'systemctl start httpd mariadb'

echo "Migration complete!"
```
