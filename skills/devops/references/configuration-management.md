# Configuration Management


### Ansible Playbook Example

```yaml
# playbook.yml
---
- name: Configure web servers
  hosts: webservers
  become: yes
  vars:
    app_version: "1.0.0"
    app_port: 8080
  
  tasks:
    - name: Update package cache
      apt:
        update_cache: yes
        cache_valid_time: 3600
    
    - name: Install required packages
      apt:
        name:
          - nginx
          - nodejs
          - npm
        state: present
    
    - name: Create app user
      user:
        name: appuser
        shell: /bin/bash
        create_home: yes
        state: present
    
    - name: Create app directory
      file:
        path: /opt/app
        state: directory
        owner: appuser
        group: appuser
        mode: '0755'
    
    - name: Deploy application
      copy:
        src: "dist/app-{{ app_version }}.tar.gz"
        dest: "/opt/app/app.tar.gz"
        owner: appuser
        group: appuser
      notify: Restart application
    
    - name: Extract application
      unarchive:
        src: "/opt/app/app.tar.gz"
        dest: /opt/app
        remote_src: yes
        owner: appuser
        group: appuser
    
    - name: Configure nginx
      template:
        src: templates/nginx.conf.j2
        dest: /etc/nginx/sites-available/app
        mode: '0644'
      notify: Reload nginx
    
    - name: Enable nginx site
      file:
        src: /etc/nginx/sites-available/app
        dest: /etc/nginx/sites-enabled/app
        state: link
      notify: Reload nginx
    
    - name: Configure systemd service
      template:
        src: templates/app.service.j2
        dest: /etc/systemd/system/app.service
        mode: '0644'
      notify: Restart application
  
  handlers:
    - name: Reload nginx
      systemd:
        name: nginx
        state: reloaded
    
    - name: Restart application
      systemd:
        name: app
        state: restarted
        daemon_reload: yes
```

---
