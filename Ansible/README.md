# Ansible Playbooks for Kali Linux Configuration

This directory contains Ansible playbooks that configure your Kali Linux VM, regardless of whether you're using VirtualBox or QEMU/KVM.

> **See also:**
> - [Main Project README](../README.md)
> - [QEMU Setup Guide](../Vagrant/QEMU/README.md)

## Main Playbook: configure-kali.yml

The main playbook installs and configures a complete Kali Linux environment with development tools, security tools, and modern terminal utilities.

### Customizing the Playbook

You can easily customize `configure-kali.yml` by modifying these sections:

1. **Variables** - At the top of the playbook:
```yaml
vars:
    user_home: "/home/vagrant"
    docker_gpg_key_url: "https://download.docker.com/linux/debian/gpg"
    docker_repo: "deb [arch=amd64 signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian bookworm stable"
```

2. **Base Packages** - Modify the package list in the "Install base packages" task:
```yaml
- name: Install base packages
  apt:
    name:
      - eza
      - alacritty
      - git
      # Add or remove packages here
    state: present
```

3. **Zsh Plugins** - Add or remove plugins in the "Install zsh plugins" task:
```yaml
- name: Install zsh plugins
  loop:
    - { repo: 'https://github.com/zsh-users/zsh-autosuggestions', name: 'zsh-autosuggestions' }
    # Add more plugins here
```

4. **Emacs Configuration** - Modify Emacs build options in the "Configure Emacs" task:
```yaml
./configure --with-native-compilation --with-json --with-tree-sitter --with-sqlite3
# Add or remove compile options
```

5. **Dotfiles** - Change dotfile symlinks in the "Create symlinks" task:
```yaml
- name: Create symlinks
  loop:
    - { src: "path/to/source", dest: "path/to/destination" }
    # Add more symlinks here
```

### Adding New Tasks

To add new functionality:

1. Add a new task at the appropriate section:
```yaml
- name: My New Task
  apt:  # or any other module
    name: my-package
    state: present
```

2. For complex installations, use a block:
```yaml
- name: Install My Software
  block:
    - name: Download
      get_url:
        url: https://example.com/software
        dest: /tmp/software
    
    - name: Install
      shell: |
        cd /tmp/software
        ./install.sh
```

### Testing Changes

1. Test individual tasks:
```bash
ansible-playbook -i .vagrant/provisioners/ansible/inventory/vagrant_ansible_inventory configure-kali.yml --start-at-task="task name"
```

2. Test specific tags (if added):
```bash
ansible-playbook -i .vagrant/provisioners/ansible/inventory/vagrant_ansible_inventory configure-kali.yml --tags "tag_name"
```

## VirtioFS Playbook: configure-shared-folders.yml

This playbook configures shared folders for QEMU/KVM using VirtioFS. Customize the mount points in the "Create mount points" task:

```yaml
- name: Create mount points
  loop:
    - /home/vagrant/my-folder
    # Add more mount points here
```

## Best Practices

1. Always use variables for paths and URLs
2. Add meaningful comments for complex tasks
3. Use `become: yes` only when necessary
4. Include proper error handling with `register` and `when`
5. Use `async` for long-running tasks
6. Add `creates` arguments to prevent unnecessary reruns

## Troubleshooting

1. **Task Failures**:
   - Check the error message in the output
   - Use `ignore_errors: yes` for debugging
   - Add `debug` tasks to inspect variables

2. **Idempotency Issues**:
   - Use `creates`, `when`, or `changed_when`
   - Check file/directory permissions
   - Verify package names and versions

3. **Performance Issues**:
   - Use `async` for long tasks
   - Consider using `cache_valid_time` for apt
   - Add proper `when` conditions to skip unnecessary tasks
