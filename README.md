# Kali Linux Configuration Automation

This repository provides automated configuration scripts for setting up a fully-featured Kali Linux environment using Vagrant and Ansible. It supports both VirtualBox and QEMU/KVM providers, allowing you to choose the virtualization solution that best fits your needs.

## Documentation

- [Ansible Configuration Guide](Ansible/README.md) - Details on customizing the Ansible playbooks
- [QEMU/KVM Setup Guide](Vagrant/QEMU/README.md) - Instructions for QEMU/KVM setup
- [Alacritty Configuration](Alacritty/README.org) - Terminal emulator configuration
- [Doom Emacs Configuration](Doom/README.org) - Doom Emacs setup and customization
- [Tmux Configuration](Tmux/README.org) - Terminal multiplexer setup and keybindings

## Quick Start

```bash
# Clone the repository
git clone https://github.com/bloodstiller/kaliconfigs.git

# For QEMU/KVM:
cd kaliconfigs/Vagrant/QEMU

# For VirtualBox:
cd kaliconfigs/Vagrant/VirtualBox

# Start the virtual machine
vagrant up

# SSH into the virtual machine
vagrant ssh
```

## Prerequisites

Choose your preferred virtualization provider:

### VirtualBox Setup
- [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
- [Vagrant](https://www.vagrantup.com/downloads)
- [Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

### QEMU/KVM Setup (Linux only)
- [QEMU/KVM](https://www.qemu.org/download/) (≥ 4.2)
- [Libvirt](https://libvirt.org/downloads.html) (≥ 6.2.0)
- [Vagrant](https://www.vagrantup.com/downloads)
- [Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

For QEMU/KVM shared folder support:
- Host: Linux kernel ≥ 5.4
- Guest: Linux kernel ≥ 5.4

## Included Tools & Features

### Security Tools
- SecLists for penetration testing
- Kerbrute for Kerberos authentication testing
- Statistically Likely Usernames wordlist
- Common pentesting tools
- Bloodhound for Active Directory analysis (dockerized)

### Development Environment
- Emacs 29.4 with native compilation
- Doom Emacs configuration
- Git version control
- Docker and Docker Compose (mainly for bloodhound)
- Build tools and development libraries

### Modern Terminal Experience
- Alacritty terminal emulator
- Starship cross-shell prompt
- Tmuxinator for session management

### Command Line Improvements
- Eza (modern ls replacement)
- Bat (better cat)
- Ripgrep for searching
- fd-find for file discovery
- Enhanced Zsh configuration:
  - zsh-autosuggestions
  - zsh-syntax-highlighting
  - zsh-autocomplete
  - fast-syntax-highlighting

## Project Structure

```bash
kaliconfigs/
├── Ansible/
│   ├── configure-kali.yml         # Main Ansible playbook (works with both providers)
│   └── configure-shared-folders.yml # VirtioFS configuration for QEMU/KVM
├── Vagrant/
│   ├── QEMU/                      # QEMU/KVM specific configuration
│   │   ├── Vagrantfile
│   │   └── README.md
│   └── VirtualBox/               # VirtualBox specific configuration
│       ├── Vagrantfile
│       └── README.md
└── README.md
```

## Provider-Specific Features

### VirtualBox
- Works on all major operating systems
- Simpler setup process
- Traditional shared folder mechanism

### QEMU/KVM
- Better performance on Linux hosts
- VirtioFS for efficient file sharing
- Native virtualization support

The main Ansible playbook (`configure-kali.yml`) works identically with both providers, ensuring a consistent environment regardless of your virtualization choice.

## Development Workflow

For efficient development and testing:

1. Rerun only Ansible playbooks:
   ```bash
   vagrant provision
   ```

2. Run Ansible directly against the VM:
   ```bash
   vagrant ssh-config > vagrant-ssh-config
   ansible-playbook -i .vagrant/provisioners/ansible/inventory/vagrant_ansible_inventory Ansible/configure-kali.yml
   ```

3. Use snapshots for safe testing:
   ```bash
   vagrant snapshot save baseline
   vagrant snapshot restore baseline
   ```

## Terminal Configuration

### Alacritty Settings

The Alacritty terminal emulator is configured with:
- Large scrollback history (100,000 lines)
- Customizable window properties
- JetBrains Mono and Ubuntu Mono fonts
- Extensive color theme support

Available color themes include:
- Alabaster (Light & Dark)
- Dracula
- Doom One
- Gruvbox Dark
- Nord
- Solarized (Light & Dark)
- And many more...

Configuration files can use either TOML or YAML format, with support for:
- Custom key bindings
- Font configuration
- Window opacity settings
- Theme importing
