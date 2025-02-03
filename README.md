# Kali Linux Configuration Automation

This repository provides automated configuration scripts for setting up a fully-featured Kali Linux environment using Vagrant and Ansible. It creates a consistent, reproducible development environment that can be quickly deployed on any system supporting Vagrant.

## Quick Start

```bash
# Clone the repository
git clone https://github.com/bloodstiller/kaliconfigs.git

# Navigate to the vagrant directory
cd kaliconfigs/Vagrant

# Start the virtual machine
vagrant up

# SSH into the virtual machine
vagrant ssh
```

## Prerequisites

Before getting started, ensure you have the following installed:
- [Vagrant](https://www.vagrantup.com/downloads)
- [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
- [Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

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
│   ├── configure-kali.yml    # Main Ansible playbook
├── Vagrant/
│   ├── Vagrantfile          # Vagrant configuration
└── README.md
```

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
