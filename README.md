# Kali Linux Configuration Automation

This repository contains automated configuration scripts for setting up a Kali Linux environment using Vagrant and Ansible. It provides a consistent, reproducible development environment that can be quickly deployed on any system supporting Vagrant.

## Prerequisites

- [Vagrant](https://www.vagrantup.com/downloads)
- [VirtualBox](https://www.virtualbox.org/wiki/Downloads)
- [Ansible](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html)

## Quick Start

```bash
#clone the repository
git clone https://github.com/bloodstiller/kaliconfigs.git

#navigate to the vagrant directory
cd kaliconfigs/Vagrant

#start the virtual machine
vagrant up

# SSH into the virtual machine
vagrant ssh
```

## Features

- Automated Kali Linux setup using Vagrant
- Configuration management with Ansible
- Includes common security tools and utilities:
  - Emacs (no-X11 version)
  - Eza (modern ls replacement)
  - Alacritty terminal
  - Git
  - Bat (better cat)
  - SecLists

## Development and Testing

For faster development cycles, you can:

1. Use `vagrant provision` to rerun just the Ansible playbooks
2. Run Ansible directly against the VM:
   ```bash:45.00-KaliConfigs/README.md
   vagrant ssh-config > vagrant-ssh-config
   ansible-playbook -i .vagrant/provisioners/ansible/inventory/vagrant_ansible_inventory Ansible/configure-kali.yml
   ```
3. Take snapshots before major changes:
   ```bash
   vagrant snapshot save baseline
   vagrant snapshot restore baseline
   ```

## Directory Structure

```bash
kaliconfigs/
├── Ansible/
│ ├── configure-kali.yml
├── Vagrant/
│ ├── Vagrantfile
├── README.md
```

## Contributing

Feel free to submit issues and pull requests for additional configurations or improvements.

## License

[Your chosen license]

# Kali Linux Configuration Files

This repository contains various configuration files for Kali Linux.

## Alacritty

The `Alacritty/` directory contains configuration files for the Alacritty terminal emulator, including:

- `alacritty.toml` - Main configuration file with settings for:
  - Scrolling history
  - Window properties (padding, opacity, title)
  - Font configuration (JetBrains Mono and Ubuntu Mono)
  - Terminal colors
  - Key bindings

### Color Themes

Multiple color themes are available:
- Alabaster (Light & Dark variants)
- Afterglow
- Cobalt2
- Dracula
- Doom One
- Gruvbox Dark
- Mariana
- Monokai
- Nord
- OceanicNext
- Palenight
- Solarized (Light & Dark variants)
- Tomorrow Night

Themes can be imported using the `import` directive in the config file.

### Key Features

- Configurable font sizes and families
- Custom key bindings for common operations
- Multiple color schemes available
- Window padding and opacity settings
- Extensive scrollback buffer (100,000 lines)
- Support for both TOML and YAML configurations


## TODO
- [ ] Ensure emacs is installed only downloading repo currently.
- [ ] Mount other shared folders for doing boxes etc. 