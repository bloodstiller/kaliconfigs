# Vagrant with QEMU/KVM Setup Guide

## Requirements

Using QEMU to create a Kali VM requires:
- libvirt (1.5.0 or newer)
- QEMU/KVM
- vagrant-libvirt plugin

## Install Required Packages

```bash
# For Debian/Ubuntu
sudo apt install qemu-kvm libvirt-daemon-system libvirt-clients bridge-utils

# For Arch Linux
sudo pacman -S qemu-full libvirt virt-manager
```

## Arch Linux Specific Setup
For Arch Linux users, there's a [specific installation process](https://marcstech.blog/archives/install-vagrant-libvirt-plugin-arch-linux/) for the vagrant-libvirt plugin:

1. Install required packages:
```bash
sudo pacman -S git vagrant dnsmasq iptables-nft libvirt qemu-base bridge-utils ruby pkgconf gcc make
```

2. Uninstall existing vagrant-libvirt plugin (if installed):
```bash
vagrant plugin uninstall vagrant-libvirt
```

3. Clone and build the plugin:
```bash
git clone https://github.com/vagrant-libvirt/vagrant-libvirt.git
cd vagrant-libvirt
```

4. Edit lib/vagrant-libvirt/driver.rb and remove the ip_command related lines:
```ruby
# Remove these lines:
# ip_command = %q( awk "/$mac/ {print \$1}" /proc/net/arp )
# libvirt_ip_command: ip_command,
```

5. Build and install:
```bash
gem build
VAGRANT_DISABLE_STRICT_DEPENDENCY_ENFORCEMENT=1 vagrant plugin install ./vagrant-libvirt-*.gem
```

## Enable and Start libvirtd Service

```bash
sudo systemctl enable --now libvirtd
```

## Add User to Required Groups

```bash
sudo usermod -aG libvirt $(whoami)
sudo usermod -aG kvm $(whoami)
```

## Install Vagrant Plugin

```bash
vagrant plugin install vagrant-libvirt
```

## Configure Shared Folders with VirtioFS

VirtioFS is the recommended way to share folders between host and guest. It requires:
- Host: Linux >= 5.4, QEMU >= 4.2 and Libvirt >= 6.2
- Guest: Linux >= 5.4

### 1. Install VirtioFS Daemon
```bash
sudo apt update
sudo apt install virtiofsd
```

### 2. Configure Memory Backing
Edit `/etc/libvirt/qemu.conf`:
```bash
sudo nano /etc/libvirt/qemu.conf
```

Add or modify:
```conf
memory_backing_dir = "/dev/shm"
```

### 3. Restart libvirt
```bash
sudo systemctl restart libvirtd
```

### 4. Verify VirtioFS Support
```bash
# Check kernel version
uname -r

# Check QEMU version
qemu-system-x86_64 --version

# Check libvirt version
libvirtd --version
```

## Verify Installation

```bash
# Check libvirt status
sudo systemctl status libvirtd

# List virtual machines
virsh list --all
```

## Usage

```bash
# Start the VM
vagrant up

# SSH into the VM
vagrant ssh

# Stop the VM
vagrant halt

# Remove the VM
vagrant destroy
```

## Troubleshooting

1. **Permission Issues**:
   - Make sure you've added your user to the libvirt and kvm groups
   - Log out and back in for group changes to take effect
   - Or use `exec su -l $(whoami)` to reload groups without logging out

2. **Shared Folder Issues**:
   - Ensure virtiofsd is installed and running
   - Check if shared memory is enabled in VM configuration
   - Verify folder permissions (750 recommended)
   - Check if virtiofs kernel module is loaded

3. **Network Issues**:
   - Check if libvirtd service is running
   - Ensure bridge-utils is installed
   - Verify network settings in Vagrantfile

4. **Graphics Issues**:
   - Make sure spice and qxl are configured correctly
   - X11 forwarding requires proper SSH configuration

## VM Configuration

The VM is configured with:
- 8GB RAM
- 4 CPU cores
- SPICE graphics with QXL driver
- X11 forwarding enabled
- Shared folders using VirtioFS
- Private network (192.168.57.11)

### Shared Folder Configuration
The VM is configured to share the following directories for myself:
- `/home/martin/.config/tmuxinator` → `/home/vagrant/.config/tmuxinator`
- `/home/martin/Dropbox/40-49_Career/45-KaliShared/45.02-LinuxTools` → `/home/vagrant/linuxTools`
- `/home/martin/Dropbox/40-49_Career/45-KaliShared/45.01-WindowsTools` → `/home/vagrant/windowsTools`
- `/home/martin/Dropbox/40-49_Career` → `/home/vagrant/Dropbox/40-49_Career`
- You can modify the Vagrantfile to add more shared folders and specify the folders you want to share.

See the Vagrantfile for detailed configuration options.