#!/usr/bin/env sh

set -e

# Ensure we're not running as root
if [ "$(id -u)" -eq 0 ]; then
    echo "❌ Do not run this script as root. Run it as your normal user."
    exit 1
fi

# Set noninteractive frontend for APT
export DEBIAN_FRONTEND=noninteractive

# Preseed Postfix to avoid interactive prompt (in case it's installed)
echo "postfix postfix/main_mailer_type select No configuration" | sudo debconf-set-selections

cd ~/

# Update and install essential system packages
sudo apt update && sudo apt upgrade -y
sudo apt install -y \
    emacs \
    bat \
    alacritty \
    exa \
    ripgrep \
    tmux \
    gnupg \
    fonts-firacode \
    python3-argcomplete \
    syncthing \
    syncthingtray \
    golang-go \
    ansifilter 

# Atuin Install
bash -c "curl --proto '=https' --tlsv1.2 -LsSf https://setup.atuin.sh | sh"

# Clone personal dotfiles
git clone https://github.com/bloodstiller/kaliconfigs.git "$HOME/.dotfiles"

# Install Doom Emacs
git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.config/emacs"
"$HOME/.config/emacs/bin/doom" install

echo "Be patient I am not hanging...."

# Nerd Fonts install
mkdir -p "$HOME/.local/share/fonts/nerd-fonts"
cd /tmp

# Download Nerd Fonts
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Iosevka.zip
wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CommitMono.zip

# Unzip fonts
unzip -q Iosevka.zip -d "$HOME/.local/share/fonts/nerd-fonts/Iosevka"
unzip -q CommitMono.zip -d "$HOME/.local/share/fonts/nerd-fonts/CommitMono"

# Clean up
rm Iosevka.zip CommitMono.zip

# Refresh font cache
fc-cache -fv


# Configure Dots
rm ~/.zshrc
ln -s ~/.dotfiles/Zsh/cloud_zshrc ~/.zshrc
rm ~/.config/doom/*.el
ln -s ~/.dotfiles/Doom/*.el ~/.config/doom
ln -s ~/.dotfiles/Tmux/.tmux.conf ~/.tmux.conf
cd
./.config/emacs/bin/doom sync

# Setup git
git config --global user.name "bloodstiller"
git config --global user.email "bloodstiller@bloodstiller.com"

# Add Shared Folder To Fstab
echo ".host:/ /mnt/hgfs fuse.vmhgfs-fuse allow_other,defaults 0 0" | sudo tee -a /etc/fstab

# Configure Shared Folder
ln -s /mnt/hgfs/VMShare ~/VMShare

#Next Steps
echo "################################"
echo "Read 'cat ../PostInstall/TODO.org' for next steps"
echo "################################"


