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
    eza \
    bat \
    ripgrep \
    git \
    cmake \
    tmux \
    zsh \
    wget \
    curl \
    gnupg \
    unzip \
    fonts-firacode \
    python3-argcomplete \
    atuin \
    flameshot \
    syncthing \
    syncthingtray \
    ffuf \
    nmap \
    nikto \
    golang-go \
    ansifilter \
    docker.io \
    docker-buildx \
    docker-compose 

# Start Docker
sudo systemctl enable docker --now
# Add docker user so we don't have to use sudo
sudo usermod -aG docker $USER


# Install Nuclei
go install -v github.com/projectdiscovery/nuclei/v3/cmd/nuclei@latest
sudo ln -s ~/go/bin/nuclei /usr/local/bin/nuclei 
git clone https://github.com/bloodstiller/nuclei-templates.git ~/nuclei-templates/

#Install katana
CGO_ENABLED=1 go install github.com/projectdiscovery/katana/cmd/katana@latest
sudo ln -s ~/go/bin/katana /usr/local/bin/katana

#Install Waymore
pipx install git+https://github.com/xnl-h4ck3r/waymore.git

# Install Oh My Zsh without launching Zsh or changing shell automatically
export RUNZSH=no CHSH=no
sh -c "$(wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)"

# Set up Zsh plugins
ZSH_CUSTOM="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}"
mkdir -p "$ZSH_CUSTOM/plugins"
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$ZSH_CUSTOM/plugins/zsh-syntax-highlighting"
git clone https://github.com/zsh-users/zsh-autosuggestions "$ZSH_CUSTOM/plugins/zsh-autosuggestions"
git clone https://github.com/zdharma-continuum/fast-syntax-highlighting.git "$ZSH_CUSTOM/plugins/fast-syntax-highlighting"
git clone --depth 1 https://github.com/marlonrichert/zsh-autocomplete.git "$ZSH_CUSTOM/plugins/zsh-autocomplete"

# Change default shell to Zsh if available
if command -v zsh >/dev/null && grep -q "$(command -v zsh)" /etc/shells; then
    chsh -s "$(command -v zsh)"
else
    echo "⚠️ Zsh not found in /etc/shells. Skipping chsh."
fi

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
mkdir ~/VMShare
rm ~/.zshrc
ln -s ~/.dotfiles/Zsh/.zshrc ~/.zshrc
rm ~/.config/doom/*.el
ln -s ~/.dotfiles/Doom/*.el ~/.config/doom
ln -s ~/.dotfiles/Tmux/.tmux.conf ~/.tmux.conf
cd
./.config/emacs/bin/doom sync

