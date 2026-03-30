#!/usr/bin/env sh

set -e

# ══════════════════════════════════════════════════════════════════════════════
#  COLOURS & STYLES
# ══════════════════════════════════════════════════════════════════════════════
ESC=$(printf '\033')
RESET="${ESC}[0m"
BOLD="${ESC}[1m"
DIM="${ESC}[2m"

RED="${ESC}[31m"
GREEN="${ESC}[32m"
YELLOW="${ESC}[33m"
CYAN="${ESC}[36m"

BG_RED="${ESC}[41m"
BG_GREEN="${ESC}[42m"
BG_BLUE="${ESC}[44m"

# ══════════════════════════════════════════════════════════════════════════════
#  STEP TRACKING
# ══════════════════════════════════════════════════════════════════════════════
TOTAL_STEPS=17
CURRENT_STEP=0
SCRIPT_START=$(date +%s)

# ══════════════════════════════════════════════════════════════════════════════
#  OUTPUT HELPERS
# ══════════════════════════════════════════════════════════════════════════════

banner() {
    clear
    printf "${BOLD}${CYAN}"
    cat <<'EOF'
  ██████╗ ██╗      ██████╗  ██████╗ ██████╗ ███████╗████████╗██╗██╗     ██╗     ███████╗██████╗
  ██╔══██╗██║     ██╔═══██╗██╔═══██╗██╔══██╗██╔════╝╚══██╔══╝██║██║     ██║     ██╔════╝██╔══██╗
  ██████╔╝██║     ██║   ██║██║   ██║██║  ██║███████╗   ██║   ██║██║     ██║     █████╗  ██████╔╝
  ██╔══██╗██║     ██║   ██║██║   ██║██║  ██║╚════██║   ██║   ██║██║     ██║     ██╔══╝  ██╔══██╗
  ██████╔╝███████╗╚██████╔╝╚██████╔╝██████╔╝███████║   ██║   ██║███████╗███████╗███████╗██║  ██║
  ╚═════╝ ╚══════╝ ╚═════╝  ╚═════╝ ╚═════╝ ╚══════╝   ╚═╝   ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═╝
EOF
    printf "${RESET}"
    printf "${DIM}  Kali Linux Environment Setup  •  bloodstiller.com${RESET}\n"
    printf "${DIM}  ─────────────────────────────────────────────────────────────────────────────${RESET}\n\n"
}

_progress_bar() {
    step=$1
    total=$2
    width=40
    filled=$(( step * width / total ))
    empty=$(( width - filled ))
    bar=""
    i=0
    while [ $i -lt $filled ]; do bar="${bar}█"; i=$(( i + 1 )); done
    i=0
    while [ $i -lt $empty ];  do bar="${bar}░"; i=$(( i + 1 )); done
    pct=$(( step * 100 / total ))
    printf "  ${DIM}[${RESET}${CYAN}${bar}${RESET}${DIM}]${RESET} ${BOLD}%3d%%${RESET}  ${DIM}step %d/%d${RESET}" \
        "$pct" "$step" "$total"
}

section() {
    CURRENT_STEP=$(( CURRENT_STEP + 1 ))
    label="$1"
    icon="$2"
    elapsed=$(( $(date +%s) - SCRIPT_START ))
    elapsed_fmt=$(printf '%dm%02ds' $(( elapsed / 60 )) $(( elapsed % 60 )))
    printf "\n"
    printf "  ${BOLD}${BG_BLUE}  %s  %s  ${RESET}  ${DIM}+%s${RESET}\n" "$icon" "$label" "$elapsed_fmt"
    printf "  "
    _progress_bar "$CURRENT_STEP" "$TOTAL_STEPS"
    printf "\n\n"
}

ok()   { printf "    ${GREEN}✔${RESET}  %s\n" "$1"; }
info() { printf "    ${CYAN}→${RESET}  %s\n" "$1"; }
warn() { printf "    ${YELLOW}⚠${RESET}  %s\n" "$1"; }

spin() {
    label="$1"; shift
    spin_chars="⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    i=0
    tmpout=$(mktemp)
    "$@" >"$tmpout" 2>&1 &
    cmd_pid=$!
    while kill -0 "$cmd_pid" 2>/dev/null; do
        pos=$(( (i % 10) + 1 ))
        c=$(printf '%s' "$spin_chars" | cut -c${pos})
        printf "    ${CYAN}%s${RESET}  ${DIM}%s...${RESET}\r" "$c" "$label"
        i=$(( i + 1 ))
        sleep 0.1
    done
    wait "$cmd_pid"; rc=$?
    if [ $rc -eq 0 ]; then
        printf "    ${GREEN}✔${RESET}  %-55s\n" "$label"
    else
        printf "    ${RED}✘${RESET}  %-55s\n" "$label"
        printf "\n${RED}  ── Error output ───────────────────────────────────────${RESET}\n"
        cat "$tmpout" | sed 's/^/  /'
        printf "${RED}  ────────────────────────────────────────────────────────${RESET}\n\n"
    fi
    rm -f "$tmpout"
    return $rc
}

spin_soft() {
    label="$1"; shift
    spin "$label" "$@" || warn "$label failed (non-fatal, continuing)"
}

safe_link() {
    src="$1"; dest="$2"
    if [ -e "$dest" ] || [ -L "$dest" ]; then sudo rm -f "$dest"; fi
    sudo ln -s "$src" "$dest"
    ok "linked $(basename "$src") → $dest"
}

safe_link_user() {
    src="$1"; dest="$2"
    if [ -e "$dest" ] || [ -L "$dest" ]; then rm -f "$dest"; fi
    ln -s "$src" "$dest"
    ok "linked $(basename "$src") → $dest"
}

# ══════════════════════════════════════════════════════════════════════════════
#  START
# ══════════════════════════════════════════════════════════════════════════════
banner

if [ "$(id -u)" -eq 0 ]; then
    printf "${BOLD}${RED}  ✘  Do not run this script as root.${RESET}\n\n"
    exit 1
fi

printf "  ${BOLD}${YELLOW}🔑  Sudo required${RESET}\n"
printf "  ${DIM}Enter your password once — it will stay alive for the duration.\n\n${RESET}"
sudo -v

(while true; do sudo -v; sleep 50; done) &
SUDO_KEEPALIVE_PID=$!

trap 'kill "$SUDO_KEEPALIVE_PID" 2>/dev/null; printf "\n${RED}  ✘  Script interrupted.\n${RESET}"' EXIT INT TERM

export DEBIAN_FRONTEND=noninteractive
echo "postfix postfix/main_mailer_type select No configuration" | sudo debconf-set-selections >/dev/null 2>&1

# ══════════════════════════════════════════════════════════════════════════════

section "System Update & Core Packages" "📦"
spin "apt update & upgrade"        sudo apt-get update -qq
spin "install core packages"       sudo apt-get install -y -qq \
    emacs exa bat ripgrep git tmux gnupg unzip fonts-firacode \
    python3-argcomplete atuin flameshot syncthing syncthingtray \
    golang-go ansifilter docker.io docker-buildx docker-compose \
    ntpsec-ntpdate hugo pandoc awscli codelite

# ══════════════════════════════════════════════════════════════════════════════

section "Docker Setup" "🐳"
spin "enable & start Docker"       sudo systemctl enable docker --now
spin "add $USER to docker group"   sudo usermod -aG docker "$USER"

# ══════════════════════════════════════════════════════════════════════════════

section "Go Tools — Nuclei & Katana" "⚡"
spin "install nuclei"              go install -v github.com/projectdiscovery/nuclei/v3/cmd/nuclei@latest
safe_link "$HOME/go/bin/nuclei" /usr/local/bin/nuclei

spin "install katana"              sh -c 'CGO_ENABLED=1 go install github.com/projectdiscovery/katana/cmd/katana@latest'
safe_link "$HOME/go/bin/katana" /usr/local/bin/katana

# ══════════════════════════════════════════════════════════════════════════════

section "Cloud Tools" "☁️"
spin "install cloudfox"            go install github.com/BishopFox/cloudfox@latest
safe_link "$HOME/go/bin/cloudfox" /usr/bin/cloudfox

spin "install scoutsuite"          pipx install scoutsuite
safe_link "$HOME/.local/share/pipx/venvs/scoutsuite/bin/scout" /usr/bin/scout

spin "install prowler"             pipx install prowler
spin "install roadrecon"           pipx install roadrecon

# ══════════════════════════════════════════════════════════════════════════════

section "Waymore" "🌊"
spin "install waymore"             pipx install git+https://github.com/xnl-h4ck3r/waymore.git

# ══════════════════════════════════════════════════════════════════════════════

section "bbot" "🤖"
if [ ! -d "$HOME/bbot" ]; then
    spin "clone bbot"              git clone https://github.com/blacklanternsecurity/bbot "$HOME/bbot"
else
    info "bbot already cloned — skipping"
fi

if [ -f "$HOME/bbot/bbot-docker.sh" ]; then
    chmod +x "$HOME/bbot/bbot-docker.sh"
    safe_link "$HOME/bbot/bbot-docker.sh" /usr/bin/bbot
else
    warn "bbot-docker.sh not found — check $HOME/bbot manually"
fi

# ══════════════════════════════════════════════════════════════════════════════

section "PMapper" "📐"
if [ ! -d "$HOME/pmapper" ]; then
    spin "clone PMapper"           git clone https://github.com/nccgroup/PMapper.git "$HOME/pmapper"
else
    info "PMapper already cloned — skipping"
fi
cd "$HOME/pmapper"
spin "create venv"                 python3 -m venv venv
spin "pip install"                 sh -c '. venv/bin/activate && pip install . -q && deactivate'
cd "$HOME"

# ══════════════════════════════════════════════════════════════════════════════

section "Oh My Zsh & Plugins" "🐚"
if [ ! -d "$HOME/.oh-my-zsh" ]; then
    spin "install oh-my-zsh" \
        sh -c 'RUNZSH=no CHSH=no sh -c "$(wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)"'
else
    info "oh-my-zsh already installed — skipping"
fi

ZSH_CUSTOM="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}"
mkdir -p "$ZSH_CUSTOM/plugins"

_zsh_plugin() {
    name="$1"; url="$2"; dest="$ZSH_CUSTOM/plugins/$name"
    if [ ! -d "$dest" ]; then
        spin "plugin: $name"       git clone --depth 1 "$url" "$dest"
    else
        info "plugin $name already exists — skipping"
    fi
}
_zsh_plugin zsh-syntax-highlighting   https://github.com/zsh-users/zsh-syntax-highlighting.git
_zsh_plugin zsh-autosuggestions       https://github.com/zsh-users/zsh-autosuggestions
_zsh_plugin fast-syntax-highlighting  https://github.com/zdharma-continuum/fast-syntax-highlighting.git
_zsh_plugin zsh-autocomplete          https://github.com/marlonrichert/zsh-autocomplete.git

if command -v zsh >/dev/null && grep -q "$(command -v zsh)" /etc/shells; then
    spin "set default shell to zsh" chsh -s "$(command -v zsh)"
else
    warn "zsh not in /etc/shells — skipping chsh"
fi

# ══════════════════════════════════════════════════════════════════════════════

section "Dotfiles" "📁"
if [ ! -d "$HOME/.dotfiles" ]; then
    spin "clone kaliconfigs"       git clone https://github.com/bloodstiller/kaliconfigs.git "$HOME/.dotfiles"
else
    info "dotfiles already cloned — skipping"
fi

# ══════════════════════════════════════════════════════════════════════════════

section "Doom Emacs" "☠️"
if [ ! -d "$HOME/.config/emacs" ]; then
    spin "clone doom emacs" \
        git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.config/emacs"
else
    info "doom emacs already cloned — skipping"
fi
printf "\n    ${DIM}Running doom install — this can take several minutes...${RESET}\n"
"$HOME/.config/emacs/bin/doom" install
ok "doom install complete"

# ══════════════════════════════════════════════════════════════════════════════

section "Nerd Fonts" "🔤"
mkdir -p "$HOME/.local/share/fonts/nerd-fonts"
cd /tmp
spin "download Iosevka"            wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Iosevka.zip
spin "download CommitMono"         wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CommitMono.zip
spin "unzip Iosevka"               unzip -q Iosevka.zip    -d "$HOME/.local/share/fonts/nerd-fonts/Iosevka"
spin "unzip CommitMono"            unzip -q CommitMono.zip -d "$HOME/.local/share/fonts/nerd-fonts/CommitMono"
rm -f Iosevka.zip CommitMono.zip
spin "refresh font cache"          fc-cache -fv
cd "$HOME"

# ══════════════════════════════════════════════════════════════════════════════

section "Misc Security Tools" "🔧"
mkdir -p "$HOME/.local/bin"
spin "download kerbrute" \
    wget -q https://github.com/ropnop/kerbrute/releases/download/v1.0.3/kerbrute_linux_amd64 \
         -O "$HOME/.local/bin/kerbrute"
chmod +x "$HOME/.local/bin/kerbrute"
ok "kerbrute installed → ~/.local/bin/kerbrute"

spin_soft "clone statistically-likely-usernames" \
    sudo git clone https://github.com/insidetrust/statistically-likely-usernames.git \
        /usr/share/wordlists/statistically-likely-usernames

if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
    spin "install tmux plugin manager" \
        git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
else
    info "tpm already installed — skipping"
fi

# ══════════════════════════════════════════════════════════════════════════════

section "Dotfile Symlinks" "🔗"
safe_link_user "$HOME/.dotfiles/Zsh/.zshrc" "$HOME/.zshrc"

rm -f "$HOME/.config/doom/"*.el
for f in "$HOME/.dotfiles/Doom/"*.el; do
    safe_link_user "$f" "$HOME/.config/doom/$(basename "$f")"
done

safe_link_user "$HOME/.dotfiles/Tmux/.tmux.conf"               "$HOME/.tmux.conf"
mkdir -p "$HOME/.config/alacritty"
safe_link_user "$HOME/.dotfiles/alacritty/alacritty.toml"      "$HOME/.config/alacritty/alacritty.toml"
safe_link_user /usr/share/wordlists                             "$HOME/Wordlists"

# ══════════════════════════════════════════════════════════════════════════════

section "Doom Sync & Git Config" "🔄"
printf "    ${DIM}Running doom sync...${RESET}\n"
"$HOME/.config/emacs/bin/doom" sync
ok "doom sync complete"
spin "set git user.name"           git config --global user.name  "bloodstiller"
spin "set git user.email"          git config --global user.email "bloodstiller@bloodstiller.com"

# ══════════════════════════════════════════════════════════════════════════════

section "VMware Shared Folder" "📂"
sudo mkdir -p /mnt/hgfs
spin_soft "mount vmhgfs" \
    sudo vmhgfs-fuse .host:/ /mnt/hgfs -o allow_other -o uid=1000

if mountpoint -q /mnt/hgfs 2>/dev/null; then
    echo ".host:/ /mnt/hgfs fuse.vmhgfs-fuse allow_other,defaults 0 0" | sudo tee -a /etc/fstab >/dev/null
    safe_link_user /mnt/hgfs/VMShare "$HOME/VMShare"
    ok "VMware share mounted and fstab updated"
else
    warn "VMware share not available — skipping fstab & symlink"
fi

# ══════════════════════════════════════════════════════════════════════════════
#  DONE
# ══════════════════════════════════════════════════════════════════════════════

kill "$SUDO_KEEPALIVE_PID" 2>/dev/null
trap - EXIT INT TERM

elapsed_total=$(( $(date +%s) - SCRIPT_START ))
elapsed_fmt=$(printf '%dm%02ds' $(( elapsed_total / 60 )) $(( elapsed_total % 60 )))

printf "\n"
printf "  ${BOLD}${BG_GREEN}                                                    ${RESET}\n"
printf "  ${BOLD}${BG_GREEN}   ✅  Setup complete in %-6s                     ${RESET}\n" "$elapsed_fmt"
printf "  ${BOLD}${BG_GREEN}                                                    ${RESET}\n"
printf "\n"
printf "  ${DIM}Next steps:${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}cat ../PostInstall/TODO.org${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Log out and back in for Docker group changes to take effect${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Press ${RESET}${BOLD}prefix + I${RESET}${DIM} inside tmux to install TPM plugins${RESET}\n"
printf "\n"
