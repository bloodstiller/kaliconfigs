#!/usr/bin/env bash
# =============================================================================
#  Exegol on Ubuntu — Host bootstrap + my-resources provisioning
#  bloodstiller.com
#
#  What this script does, in plain English:
#    1.  Brings a fresh Ubuntu VM up to spec: git, docker, pipx, zsh, tmux,
#        emacs, age, ripgrep, fzf, and other productivity packages.
#    2.  Installs Exegol (per the official docs: pipx + argcomplete + alias).
#    3.  Clones your kaliconfigs dotfiles so the *host* shell feels like home.
#    4.  Installs Oh My Zsh + plugins, Doom Emacs, Nerd Fonts.
#    5.  Wires your tmux / zsh / doom configs via symlinks.
#    6.  Wires your tmux / zsh configs into ~/.exegol/my-resources/setup/ so
#        EVERY exegol container you ever spin up inherits them automatically.
#    7.  Pre-positions mitmproxy2swagger and the Hacking-APIs wordlist.
#    8.  Scaffolds Burp Suite Pro for per-container activation.
#    9.  Installs sops, prompts for your age key, deploys SSH keys from the
#        encrypted vault (same flow as kali setup.sh).
#    10. Configures git user and switches dotfiles remote to SSH.
#    11. Optionally mounts a VMware shared folder.
#
#  Re-runnable: each section checkpoints itself.
#  To start fresh:  rm -rf ~/.exegol_setup_checkpoints
# =============================================================================

set -euo pipefail

# ──────────────────────────────────────────────────────────────────────────────
#  Colours & styles
# ──────────────────────────────────────────────────────────────────────────────
ESC=$(printf '\033')
RESET="${ESC}[0m"
BOLD="${ESC}[1m"
DIM="${ESC}[2m"
RED="${ESC}[31m"
GREEN="${ESC}[32m"
YELLOW="${ESC}[33m"
CYAN="${ESC}[36m"
BG_GREEN="${ESC}[42m"
BG_BLUE="${ESC}[44m"

# ──────────────────────────────────────────────────────────────────────────────
#  Step tracking & log file
# ──────────────────────────────────────────────────────────────────────────────
TOTAL_STEPS=17
CURRENT_STEP=0
SCRIPT_START=$(date +%s)

LOG="$HOME/exegol-setup-$(date +%Y%m%d-%H%M%S).log"
touch "$LOG"
log() { printf "[%s] %s\n" "$(date +%H:%M:%S)" "$*" >> "$LOG"; }

# ──────────────────────────────────────────────────────────────────────────────
#  Checkpoints — re-run friendly
# ──────────────────────────────────────────────────────────────────────────────
CHECKPOINT_DIR="$HOME/.exegol_setup_checkpoints"
mkdir -p "$CHECKPOINT_DIR"
is_done()   { [ -f "$CHECKPOINT_DIR/$1" ]; }
mark_done() { touch "$CHECKPOINT_DIR/$1"; log "CHECKPOINT: $1 complete"; }

# ──────────────────────────────────────────────────────────────────────────────
#  Tunables
# ──────────────────────────────────────────────────────────────────────────────
DOTFILES_REPO="https://github.com/bloodstiller/kaliconfigs.git"
DOTFILES_DIR="$HOME/.dotfiles"
EXEGOL_RES="$HOME/.exegol/my-resources"
WORDLISTS_DIR="$HOME/wordlists"

DF_ZSHRC="$DOTFILES_DIR/Zsh/.zshrc"
DF_ZSHENV="$DOTFILES_DIR/Zsh/.zshenv"
DF_TMUX="$DOTFILES_DIR/Tmux/.tmux.conf"

AGE_KEY_FILE="$HOME/.config/sops/age/keys.txt"

# ── Burp Suite Pro / JDK ──────────────────────────────────────────────────────
JDK_VERSION="23"
JDK_DIR="jdk-23"
JDK_URL_AMD64="https://download.java.net/java/GA/jdk23/3c5b90190c68498b986a97f276efd28a/37/GPL/openjdk-23_linux-x64_bin.tar.gz"
JDK_URL_ARM64="https://download.java.net/java/GA/jdk23/3c5b90190c68498b986a97f276efd28a/37/GPL/openjdk-23_linux-aarch64_bin.tar.gz"

# ──────────────────────────────────────────────────────────────────────────────
#  Output helpers
# ──────────────────────────────────────────────────────────────────────────────
banner() {
    clear
    printf "${BOLD}${CYAN}"
    cat <<'EOF'
  ███████╗██╗  ██╗███████╗ ██████╗  ██████╗ ██╗
  ██╔════╝╚██╗██╔╝██╔════╝██╔════╝ ██╔═══██╗██║
  █████╗   ╚███╔╝ █████╗  ██║  ███╗██║   ██║██║
  ██╔══╝   ██╔██╗ ██╔══╝  ██║   ██║██║   ██║██║
  ███████╗██╔╝ ██╗███████╗╚██████╔╝╚██████╔╝███████╗
  ╚══════╝╚═╝  ╚═╝╚══════╝ ╚═════╝  ╚═════╝ ╚══════╝
EOF
    printf "${RESET}"
    printf "${DIM}  Exegol Host Setup  •  bloodstiller.com${RESET}\n"
    printf "${DIM}  ─────────────────────────────────────────────────────────────────────────────${RESET}\n\n"
}

_progress_bar() {
    local step=$1 total=$2 width=40
    local filled=$(( step * width / total ))
    local empty=$(( width - filled ))
    local bar="" i=0
    while [ $i -lt $filled ]; do bar="${bar}█"; i=$(( i + 1 )); done
    i=0
    while [ $i -lt $empty ];  do bar="${bar}░"; i=$(( i + 1 )); done
    local pct=$(( step * 100 / total ))
    printf "  ${DIM}[${RESET}${CYAN}${bar}${RESET}${DIM}]${RESET} ${BOLD}%3d%%${RESET}  ${DIM}step %d/%d${RESET}" \
        "$pct" "$step" "$total"
}

section() {
    CURRENT_STEP=$(( CURRENT_STEP + 1 ))
    local label="$1" icon="$2"
    local elapsed=$(( $(date +%s) - SCRIPT_START ))
    local elapsed_fmt
    elapsed_fmt=$(printf '%dm%02ds' $(( elapsed / 60 )) $(( elapsed % 60 )))
    printf "\n"
    printf "  ${BOLD}${BG_BLUE}  %s  %s  ${RESET}  ${DIM}+%s${RESET}\n" "$icon" "$label" "$elapsed_fmt"
    printf "  "
    _progress_bar "$CURRENT_STEP" "$TOTAL_STEPS"
    printf "\n\n"
    log "SECTION $CURRENT_STEP/$TOTAL_STEPS: $label"
}

skip_section() {
    CURRENT_STEP=$(( CURRENT_STEP + 1 ))
    local label="$1" icon="$2"
    printf "\n"
    printf "  ${DIM}${BG_BLUE}  %s  %s  ${RESET}  ${DIM}(already done — skipping)${RESET}\n" "$icon" "$label"
    printf "  "
    _progress_bar "$CURRENT_STEP" "$TOTAL_STEPS"
    printf "\n"
    log "SKIP: $label"
}

ok()   { printf "    ${GREEN}✔${RESET}  %s\n" "$1"; log "OK: $1"; }
info() { printf "    ${CYAN}→${RESET}  %s\n" "$1"; log "INFO: $1"; }
warn() { printf "    ${YELLOW}⚠${RESET}  %s\n" "$1"; log "WARN: $1"; }

spin() {
    local label="$1"; shift
    local spin_chars="⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"
    local i=0
    local tmpout
    tmpout=$(mktemp)
    log "RUN: $label — $*"
    "$@" >>"$tmpout" 2>&1 &
    local cmd_pid=$!
    while kill -0 "$cmd_pid" 2>/dev/null; do
        local pos=$(( (i % 10) + 1 ))
        local c
        c=$(printf '%s' "$spin_chars" | cut -c${pos})
        printf "    ${CYAN}%s${RESET}  ${DIM}%s...${RESET}\r" "$c" "$label"
        i=$(( i + 1 ))
        sleep 0.1
    done
    wait "$cmd_pid"; local rc=$?
    cat "$tmpout" >> "$LOG"
    if [ $rc -eq 0 ]; then
        printf "    ${GREEN}✔${RESET}  %-55s\n" "$label"
        log "OK: $label"
    else
        printf "    ${RED}✘${RESET}  %-55s\n" "$label"
        printf "\n${RED}  ── Error output ───────────────────────────────────────${RESET}\n"
        sed 's/^/  /' "$tmpout"
        printf "${RED}  ────────────────────────────────────────────────────────${RESET}\n"
        printf "  ${DIM}Full log: %s${RESET}\n\n" "$LOG"
        log "FAIL: $label"
    fi
    rm -f "$tmpout"
    return $rc
}

spin_soft() {
    local label="$1"; shift
    spin "$label" "$@" || warn "$label failed (non-fatal, continuing)"
}

safe_link_user() {
    local src="$1" dest="$2"
    if [ -e "$dest" ] || [ -L "$dest" ]; then rm -f "$dest"; fi
    ln -s "$src" "$dest"
    ok "linked $(basename "$src") → $dest"
}

# ──────────────────────────────────────────────────────────────────────────────
#  sops / age helpers  (same flow as kali setup.sh)
# ──────────────────────────────────────────────────────────────────────────────

# Prompt for the age private key with echo disabled so it never appears
# on screen or in the scroll-back buffer. Validates the key before writing
# and uses install(1) to set permissions before any content touches disk.
read_age_key_securely() {
    printf "\n    ${BOLD}${YELLOW}🔑  Age private key required${RESET}\n"
    printf "    ${DIM}Retrieve from your password manager and paste the full key block.${RESET}\n"
    printf "    ${DIM}Input is NOT echoed. Press ${RESET}${BOLD}Ctrl+D${RESET}${DIM} on a blank line when done.${RESET}\n\n"

    mkdir -p "$(dirname "$AGE_KEY_FILE")"
    chmod 700 "$(dirname "$AGE_KEY_FILE")"

    local key_content
    stty -echo
    key_content=$(cat)
    stty echo
    printf "\n"

    if ! printf '%s' "$key_content" | grep -q "^AGE-SECRET-KEY-"; then
        printf "    ${RED}✘${RESET}  Input does not look like a valid age private key — aborting.\n"
        log "FAIL: age key validation — missing AGE-SECRET-KEY- prefix"
        return 1
    fi

    install -m 600 /dev/null "$AGE_KEY_FILE"
    printf '%s\n' "$key_content" > "$AGE_KEY_FILE"

    ok "Age key written → $AGE_KEY_FILE (chmod 600)"
    log "Age key written to $AGE_KEY_FILE"
}

# Decrypt the sops-encrypted secrets file and place each SSH key with
# the correct filename and permissions. Requires python3-yaml (in apt list).
deploy_ssh_keys() {
    local secrets_file="$1"

    if [ ! -f "$secrets_file" ]; then
        warn "Secrets file not found: $secrets_file — skipping SSH key deployment"
        return 1
    fi

    if [ ! -f "$AGE_KEY_FILE" ]; then
        warn "Age key not found at $AGE_KEY_FILE — skipping SSH key deployment"
        return 1
    fi

    info "Decrypting SSH keys via sops..."
    local decrypted
    if ! decrypted=$(SOPS_AGE_KEY_FILE="$AGE_KEY_FILE" sops --decrypt "$secrets_file" 2>>"$LOG"); then
        printf "    ${RED}✘${RESET}  sops decryption failed — check log: %s\n" "$LOG"
        log "FAIL: sops decrypt of $secrets_file"
        return 1
    fi

    mkdir -p "$HOME/.ssh"
    chmod 700 "$HOME/.ssh"

    python3 - "$HOME/.ssh" "$decrypted" <<'PYEOF'
import sys, os, yaml

ssh_dir  = sys.argv[1]
secrets  = yaml.safe_load(sys.argv[2])
keys     = secrets.get('ssh_keys', {})

key_map = {
    'kali_vet_ed25519':        ('kali_vet_ed25519',        0o600),
    'kali_vet_ed25519.pub':    ('kali_vet_ed25519.pub',    0o644),
    'sapphireKey_ed25519':     ('sapphireKey_ed25519',     0o600),
    'sapphireKey_ed25519.pub': ('sapphireKey_ed25519.pub', 0o644),
    'work_kali_ed25519':       ('work_kali_ed25519',        0o600),
    'work_kali_ed25519.pub':   ('work_kali_ed25519.pub',   0o644),
    'ssh_config':              ('config',                   0o600),
}

deployed = 0
for field, (filename, perms) in key_map.items():
    value = keys.get(field, '').strip()
    if not value:
        continue
    path = os.path.join(ssh_dir, filename)
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, perms)
    with os.fdopen(fd, 'w') as f:
        f.write(value + '\n')
    os.chmod(path, perms)
    print(f"    \033[32m✔\033[0m  Deployed {filename}")
    deployed += 1

if deployed == 0:
    print("    \033[33m⚠\033[0m  No SSH keys found in secrets file — check field names")
    sys.exit(1)
PYEOF

    ok "SSH keys deployed → ~/.ssh/"
    log "SSH keys deployed from $secrets_file"
}

# ──────────────────────────────────────────────────────────────────────────────
#  Pre-flight
# ──────────────────────────────────────────────────────────────────────────────
banner

if [ "$(id -u)" -eq 0 ]; then
    printf "${BOLD}${RED}  ✘  Do not run this script as root. Run as your normal user.${RESET}\n\n"
    exit 1
fi

if ! command -v apt-get >/dev/null 2>&1; then
    printf "${BOLD}${RED}  ✘  This script targets Ubuntu/Debian (apt). Aborting.${RESET}\n\n"
    exit 1
fi

printf "  ${BOLD}${YELLOW}🔑  Sudo required${RESET}\n"
printf "  ${DIM}Enter your password once — it will stay alive for the duration.${RESET}\n\n"
sudo -v

(while true; do sudo -v; sleep 50; done) &
SUDO_KEEPALIVE_PID=$!
trap 'kill "$SUDO_KEEPALIVE_PID" 2>/dev/null; printf "\n${RED}  ✘  Script interrupted. Log: '"$LOG"'\n${RESET}"' EXIT INT TERM

export DEBIAN_FRONTEND=noninteractive
echo "postfix postfix/main_mailer_type select No configuration" | sudo debconf-set-selections >/dev/null 2>&1

printf "  ${DIM}Log file: %s${RESET}\n\n" "$LOG"

# =============================================================================
# 1. SYSTEM PACKAGES
#    Installs both exegol host dependencies and general productivity tools
#    (emacs, ripgrep, fzf, age, syncthing, etc.) mirroring the non-pentest
#    packages from kali setup.sh.
# =============================================================================
section "System Update & Host Packages" "📦"
spin "apt update"   sudo apt-get update -qq
spin "apt upgrade"  sudo apt-get upgrade -y -qq
spin "install host packages" sudo apt-get install -y -qq \
    age \
    ca-certificates curl wget git unzip jq \
    python3 python3-pip python3-venv pipx python3-argcomplete python3-yaml \
    zsh tmux vim eza atuin bat ripgrep fd-find fzf \
    emacs \
    gnupg \
    openvpn \
    bash-completion \
    fonts-firacode \
    flameshot syncthing \
    hugo pandoc \
    ansifilter \
    alacritty
# fd is packaged as fd-find on Ubuntu; doom emacs expects 'fd' on PATH
if [ ! -e "$HOME/.local/bin/fd" ] && command -v fdfind >/dev/null 2>&1; then
    mkdir -p "$HOME/.local/bin"
    ln -s "$(command -v fdfind)" "$HOME/.local/bin/fd"
    ok "linked fd → fdfind"
fi
spin "snap install obsidian" sudo snap install obsidian --classic
# Create workspace directories used by engagements and tools
mkdir -p "$HOME/Tools" "$HOME/Engagements"
ok "created ~/Tools and ~/Engagements"

# =============================================================================
# 2. DOCKER
#    Exegol requires docker. We do NOT add $USER to the docker group — that
#    effectively gives the user root anyway (docker socket → root), which
#    Exegol's own docs warn against. Instead, section 3 wires up the sudo
#    alias so `exegol` runs as root with the user's env preserved.
# =============================================================================
if is_done "docker"; then
    skip_section "Docker Engine" "🐳"
else
    section "Docker Engine" "🐳"
    if ! command -v docker >/dev/null 2>&1; then
        spin "install docker.io"  sudo apt-get install -y -qq docker.io docker-compose-plugin
    else
        info "docker already installed — skipping"
    fi
    spin "enable & start docker"  sudo systemctl enable docker --now
    mark_done "docker"
fi

# =============================================================================
# 3. EXEGOL — pipx install per https://docs.exegol.com/first-install
# =============================================================================
if is_done "exegol"; then
    skip_section "Exegol Wrapper" "🧪"
else
    section "Exegol Wrapper" "🧪"
    spin "pipx ensurepath"  pipx ensurepath
    export PATH="$HOME/.local/bin:$PATH"

    if ! command -v exegol >/dev/null 2>&1; then
        spin "install exegol via pipx"  pipx install exegol
    else
        info "exegol already installed — running upgrade instead"
        spin_soft "pipx upgrade exegol"  pipx upgrade exegol
    fi

    # Argcomplete for zsh AND bash so tab-completion works on either shell.
    # Written to .zshenv (not .zshrc) — section 7 symlinks .zshrc from the
    # dotfiles repo, which would silently overwrite anything added here.
    ARGCOMPLETE_LINE='eval "$(register-python-argcomplete --no-defaults exegol)"'
    for rc in "$HOME/.bashrc" "$HOME/.zshenv"; do
        if [ -f "$rc" ] && ! grep -qF "register-python-argcomplete --no-defaults exegol" "$rc"; then
            echo "$ARGCOMPLETE_LINE" >> "$rc"
            ok "added exegol argcomplete → $rc"
        fi
    done

    # `sudo -E` preserves HOME so the wrapper still finds ~/.exegol/my-resources/.
    EXEGOL_BIN="$HOME/.local/bin/exegol"
    ALIAS_LINE="alias exegol='sudo -E $EXEGOL_BIN'"
    for rc in "$HOME/.bashrc" "$HOME/.zshenv"; do
        if [ -f "$rc" ] && ! grep -qF "alias exegol=" "$rc"; then
            echo "$ALIAS_LINE" >> "$rc"
            ok "added exegol sudo-alias → $rc"
        fi
    done

    mark_done "exegol"
fi

# =============================================================================
# 4. DOTFILES — clone your kaliconfigs so the HOST shell feels right
# =============================================================================
if is_done "dotfiles"; then
    skip_section "Dotfiles (kaliconfigs)" "📁"
else
    section "Dotfiles (kaliconfigs)" "📁"
    if [ ! -d "$DOTFILES_DIR" ]; then
        spin "clone kaliconfigs"  git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
    else
        info "dotfiles already cloned — skipping"
    fi
    mark_done "dotfiles"
fi

# =============================================================================
# 5. OH-MY-ZSH + PLUGINS (host shell)
# =============================================================================
if is_done "ohmyzsh"; then
    skip_section "Oh My Zsh & Plugins (host)" "🐚"
else
    section "Oh My Zsh & Plugins (host)" "🐚"
    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        spin "install oh-my-zsh" \
            bash -c 'RUNZSH=no CHSH=no sh -c "$(wget -qO- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"'
    else
        info "oh-my-zsh already installed — skipping"
    fi

    ZSH_CUSTOM="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}"
    mkdir -p "$ZSH_CUSTOM/plugins"
    _zsh_plugin() {
        local name="$1" url="$2" dest="$ZSH_CUSTOM/plugins/$1"
        if [ ! -d "$dest" ]; then
            spin "plugin: $name"  git clone --depth 1 "$url" "$dest"
        else
            info "plugin $name already exists — skipping"
        fi
    }
    _zsh_plugin zsh-syntax-highlighting   https://github.com/zsh-users/zsh-syntax-highlighting.git
    _zsh_plugin zsh-autosuggestions       https://github.com/zsh-users/zsh-autosuggestions
    _zsh_plugin fast-syntax-highlighting  https://github.com/zdharma-continuum/fast-syntax-highlighting.git
    _zsh_plugin fzf-tab                   https://github.com/Aloxaf/fzf-tab.git

    if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
        spin "install tmux plugin manager" \
            git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
    else
        info "tpm already installed — skipping"
    fi

    # Ubuntu does not ship zsh as default
    if [ "$(getent passwd "$USER" | cut -d: -f7)" != "$(command -v zsh)" ]; then
        spin "chsh to zsh"  sudo chsh -s "$(command -v zsh)" "$USER"
        warn "Default shell changed to zsh — takes effect on next login."
    fi

    mark_done "ohmyzsh"
fi

# =============================================================================
# 6. DOOM EMACS
#    Installed before the dotfile symlinks section so ~/.config/doom/ exists
#    when we link the .el files in section 7.
# =============================================================================
if is_done "doom"; then
    skip_section "Doom Emacs" "☠️"
else
    section "Doom Emacs" "☠️"
    if [ ! -d "$HOME/.config/emacs" ]; then
        spin "clone doom emacs" \
            git clone --depth 1 https://github.com/doomemacs/doomemacs "$HOME/.config/emacs"
    else
        info "doom emacs already cloned — skipping"
    fi
    printf "\n    ${DIM}Running doom install — this can take several minutes...${RESET}\n\n"
    "$HOME/.config/emacs/bin/doom" install
    ok "doom install complete"
    mark_done "doom"
fi

# =============================================================================
# 7. HOST DOTFILE SYMLINKS
#    Links shell, editor, and terminal configs from the dotfiles repo.
# =============================================================================
if is_done "dotfile_links"; then
    skip_section "Host Dotfile Symlinks" "🔗"
else
    section "Host Dotfile Symlinks" "🔗"
    [ -f "$DF_ZSHRC" ]  && safe_link_user "$DF_ZSHRC"  "$HOME/.zshrc"     || warn "$DF_ZSHRC not found"
    [ -f "$DF_ZSHENV" ] && safe_link_user "$DF_ZSHENV" "$HOME/.zshenv"    || warn "$DF_ZSHENV not found"
    [ -f "$DF_TMUX" ]   && safe_link_user "$DF_TMUX"   "$HOME/.tmux.conf" || warn "$DF_TMUX not found"

    # Doom Emacs config — replace doom's default .el files with ours
    if [ -d "$DOTFILES_DIR/Doom" ]; then
        mkdir -p "$HOME/.config/doom"
        rm -f "$HOME/.config/doom/"*.el
        for f in "$DOTFILES_DIR/Doom/"*.el; do
            safe_link_user "$f" "$HOME/.config/doom/$(basename "$f")"
        done
    else
        warn "$DOTFILES_DIR/Doom not found — skipping doom config links"
    fi

    # Alacritty terminal config
    mkdir -p "$HOME/.config/alacritty"
    if [ -f "$DOTFILES_DIR/alacritty/alacritty.toml" ]; then
        safe_link_user "$DOTFILES_DIR/alacritty/alacritty.toml" \
            "$HOME/.config/alacritty/alacritty.toml"
    else
        warn "$DOTFILES_DIR/alacritty/alacritty.toml not found — skipping"
    fi

    mark_done "dotfile_links"
fi

# =============================================================================
# 8. MY-RESOURCES SCAFFOLD
#    Create the full directory tree Exegol expects under ~/.exegol/my-resources.
#    Reference: https://docs.exegol.com/images/my-resources
# =============================================================================
if is_done "myresources_scaffold"; then
    skip_section "my-resources Scaffold" "🗂️"
else
    section "my-resources Scaffold" "🗂️"
    mkdir -p \
        "$EXEGOL_RES/bin" \
        "$EXEGOL_RES/setup/zsh" \
        "$EXEGOL_RES/setup/tmux" \
        "$EXEGOL_RES/setup/vim" \
        "$EXEGOL_RES/setup/apt" \
        "$EXEGOL_RES/setup/python3" \
        "$EXEGOL_RES/setup/firefox" \
        "$EXEGOL_RES/setup/arsenal-cheats" \
        "$EXEGOL_RES/wordlists"
    ok "created $EXEGOL_RES tree"
    mark_done "myresources_scaffold"
fi

# =============================================================================
# 9. CONFIG FILES → my-resources/setup/
#    tmux.conf  — overwrites container's ~/.tmux.conf
#    zsh/zshrc  — APPENDED to Exegol's own zshrc (do NOT replace)
#    zsh/aliases — sourced automatically
#    vim/vimrc  — overwrites container's ~/.vimrc
# =============================================================================
if is_done "myresources_configs"; then
    skip_section "Container Configs → my-resources" "⚙️"
else
    section "Container Configs → my-resources" "⚙️"

    if [ -f "$DF_TMUX" ]; then
        cp "$DF_TMUX" "$EXEGOL_RES/setup/tmux/tmux.conf"
        ok "tmux.conf  →  my-resources/setup/tmux/tmux.conf"
    else
        warn "no tmux.conf in dotfiles — skipping"
    fi

    # IMPORTANT: Exegol APPENDS my-resources/setup/zsh/zshrc to its own zshrc.
    # Strip OMZ boilerplate — exegol has its own OMZ setup.
    if [ -f "$DF_ZSHRC" ]; then
        cat > "$EXEGOL_RES/setup/zsh/zshrc" <<EOF
# ──────────────────────────────────────────────────────────────
#  bloodstiller — custom zshrc additions, appended to exegol's
#  Source of truth: $DF_ZSHRC
#  Edit there and re-run exegol-setup.sh to refresh.
# ──────────────────────────────────────────────────────────────
EOF
        grep -vE '^(source.*oh-my-zsh\.sh|ZSH=|ZSH_THEME=|plugins=\()' "$DF_ZSHRC" \
            >> "$EXEGOL_RES/setup/zsh/zshrc"
        ok "zshrc (filtered)  →  my-resources/setup/zsh/zshrc"
    fi

    cat > "$EXEGOL_RES/setup/zsh/aliases" <<'EOF'
# bloodstiller — API testing aliases (auto-loaded by exegol zshrc)

# Wordlists — Hacking-APIs is our addition; seclists is shipped with exegol
export APIWL='/opt/my-resources/wordlists/Hacking-APIs-main'
export SECLISTS='/usr/share/seclists'

# Burp proxy toggles (burp itself is shipped with exegol)
export http_proxy_burp='http://127.0.0.1:8080'
export https_proxy_burp='http://127.0.0.1:8080'
alias burpproxy='export http_proxy=$http_proxy_burp https_proxy=$https_proxy_burp; echo "Burp proxy ON"'
alias unproxy='unset http_proxy https_proxy; echo "Proxy OFF"'

# mitmproxy2swagger — the only API tool we install ourselves
alias mp2sw='mitmproxy2swagger'

# Burp Suite Pro — backgrounded so the shell stays usable.
# Requires java-burp-setup.sh to have been run inside this container once.
alias burp='nohup java -jar /opt/my-resources/bin/BurpSuitePro/burpsuite_pro.jar >/dev/null 2>&1 & disown'

# Reminders for exegol-native tools (so muscle-memory from kali works):
#   jwt           → ticarpi/jwt_tool
#   kiterunner    → assetnote/kiterunner
#   arjun         → s0md3v/Arjun
#   kerbrute      → ropnop/kerbrute
EOF
    ok "zsh aliases  →  my-resources/setup/zsh/aliases"

    if [ -f "$HOME/.vimrc" ]; then
        cp "$HOME/.vimrc" "$EXEGOL_RES/setup/vim/vimrc"
        ok "vimrc  →  my-resources/setup/vim/vimrc"
    fi

    mark_done "myresources_configs"
fi

# =============================================================================
# 10. APT + PIP customisations for each new container
# =============================================================================
if is_done "myresources_pkgs"; then
    skip_section "Container Packages (apt + pip)" "📦"
else
    section "Container Packages (apt + pip)" "📦"

    cat > "$EXEGOL_RES/setup/apt/packages.list" <<'EOF'
# Extra APT packages for every new exegol container — bloodstiller
# Exegol full/web/ad images already include most things. Add lines below
# only for tools NOT shipped in your chosen image.
EOF
    ok "apt/packages.list seeded (empty by default)"

    cat > "$EXEGOL_RES/setup/python3/requirements.txt" <<'EOF'
# Python packages installed in every new exegol container — bloodstiller
# Only list things NOT already in exegol full image.
mitmproxy2swagger
EOF
    ok "python3/requirements.txt seeded (mitmproxy2swagger only)"

    mark_done "myresources_pkgs"
fi

# =============================================================================
# 11. load_user_setup.sh — runs ONCE per new container at first start
# =============================================================================
if is_done "load_user_setup"; then
    skip_section "load_user_setup.sh" "🚀"
else
    section "load_user_setup.sh" "🚀"

    cat > "$EXEGOL_RES/setup/load_user_setup.sh" <<'LOAD_EOF'
#!/usr/bin/env bash
# =============================================================================
#  bloodstiller — per-container first-run init
#  Runs ONCE the first time a new exegol container starts.
#  Log: /var/log/exegol/load_setups.log
# =============================================================================
set -u
LOG=/var/log/exegol/load_setups.log
exec >> "$LOG" 2>&1
echo "[$(date)] load_user_setup.sh START"

if command -v nuclei >/dev/null 2>&1; then
    echo "[+] Updating nuclei templates..."
    nuclei -update-templates -silent || true
fi

cat > /etc/motd <<'MOTD'

  ┌──────────────────────────────────────────────────────────────────┐
  │  bloodstiller exegol container                                   │
  │  Native: jwt, kiterunner, arjun, kerbrute, impacket, netexec    │
  │  Added : mitmproxy2swagger                                       │
  │  Lists : $APIWL (Hacking-APIs), $SECLISTS (seclists)             │
  │  Proxy : burpproxy / unproxy                                     │
  └──────────────────────────────────────────────────────────────────┘

MOTD

echo "[$(date)] load_user_setup.sh DONE"
LOAD_EOF
    chmod +x "$EXEGOL_RES/setup/load_user_setup.sh"
    ok "load_user_setup.sh installed"

    mark_done "load_user_setup"
fi

# =============================================================================
# 12. WORDLISTS
#     SecLists is already in exegol full image at /usr/share/seclists,
#     so we skip it. Hacking-APIs is NOT — clone it once on the host.
# =============================================================================
if is_done "wordlists"; then
    skip_section "Wordlists (Hacking-APIs)" "📚"
else
    section "Wordlists (Hacking-APIs)" "📚"
    mkdir -p "$WORDLISTS_DIR"

    if [ ! -d "$EXEGOL_RES/wordlists/Hacking-APIs-main" ]; then
        spin "download Hacking-APIs" \
            wget -q https://github.com/hAPI-hacker/Hacking-APIs/archive/refs/heads/main.zip \
                 -O /tmp/HackingAPIs.zip
        spin "unzip Hacking-APIs"  unzip -q /tmp/HackingAPIs.zip -d "$EXEGOL_RES/wordlists/"
        rm -f /tmp/HackingAPIs.zip
    else
        info "Hacking-APIs already present — skipping"
    fi

    if [ ! -L "$WORDLISTS_DIR/Hacking-APIs" ]; then
        safe_link_user "$EXEGOL_RES/wordlists/Hacking-APIs-main" "$WORDLISTS_DIR/Hacking-APIs"
    fi

    mark_done "wordlists"
fi

# =============================================================================
# 13. BURP SUITE PRO BOOTSTRAP
#     Downloads OpenJDK + generates the per-container helper script.
#     The Burp installer itself must be downloaded manually from portswigger.
# =============================================================================
if is_done "burp_pro"; then
    skip_section "Burp Suite Pro Bootstrap" "🕷️"
else
    section "Burp Suite Pro Bootstrap" "🕷️"

    ARCH="$(uname -m)"
    case "$ARCH" in
        x86_64|amd64)
            JDK_URL="$JDK_URL_AMD64"
            JDK_TARBALL="openjdk-${JDK_VERSION}_linux-x64_bin.tar.gz"
            ;;
        aarch64|arm64)
            JDK_URL="$JDK_URL_ARM64"
            JDK_TARBALL="openjdk-${JDK_VERSION}_linux-aarch64_bin.tar.gz"
            ;;
        *)
            warn "Unknown arch '$ARCH' — skipping Burp Pro bootstrap."
            warn "Edit JDK_URL_* tunables at top of script and re-run."
            mark_done "burp_pro"
            ARCH=""
            ;;
    esac

    if [ -n "$ARCH" ]; then
        info "Architecture: $ARCH  →  $JDK_TARBALL"

        if [ ! -f "$EXEGOL_RES/bin/$JDK_TARBALL" ]; then
            spin "download OpenJDK ${JDK_VERSION}" \
                wget -q "$JDK_URL" -O "$EXEGOL_RES/bin/$JDK_TARBALL"
        else
            info "JDK tarball already present — skipping"
        fi

        cat > "$EXEGOL_RES/bin/java-burp-setup.sh" <<EOF
#!/usr/bin/env bash
# =============================================================================
#  java-burp-setup.sh — RUN MANUALLY INSIDE EACH NEW EXEGOL CONTAINER
#  Generated by exegol-setup.sh on $(date -Iseconds)
#  Source: https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/
#
#  Pre-reqs (done once on the HOST before any of this):
#    - Burp Pro installed at /opt/my-resources/bin/BurpSuitePro/
#    - prefs.xml copied to  /opt/my-resources/bin/prefs.xml
# =============================================================================
set -e

JDK_TARBALL="${JDK_TARBALL}"
JDK_DIR="${JDK_DIR}"
MR=/opt/my-resources/bin

# 1. Stage the JDK
if [ ! -d "/usr/lib/jvm/\$JDK_DIR" ]; then
    cp "\$MR/\$JDK_TARBALL" /usr/lib/jvm/
    ( cd /usr/lib/jvm && tar -xzf "\$JDK_TARBALL" && rm -f "\$JDK_TARBALL" )
fi

# 2. Register with alternatives (priority 2 — higher than exegol's default)
# (no sudo needed — exegol containers run as root)
update-alternatives --install /usr/bin/java java "/usr/lib/jvm/\$JDK_DIR/bin/java" 2

# 3. Interactive picker — choose the new JDK as default
update-alternatives --config java

# 4. Apply the activated Burp prefs
mkdir -p /root/.java/.userPrefs/burp
if [ -f "\$MR/prefs.xml" ]; then
    cp "\$MR/prefs.xml" /root/.java/.userPrefs/burp/prefs.xml
    echo "[+] Burp prefs.xml applied — activation carried over from host."
else
    echo "[!] \$MR/prefs.xml not found."
    echo "    On the HOST: copy ~/.java/.userPrefs/burp/prefs.xml into ~/.exegol/my-resources/bin/"
    echo "    then re-run this script."
fi

cat <<'BANNER'

  ┌──────────────────────────────────────────────────────────────────┐
  │  Burp Pro ready. Launch with:                                    │
  │    java -jar /opt/my-resources/bin/BurpSuitePro/burpsuite_pro.jar│
  └──────────────────────────────────────────────────────────────────┘

BANNER
EOF
        chmod +x "$EXEGOL_RES/bin/java-burp-setup.sh"
        ok "java-burp-setup.sh generated (run manually inside each container)"

        if [ ! -d "$EXEGOL_RES/bin/BurpSuitePro" ]; then
            warn "Burp Pro not yet installed in $EXEGOL_RES/bin/BurpSuitePro/"
        fi
        if [ ! -f "$EXEGOL_RES/bin/prefs.xml" ]; then
            warn "prefs.xml not yet copied to $EXEGOL_RES/bin/prefs.xml"
        fi

        mark_done "burp_pro"
    fi
fi

# =============================================================================
# 14. NERD FONTS — Iosevka & CommitMono for alacritty / emacs
# =============================================================================
if is_done "fonts"; then
    skip_section "Nerd Fonts" "🔤"
else
    section "Nerd Fonts" "🔤"
    mkdir -p "$HOME/.local/share/fonts/nerd-fonts"
    spin "download Iosevka"    wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Iosevka.zip    -O /tmp/Iosevka.zip
    spin "download CommitMono" wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CommitMono.zip -O /tmp/CommitMono.zip
    spin "unzip Iosevka"       unzip -q /tmp/Iosevka.zip    -d "$HOME/.local/share/fonts/nerd-fonts/Iosevka"
    spin "unzip CommitMono"    unzip -q /tmp/CommitMono.zip -d "$HOME/.local/share/fonts/nerd-fonts/CommitMono"
    rm -f /tmp/Iosevka.zip /tmp/CommitMono.zip
    spin "refresh font cache"  fc-cache -fv
    mark_done "fonts"
fi

# =============================================================================
# 15. SSH SECRETS — install sops, prompt for age key, decrypt & deploy SSH keys
#
#  Prerequisites (one-time setup on your trusted machine):
#    1. age-keygen -o ~/.config/sops/age/keys.txt
#    2. Add the public key to .sops.yaml at the root of kaliconfigs
#    3. sops --encrypt secrets/ssh_keys_plain.yaml > secrets/ssh_keys.yaml
#    4. Commit secrets/ssh_keys.yaml (safe to be public)
#    5. Store the private key in your password manager
# =============================================================================
if is_done "ssh_secrets"; then
    skip_section "SSH Secrets (sops/age)" "🔐"
else
    section "SSH Secrets (sops/age)" "🔐"

    if ! command -v sops &>/dev/null; then
        info "Fetching latest sops release..."
        SOPS_VERSION=$(curl -s https://api.github.com/repos/getsops/sops/releases/latest \
            | grep '"tag_name"' | cut -d'"' -f4)
        spin "download sops ${SOPS_VERSION}" \
            wget -q "https://github.com/getsops/sops/releases/download/${SOPS_VERSION}/sops-${SOPS_VERSION}.linux.amd64" \
                 -O /tmp/sops-bin
        sudo install -m 755 /tmp/sops-bin /usr/local/bin/sops
        rm -f /tmp/sops-bin
        ok "sops installed → /usr/local/bin/sops"
    else
        info "sops already installed — skipping download"
    fi

    SECRETS_FILE="$DOTFILES_DIR/secrets/ssh_keys.yaml"

    if [ ! -f "$SECRETS_FILE" ]; then
        warn "Secrets file not found at $SECRETS_FILE"
        warn "Ensure kaliconfigs contains secrets/ssh_keys.yaml (sops-encrypted)"
        warn "Skipping SSH key deployment — add keys manually later"
    else
        if [ -f "$AGE_KEY_FILE" ]; then
            info "Age key already present at $AGE_KEY_FILE — skipping prompt"
        else
            read_age_key_securely || warn "Age key entry failed — skipping SSH key deployment"
        fi

        if [ -f "$AGE_KEY_FILE" ]; then
            deploy_ssh_keys "$SECRETS_FILE"
        fi
    fi

    mark_done "ssh_secrets"
fi

# =============================================================================
# 16. DOOM SYNC & GIT CONFIG
# =============================================================================
if is_done "doom_sync"; then
    skip_section "Doom Sync & Git Config" "🔄"
else
    section "Doom Sync & Git Config" "🔄"
    printf "    ${DIM}Running doom sync...${RESET}\n\n"
    "$HOME/.config/emacs/bin/doom" sync
    ok "doom sync complete"
    spin "set git user.name"       git config --global user.name  "bloodstiller"
    spin "set git user.email"      git config --global user.email "bloodstiller@bloodstiller.com"
    spin "set dotfiles remote url" git -C "$DOTFILES_DIR" remote set-url origin git@github.com:bloodstiller/kaliconfigs.git
    mark_done "doom_sync"
fi

# =============================================================================
# 17. VMWARE SHARED FOLDER (non-fatal — only applies when running as a VM)
# =============================================================================
if is_done "vmware"; then
    skip_section "VMware Shared Folder" "💾"
else
    section "VMware Shared Folder" "💾"
    sudo mkdir -p /mnt/hgfs
    spin_soft "mount vmhgfs" \
        sudo vmhgfs-fuse .host:/ /mnt/hgfs -o allow_other -o uid=1000
    if mountpoint -q /mnt/hgfs 2>/dev/null; then
        echo ".host:/ /mnt/hgfs fuse.vmhgfs-fuse allow_other,defaults 0 0" | sudo tee -a /etc/fstab >/dev/null
        safe_link_user /mnt/hgfs/VMShare "$HOME/VMShare"
        ok "VMware share mounted and fstab updated"
    else
        warn "VMware share not available — skipping fstab & symlink (safe to ignore on bare metal)"
    fi
    mark_done "vmware"
fi

# =============================================================================
# DONE
# =============================================================================
kill "$SUDO_KEEPALIVE_PID" 2>/dev/null
trap - EXIT INT TERM

elapsed_total=$(( $(date +%s) - SCRIPT_START ))
elapsed_fmt=$(printf '%dm%02ds' $(( elapsed_total / 60 )) $(( elapsed_total % 60 )))

printf "\n"
printf "  ${BOLD}${BG_GREEN}                                                        ${RESET}\n"
printf "  ${BOLD}${BG_GREEN}   ✅  Host setup complete in %-6s                    ${RESET}\n" "$elapsed_fmt"
printf "  ${BOLD}${BG_GREEN}                                                        ${RESET}\n"
printf "\n"
printf "  ${DIM}Next steps:${RESET}\n"
printf "  ${CYAN}→${RESET}  ${BOLD}Open a new shell${RESET}${DIM} so zsh, the exegol alias + argcomplete take effect.${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Press ${RESET}${BOLD}prefix + I${RESET}${DIM} inside tmux to install TPM plugins.${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Pull your first image:${RESET}  ${BOLD}exegol install full${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Start a container:${RESET}      ${BOLD}exegol start test full${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}If tmux falls back to bash, start with:${RESET} ${BOLD}-e SHELL=/usr/bin/zsh${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}my-resources mounts at:${RESET}  ${BOLD}/opt/my-resources${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Full log:${RESET} %s\n" "$LOG"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  A note on the zsh integration${RESET}\n"
printf "  ${DIM}Exegol APPENDS my-resources/setup/zsh/zshrc to its own zshrc — it does NOT${RESET}\n"
printf "  ${DIM}replace it. If you see plugin double-load or theme weirdness, edit${RESET}\n"
printf "  ${DIM}~/.exegol/my-resources/setup/zsh/zshrc on the HOST and recreate the container.${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  SSH secrets / age key${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Age key persists at ${RESET}${BOLD}~/.config/sops/age/keys.txt${RESET}${DIM} (chmod 600)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Rotate SSH keys: ${RESET}${BOLD}SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt sops ~/.dotfiles/secrets/ssh_keys.yaml${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Re-run SSH deploy only: ${RESET}${BOLD}rm ~/.exegol_setup_checkpoints/ssh_secrets && ./exegol-setup.sh${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}🕷️  Burp Suite Pro — finish these MANUAL steps on the host${RESET}\n"
printf "  ${DIM}The JDK tarball and helper script are already in place. You still need:${RESET}\n"
printf "\n"
printf "  ${CYAN}1.${RESET}  Download the Burp Pro Linux installer from ${BOLD}https://portswigger.net/users/${RESET}\n"
printf "      ${DIM}(login required — license is bound to your account)${RESET}\n"
printf "  ${CYAN}2.${RESET}  ${BOLD}cd ~/.exegol/my-resources/bin/ && bash burpsuite_pro_linux_*.sh${RESET}\n"
printf "      ${DIM}Install path: ${RESET}${BOLD}~/.exegol/my-resources/bin/BurpSuitePro${RESET}\n"
printf "  ${CYAN}3.${RESET}  Launch Burp on the host once, paste your license key, complete activation\n"
printf "      ${DIM}(burns ONE activation — propagates to all containers via prefs.xml)${RESET}\n"
printf "  ${CYAN}4.${RESET}  ${BOLD}cp ~/.java/.userPrefs/burp/prefs.xml ~/.exegol/my-resources/bin/${RESET}\n"
printf "\n"
printf "  ${DIM}Then, INSIDE each new container (run ONCE per container):${RESET}\n"
printf "  ${CYAN}→${RESET}  ${BOLD}/opt/my-resources/bin/java-burp-setup.sh${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Launch Burp:${RESET} ${BOLD}burp${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  VPN configs${RESET}\n"
printf "  ${CYAN}→${RESET}  Pass .ovpn per-engagement: ${BOLD}exegol start <name> full --vpn <path>${RESET}\n"
printf "\n"
