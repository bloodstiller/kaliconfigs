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
#        zsh/aliases (auto-loaded) and zsh/zshrc (appended by exegol) carry
#        only the container-safe subset of ~/.zshrc — see inline comments.
#        Also wires load_user_setup.sh to scaffold a box folder structure
#        (loot/ticket/scans/payloads) and a /workspace/.env for per-box vars
#        (box/machine/domain, editable via `update_var`) on first container start.
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
TOTAL_STEPS=25
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

# ── pyenv / prowler ───────────────────────────────────────────────────────────
# Prowler is pinned to a pyenv-managed interpreter so it never breaks when
# Ubuntu bumps the system python. Change PYENV_PY to move prowler's runtime.
PYENV_ROOT_DIR="$HOME/.pyenv"
PYENV_PY=3.12

# ── Burp Suite Pro / JDK ──────────────────────────────────────────────────────
# JDK version is resolved at runtime via the Eclipse Temurin (Adoptium) API.
# To pin to a specific LTS, change JDK_FEATURE here.
JDK_FEATURE=21   # current LTS; supported until 2029

_temurin_url() {
    local arch="$1"
    curl -s "https://api.adoptium.net/v3/assets/latest/${JDK_FEATURE}/hotspot?os=linux&architecture=${arch}&image_type=jdk&vendor=eclipse" \
        | jq -r '.[0].binary.package.link // empty'
}

# GitHub REST calls are capped at 60/hr per IP when unauthenticated — easy to
# burn through across a full host-setup run (Obsidian, Nerd Fonts, sops,
# ligolo-ng all hit api.github.com). If $GITHUB_TOKEN is set, use it to raise
# the ceiling to 5000/hr. Always returns the raw body on stdout; the caller
# checks the return code, so a rate-limit (or any other failure) can be
# retried on the next run instead of silently no-op'ing.
_gh_api() {
    local url="$1" body status
    local -a curl_args=(-s -w '\n%{http_code}' "$url")
    if [ -n "${GITHUB_TOKEN:-}" ]; then
        curl_args=(-s -w '\n%{http_code}' -H "Authorization: Bearer ${GITHUB_TOKEN}" "$url")
    fi
    local resp
    resp=$(curl "${curl_args[@]}")
    status="${resp##*$'\n'}"
    body="${resp%$'\n'"$status"}"

    if [ "$status" != "200" ]; then
        local reset_msg=""
        if printf '%s' "$body" | grep -qi 'rate limit'; then
            reset_msg=" — set \$GITHUB_TOKEN to raise the 60/hr limit, or wait for it to reset"
        fi
        warn "GitHub API request failed (HTTP $status): $url${reset_msg}"
        return 1
    fi

    printf '%s' "$body"
}

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

UBUNTU_MAJOR=$(lsb_release -sr 2>/dev/null | cut -d. -f1)
if [ -z "$UBUNTU_MAJOR" ] || [ "$UBUNTU_MAJOR" -lt 22 ]; then
    printf "${BOLD}${RED}  ✘  Ubuntu 22.04+ required (detected: %s). Aborting.${RESET}\n\n" \
        "$(lsb_release -sd 2>/dev/null || echo unknown)"
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

# GitHub REST calls (Obsidian, Nerd Fonts, sops, ligolo-ng) are capped at
# 60/hr per IP when unauthenticated — easy to exhaust over a full run.
# A token raises that to 5000/hr. Kept in-memory only for this run via
# $GITHUB_TOKEN (used by _gh_api further down) — never written to disk.
if [ -n "${GITHUB_TOKEN:-}" ]; then
    info "Using GITHUB_TOKEN already set in environment"
else
    printf "  ${BOLD}${YELLOW}🔑  GitHub API token (optional)${RESET}\n"
    printf "  ${DIM}Raises the api.github.com rate limit from 60/hr to 5000/hr for this run.${RESET}\n"
    printf "  ${DIM}Used only in-memory — never written to disk. Press Enter to skip.${RESET}\n\n"
    printf "    Token (input hidden): "
    GITHUB_TOKEN_INPUT=""
    read -rs GITHUB_TOKEN_INPUT || true
    printf "\n\n"
    if [ -n "$GITHUB_TOKEN_INPUT" ]; then
        export GITHUB_TOKEN="$GITHUB_TOKEN_INPUT"
        ok "GitHub token set for this session (in-memory only)"
    else
        info "No GitHub token provided — using unauthenticated rate limit (60/hr)"
    fi
    unset GITHUB_TOKEN_INPUT
fi

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
    keepassxc \
    emacs \
    btop \
    gnupg \
    openvpn \
    bash-completion \
    fonts-firacode \
    flameshot syncthing \
    libfuse2t64 \
    hugo pandoc \
    ansifilter \
    alacritty \
    kitty \
    luarocks libmagickwand-dev \
    nodejs npm

# luarocks magick — required by image.nvim for inline image rendering in Kitty
if ! sudo luarocks list 2>/dev/null | grep -q "^magick"; then
    spin "luarocks install magick"  sudo luarocks install magick
else
    info "luarocks magick already installed — skipping"
fi

# fd is packaged as fd-find on Ubuntu; doom emacs expects 'fd' on PATH
if [ ! -e "$HOME/.local/bin/fd" ] && command -v fdfind >/dev/null 2>&1; then
    mkdir -p "$HOME/.local/bin"
    ln -s "$(command -v fdfind)" "$HOME/.local/bin/fd"
    ok "linked fd → fdfind"
fi
#spin "snap install obsidian" sudo snap install obsidian --classic

# Neovim — snap gives the latest stable release
if ! command -v nvim >/dev/null 2>&1; then
    spin "snap install neovim"  sudo snap install nvim --classic
else
    info "nvim already installed — skipping"
fi
# Create workspace directories used by engagements and tools
mkdir -p "$HOME/Tools" "$HOME/Engagements"
ok "created ~/Tools and ~/Engagements"

# =============================================================================
# 2. OBSIDIAN — latest .deb from GitHub releases
# =============================================================================
if is_done "obsidian"; then
    skip_section "Obsidian" "📝"
else
    section "Obsidian" "📝"

    ARCH="$(uname -m)"
    case "$ARCH" in
        x86_64|amd64)  OBS_ARCH="amd64" ;;
        aarch64|arm64) OBS_ARCH="arm64" ;;
        *)
            warn "Unknown arch '$ARCH' — skipping Obsidian install"
            mark_done "obsidian"
            OBS_ARCH=""
            ;;
    esac

    if [ -n "$OBS_ARCH" ]; then
        info "Fetching latest Obsidian release info..."
        OBS_DEB_URL=""
        if OBS_JSON=$(_gh_api "https://api.github.com/repos/obsidianmd/obsidian-releases/releases/latest"); then
            OBS_DEB_URL=$(printf '%s' "$OBS_JSON" \
                | jq -r ".assets[] | select(.name | endswith(\"_${OBS_ARCH}.deb\")) | .browser_download_url" \
                | head -1)
        fi

        if [ -z "$OBS_DEB_URL" ]; then
            warn "Could not resolve Obsidian .deb URL for $OBS_ARCH — skipping"
        else
            OBS_VERSION=$(basename "$OBS_DEB_URL" | grep -oE '[0-9]+\.[0-9]+\.[0-9]+')
            spin "download Obsidian ${OBS_VERSION}" \
                wget -q --timeout=30 --tries=3 --waitretry=3 "$OBS_DEB_URL" -O /tmp/obsidian.deb
            spin "install Obsidian ${OBS_VERSION}" \
                sudo apt-get install -y -qq /tmp/obsidian.deb
            rm -f /tmp/obsidian.deb
            ok "Obsidian ${OBS_VERSION} installed"
        fi
        mark_done "obsidian"
    fi
fi

# =============================================================================
# 3. DOCKER
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
        spin "add docker GPG key" \
            bash -c 'curl -fsSL https://download.docker.com/linux/ubuntu/gpg \
                | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg'
        spin "add docker apt source" \
            bash -c 'echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] \
                https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" \
                | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null'
        spin "apt update (docker source)"  sudo apt-get update -qq
        spin "install docker-ce" \
            sudo apt-get install -y -qq docker-ce docker-ce-cli containerd.io docker-compose-plugin
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
            bash -c 'RUNZSH=no CHSH=no sh -c "$(wget --timeout=30 --tries=3 --waitretry=3 -qO- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"'
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

    # Kitty terminal config
    mkdir -p "$HOME/.config/kitty"
    if [ -f "$DOTFILES_DIR/kitty/kitty.conf" ]; then
        safe_link_user "$DOTFILES_DIR/kitty/kitty.conf" \
            "$HOME/.config/kitty/kitty.conf"
    else
        warn "$DOTFILES_DIR/kitty/kitty.conf not found — skipping"
    fi

    # Neovim config — symlink dotfiles/nvim as ~/.config/nvim
    if [ -d "$DOTFILES_DIR/nvim" ]; then
        if [ -d "$HOME/.config/nvim" ] && [ ! -L "$HOME/.config/nvim" ]; then
            mv "$HOME/.config/nvim" "$HOME/.config/nvim.bak"
            warn "Backed up existing ~/.config/nvim to ~/.config/nvim.bak"
        fi
        ln -sfn "$DOTFILES_DIR/nvim" "$HOME/.config/nvim"
        ok "linked ~/.config/nvim → $DOTFILES_DIR/nvim"
    else
        warn "$DOTFILES_DIR/nvim not found — skipping nvim config link"
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
        "$EXEGOL_RES/setup/nvim" \
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
#    nvim/      — symlinked to /root/.config/nvim by load_user_setup.sh
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

    if [ -d "$DOTFILES_DIR/nvim" ]; then
        cp -r "$DOTFILES_DIR/nvim/." "$EXEGOL_RES/setup/nvim/"
        ok "nvim/      →  my-resources/setup/nvim/ (wired by load_user_setup.sh)"
    else
        warn "$DOTFILES_DIR/nvim not found — skipping nvim container config"
    fi

    cat > "$EXEGOL_RES/setup/zsh/aliases" <<'EOF'
# bloodstiller — API testing aliases (auto-loaded by exegol zshrc)

# Wordlists — Hacking-APIs is our addition; seclists is shipped with exegol
export APIWL='/opt/my-resources/wordlists/Hacking-APIs'
export SECLISTS='/usr/share/seclists'

# Burp proxy toggles (burp itself is shipped with exegol)
export http_proxy_burp='http://127.0.0.1:8080'
export https_proxy_burp='http://127.0.0.1:8080'
alias burpproxy='export http_proxy=$http_proxy_burp https_proxy=$https_proxy_burp; echo "Burp proxy ON"'
alias unproxy='unset http_proxy https_proxy; echo "Proxy OFF"'

# mitmproxy2swagger and goclone — installed by load_user_setup.sh on first container start
alias mp2sw='mitmproxy2swagger'

# Burp Suite Pro — backgrounded so the shell stays usable.
# Requires java-burp-setup.sh to have been run inside this container once.
alias burp='nohup java -jar /opt/my-resources/bin/BurpSuitePro/burpsuite_pro.jar >/dev/null 2>&1 & disown'

# ── from ~/.zshrc — container-safe subset only ─────────────────────────────
# (host-only bits — oh-my-zsh sourcing, theme, compinit, HISTFILE, doom/dotfiles
#  aliases, the `exegol` wrapper alias — deliberately left out; see zsh/zshrc
#  below for the rest. Full rationale: bloodstiller.com box-bootstrap notes.)
alias ls='eza -T -L=1 -a -B -h -l -g --icons'
alias lsl='eza -T -L=2 -a -B -h -l -g --icons'
alias lss='eza -T -L=1 -B -h -l -g --icons'
command -v batcat >/dev/null 2>&1 && alias cat='batcat'
alias urldecode='python3 -c "import sys, urllib.parse as ul; print(ul.unquote_plus(sys.argv[1]))"'
alias urlencode='python3 -c "import sys, urllib.parse as ul; print(ul.quote_plus(sys.argv[1]))"'
alias pws='python3 -m http.server 9000'
# Ligolo tun interface — needs NET_ADMIN in the container (exegol full image,
# started with adequate privileges); no-ops with an error otherwise.
alias lgu='sudo ip tuntap add user $(whoami) mode tun ligolo && sudo ip link set ligolo up'

# Reminders for exegol-native tools (so muscle-memory from kali works):
#   jwt           → ticarpi/jwt_tool
#   kiterunner    → assetnote/kiterunner
#   arjun         → s0md3v/Arjun
#   kerbrute      → ropnop/kerbrute
EOF
    ok "zsh aliases  →  my-resources/setup/zsh/aliases"

    # zsh/zshrc — exegol APPENDS this to the end of its own zshrc routine
    # (never replaces it). Keep this additive-only: no compinit, no theme/
    # plugin re-sourcing, no HISTFILE override — those belong to exegol's
    # own zshrc and duplicating them here is what the docs warn against.
    cat > "$EXEGOL_RES/setup/zsh/zshrc" <<'EOF'
# bloodstiller — appended to exegol's zshrc on every new container.
# Additive only — do not redefine ZSH_THEME/plugins/compinit/HISTFILE here;
# exegol's own zshrc already owns those.

# ── Engagement variables ────────────────────────────────────────────────────
# Persisted in /workspace/.env (survives container restarts — /workspace is
# exegol's per-container mount). Edit directly, or: update_var box 10.10.10.5
[ -f /workspace/.env ] && source /workspace/.env

update_var() {
    local envfile="/workspace/.env"
    touch "$envfile"
    if grep -q "^export $1=" "$envfile" 2>/dev/null; then
        sed -i "s|^export $1=.*|export $1=\"$2\"|" "$envfile"
    else
        echo "export $1=\"$2\"" >> "$envfile"
    fi
    source "$envfile"
}

txtlog2md() {
    setopt localoptions nullglob
    local files=( *.txt *.log )
    (( ${#files} )) || { echo "No .txt or .log files found."; return 1; }
    for f in $files; do
        mv -- "$f" "${f%.*}.md"
    done
}

# ── Tmux auto-logging — one file per pane per day under ~/tmux_logs ────────
if [ -n "$TMUX_PANE" ] && [ "$TMUX_PANE_LOGGING" != "1" ]; then
    export TMUX_PANE_LOGGING=1
    LOGS="$HOME/tmux_logs/$(date +%Y-%m-%d)"
    mkdir -p "$LOGS"
    LOG_PATH="$LOGS/pane${TMUX_PANE//[^0-9]/}.log"
    tmux pipe-pane -o "ansifilter >> $LOG_PATH"
fi

# ── Deferred heavy init — guarded, so it's a silent no-op if not installed ─
command -v atuin  >/dev/null 2>&1 && eval "$(atuin init zsh)"
(( $+commands[pip] )) && eval "$(register-python-argcomplete pip)" &!
EOF
    ok "zsh zshrc    →  my-resources/setup/zsh/zshrc (appended by exegol)"

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
eza
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

# Scaffold the box folder structure inside /workspace — mirrors the folder
# layout from the Obsidian box-bootstrap Templater script (loot, ticket,
# scans/*, payloads) so the container's working dir matches the wiki note
# from the first command. /workspace is exegol's per-container persistent
# mount, so the container name already IS the box name — nothing to prompt
# for. Safe to re-run: mkdir -p is a no-op on existing folders.
if [ -d /workspace ]; then
    echo "[+] Scaffolding box folder structure in /workspace..."
    mkdir -p \
        /workspace/loot \
        /workspace/ticket \
        /workspace/scans/nmap \
        /workspace/scans/bloodhound \
        /workspace/scans/ldap \
        /workspace/scans/burpsuite \
        /workspace/scans/nikto \
        /workspace/scans/nuclei \
        /workspace/scans/nessus \
        /workspace/payloads
    echo "[+] Box folders ready → /workspace/{loot,ticket,scans/*,payloads}"

    # Engagement vars file — sourced by my-resources/setup/zsh/zshrc, edited
    # via `update_var box <ip>` etc. Only seeded once; never overwritten.
    if [ ! -f /workspace/.env ]; then
        cat > /workspace/.env <<'ENV_EOF'
# bloodstiller — per-box engagement variables. Edit directly or via
# `update_var <name> <value>` (defined in zsh/zshrc). Sourced on every shell.
export box=""
export machine=""
export domain=""
ENV_EOF
        echo "[+] Seeded /workspace/.env with box/machine/domain placeholders"
    else
        echo "[+] /workspace/.env already present — leaving as-is"
    fi
else
    echo "[!] /workspace not found — skipping box folder scaffold"
fi

# Symlink Hacking-APIs wordlist to the standard container wordlist path so it
# sits alongside seclists at /usr/share/wordlists/.
if [ -d /opt/my-resources/wordlists/Hacking-APIs ]; then
    mkdir -p /usr/share/wordlists
    ln -sfn /opt/my-resources/wordlists/Hacking-APIs /usr/share/wordlists/Hacking-APIs
    echo "[+] Linked Hacking-APIs → /usr/share/wordlists/Hacking-APIs"
else
    echo "[!] /opt/my-resources/wordlists/Hacking-APIs not found — run host setup first"
fi

# Symlink nvim config from my-resources into the container's config directory.
if [ -d /opt/my-resources/setup/nvim ]; then
    mkdir -p /root/.config
    ln -sfn /opt/my-resources/setup/nvim /root/.config/nvim
    echo "[+] Linked nvim config → /root/.config/nvim"
else
    echo "[!] /opt/my-resources/setup/nvim not found — run host setup first"
fi

# Install goclone (website cloner) via Go. Exegol full image ships Go.
# Binary is copied to /usr/local/bin so it's on PATH for every shell.
if ! command -v goclone >/dev/null 2>&1; then
    if command -v go >/dev/null 2>&1; then
        echo "[+] Installing goclone..."
        go install github.com/imthaghost/goclone@latest
        if [ -f "$HOME/go/bin/goclone" ]; then
            cp "$HOME/go/bin/goclone" /usr/local/bin/goclone
            echo "[+] goclone installed → /usr/local/bin/goclone"
        else
            echo "[!] goclone build succeeded but binary not found at $HOME/go/bin/goclone"
        fi
    else
        echo "[!] go not found — skipping goclone install"
    fi
else
    echo "[+] goclone already installed — skipping"
fi

cat > /etc/motd <<'MOTD'

  ┌──────────────────────────────────────────────────────────────────┐
  │  bloodstiller exegol container                                   │
  │  Native: jwt, kiterunner, arjun, kerbrute, impacket, netexec    │
  │  Added : mitmproxy2swagger, goclone                              │
  │  Lists : $APIWL (Hacking-APIs), $SECLISTS (seclists)             │
  │  Proxy : burpproxy / unproxy                                     │
  │  C#    : /opt/my-resources/bin/SharpCollection/NetFramework_4.7_x86 │
  │  Pivot : /opt/my-resources/bin/ligolo-ng/{agent,proxy}/<platform> │
  │  Box   : /workspace/{loot,ticket,scans/*,payloads}                │
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

    if [ ! -d "$EXEGOL_RES/wordlists/Hacking-APIs" ]; then
        spin "clone Hacking-APIs wordlist" \
            git clone --depth 1 https://github.com/hAPI-hacker/Hacking-APIs.git \
                "$EXEGOL_RES/wordlists/Hacking-APIs"
    else
        info "Hacking-APIs already present — skipping"
    fi

    if [ ! -L "$WORDLISTS_DIR/Hacking-APIs" ]; then
        safe_link_user "$EXEGOL_RES/wordlists/Hacking-APIs" "$WORDLISTS_DIR/Hacking-APIs"
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
    JDK_URL=""
    JDK_TARBALL=""
    case "$ARCH" in
        x86_64|amd64)
            info "Resolving latest Eclipse Temurin JDK ${JDK_FEATURE} (x64)..."
            JDK_URL=$(_temurin_url x64)
            ;;
        aarch64|arm64)
            info "Resolving latest Eclipse Temurin JDK ${JDK_FEATURE} (aarch64)..."
            JDK_URL=$(_temurin_url aarch64)
            ;;
        *)
            warn "Unknown arch '$ARCH' — skipping Burp Pro bootstrap."
            mark_done "burp_pro"
            ARCH=""
            ;;
    esac

    if [ -n "$ARCH" ]; then
        if [ -z "$JDK_URL" ]; then
            warn "Could not resolve Temurin JDK URL from Adoptium API — skipping Burp Pro bootstrap."
            mark_done "burp_pro"
        else
            JDK_TARBALL=$(basename "$JDK_URL")
            info "Architecture: $ARCH  →  $JDK_TARBALL"

            if [ ! -f "$EXEGOL_RES/bin/$JDK_TARBALL" ]; then
                spin "download Eclipse Temurin JDK ${JDK_FEATURE}" \
                    wget -q --timeout=30 --tries=3 --waitretry=3 "$JDK_URL" -O "$EXEGOL_RES/bin/$JDK_TARBALL"
            else
                info "JDK tarball already present — skipping"
            fi

            # Discover the extracted directory name from the tarball rather than hardcoding it.
            # Temurin tarballs extract to a path like jdk-21.0.x+y/.
            JDK_DIR=$(tar -tzf "$EXEGOL_RES/bin/$JDK_TARBALL" 2>/dev/null | head -1 | cut -d/ -f1) || true
            if [ -z "$JDK_DIR" ]; then
                warn "Could not determine JDK directory name from tarball — skipping java-burp-setup.sh generation"
                mark_done "burp_pro"
                JDK_DIR=""
            fi
        fi
    fi

    if [ -n "${JDK_DIR:-}" ]; then
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
# 14. NERD FONTS — Iosevka, CommitMono & UbuntuMono for alacritty / emacs
# =============================================================================
if is_done "fonts"; then
    skip_section "Nerd Fonts" "🔤"
else
    section "Nerd Fonts" "🔤"
    mkdir -p "$HOME/.local/share/fonts/nerd-fonts"
    NF_VERSION="v3.4.0"
    if NF_JSON=$(_gh_api "https://api.github.com/repos/ryanoasis/nerd-fonts/releases/latest"); then
        NF_VERSION=$(printf '%s' "$NF_JSON" | jq -r '.tag_name // "v3.4.0"')
    else
        warn "Falling back to pinned Nerd Fonts version ${NF_VERSION}"
    fi
    info "Nerd Fonts release: $NF_VERSION"
    for _nf_font in Iosevka CommitMono UbuntuMono; do
        if [ -d "$HOME/.local/share/fonts/nerd-fonts/${_nf_font}" ]; then
            info "${_nf_font} already present — skipping"
            continue
        fi
        spin "download ${_nf_font}" \
            wget -q --timeout=30 --tries=3 --waitretry=3 \
                 "https://github.com/ryanoasis/nerd-fonts/releases/download/${NF_VERSION}/${_nf_font}.zip" \
                 -O "/tmp/${_nf_font}.zip"
        spin "unzip ${_nf_font}" \
            unzip -qo "/tmp/${_nf_font}.zip" -d "$HOME/.local/share/fonts/nerd-fonts/${_nf_font}"
        rm -f "/tmp/${_nf_font}.zip"
    done
    spin_soft "refresh font cache"  fc-cache -f
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
        SOPS_VERSION=""
        if SOPS_JSON=$(_gh_api "https://api.github.com/repos/getsops/sops/releases/latest"); then
            SOPS_VERSION=$(printf '%s' "$SOPS_JSON" | jq -r '.tag_name // empty')
        fi
        _sops_arch=""
        case "$(uname -m)" in
            x86_64|amd64)  _sops_arch="amd64" ;;
            aarch64|arm64) _sops_arch="arm64" ;;
            *) warn "Unknown arch '$(uname -m)' — cannot download sops binary" ;;
        esac
        if [ -n "$SOPS_VERSION" ] && [ -n "$_sops_arch" ]; then
            spin "download sops ${SOPS_VERSION} (${_sops_arch})" \
                wget -q --timeout=30 --tries=3 --waitretry=3 \
                     "https://github.com/getsops/sops/releases/download/${SOPS_VERSION}/sops-${SOPS_VERSION}.linux.${_sops_arch}" \
                     -O /tmp/sops-bin
            sudo install -m 755 /tmp/sops-bin /usr/local/bin/sops
            rm -f /tmp/sops-bin
            ok "sops installed → /usr/local/bin/sops"
        else
            warn "Could not resolve sops download — skipping (install manually from https://github.com/getsops/sops/releases)"
        fi
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
        FSTAB_ENTRY=".host:/ /mnt/hgfs fuse.vmhgfs-fuse allow_other,defaults 0 0"
        if grep -qF "$FSTAB_ENTRY" /etc/fstab; then
            info "fstab entry already present — skipping"
        else
            echo "$FSTAB_ENTRY" | sudo tee -a /etc/fstab >/dev/null
            ok "fstab entry added → /etc/fstab"
        fi

        spin_soft "mount -a (verify fstab takes effect)"  sudo mount -a

        if mountpoint -q /mnt/hgfs 2>/dev/null; then
            ok "VMware share verified active after fstab update"
        else
            warn "VMware share not mounted after mount -a — check fstab and vmhgfs-fuse"
        fi

        if [ -d "/mnt/hgfs/Pentest" ]; then
            safe_link_user /mnt/hgfs/Pentest "$HOME/Pentest"
        fi
    else
        warn "VMware share not available — skipping fstab & symlink (safe to ignore on bare metal)"
    fi
    mark_done "vmware"
fi

# =============================================================================
# 18. HACKTRICKS & REVSHELLS — clone wikis, build Docker images, launchers
# =============================================================================
if is_done "hacktricks_revshells"; then
    skip_section "HackTricks & RevShells" "📖"
else
    section "HackTricks & RevShells" "📖"

    mkdir -p "$HOME/Tools"

    if [ ! -d "$HOME/Tools/hacktricks" ]; then
        spin "clone HackTricks wiki"   git clone https://github.com/HackTricks-wiki/hacktricks "$HOME/Tools/hacktricks"
    else
        info "HackTricks already cloned — skipping"
    fi

    if [ ! -d "$HOME/Tools/reverse-shell-generator" ]; then
        spin "clone revshells"         git clone https://github.com/0dayCTF/reverse-shell-generator.git "$HOME/Tools/reverse-shell-generator"
    else
        info "revshells already cloned — skipping"
    fi
    if ! sudo docker image inspect reverse_shell_generator >/dev/null 2>&1; then
        spin_soft "build revshells image"  sudo docker build -t reverse_shell_generator "$HOME/Tools/reverse-shell-generator"
    else
        info "revshells image already built — skipping"
    fi

    # ── Combined service compose (hacktricks + revshells + nessus) ────────────
    cat > "$HOME/Tools/docker-compose.yml" << 'EOF'
services:
  hacktricks:
    image: ghcr.io/hacktricks-wiki/hacktricks-cloud/translator-image
    platform: linux/amd64
    ports:
      - "3337:3000"
    volumes:
      - ./hacktricks:/app
    command: >
      bash -c "mkdir -p ~/.ssh &&
               ssh-keyscan -H github.com >> ~/.ssh/known_hosts &&
               cd /app &&
               git config --global --add safe.directory /app &&
               git checkout master &&
               git pull &&
               MDBOOK_PREPROCESSOR__HACKTRICKS__ENV=dev mdbook serve --hostname 0.0.0.0"
    restart: unless-stopped

  revshells:
    image: reverse_shell_generator
    ports:
      - "9988:80"
    restart: unless-stopped

  nessus:
    image: tenable/nessus:latest-ubuntu
    ports:
      - "8834:8834"
    env_file:
      - .env
    restart: unless-stopped
EOF
    ok "docker-compose.yml created → ~/Tools/docker-compose.yml"

    # ── Unified launcher ──────────────────────────────────────────────────────
    cat > "$HOME/Tools/start-services.sh" << 'EOF'
#!/usr/bin/env bash
# Usage: ./start-services.sh [start|stop|status]

ACTION="${1:-start}"
TOOLS_DIR="$(cd "$(dirname "$0")" && pwd)"

case "$ACTION" in
    start)
        echo "🚀 Starting HackTricks on http://localhost:3337 ..."
        echo "🚀 Starting Reverse Shell Generator on http://localhost:9988 ..."
        echo "🚀 Starting Nessus on https://localhost:8834 ..."
        docker compose -f "$TOOLS_DIR/docker-compose.yml" up -d
        echo ""
        echo "  ✔  HackTricks   → http://localhost:3337  (allow ~5 min to build)"
        echo "  ✔  RevShells    → http://localhost:9988"
        echo "  ✔  Nessus       → https://localhost:8834  (allow ~2 min to start)"
        echo ""
        echo "  Stop with: $0 stop"
        ;;
    stop)
        echo "🛑 Stopping services..."
        docker compose -f "$TOOLS_DIR/docker-compose.yml" down
        ;;
    status)
        echo "📊 Running pentest service containers:"
        docker compose -f "$TOOLS_DIR/docker-compose.yml" ps
        ;;
    *)
        echo "Usage: $0 [start|stop|status]"
        exit 1
        ;;
esac
EOF
    chmod +x "$HOME/Tools/start-services.sh"
    ok "launcher created → ~/Tools/start-services.sh"

    mark_done "hacktricks_revshells"
fi

# =============================================================================
# 19. NESSUS — Docker Compose service + credential template + launcher
# =============================================================================
if is_done "nessus"; then
    skip_section "Nessus" "🔍"
else
    section "Nessus" "🔍"

    mkdir -p "$HOME/Tools"

    # ── Docker Compose for Nessus ─────────────────────────────────────────────
    cat > "$HOME/Tools/nessus-compose.yml" << 'EOF'
services:
  nessus:
    image: tenable/nessus:latest-ubuntu
    ports:
      - "8834:8834"
    env_file:
      - .env
    restart: unless-stopped
EOF
    ok "nessus-compose.yml created → ~/Tools/nessus-compose.yml"

    # ── .env template (skip if already populated) ─────────────────────────────
    if [ ! -f "$HOME/Tools/.env" ]; then
        cat > "$HOME/Tools/.env" << 'EOF'
# Nessus Professional credentials
# Fill these in before running: docker compose up -d
ACTIVATION_CODE=your-activation-code-here
USERNAME=admin
PASSWORD=changeme
EOF
        chmod 600 "$HOME/Tools/.env"
        ok ".env template created → ~/Tools/.env  (chmod 600)"
    else
        info ".env already exists — skipping"
    fi

    # ── Launcher script ───────────────────────────────────────────────────────
    cat > "$HOME/Tools/start-nessus.sh" << 'EOF'
#!/usr/bin/env bash
# Usage: ./start-nessus.sh [start|stop|status]

ACTION="${1:-start}"
TOOLS_DIR="$(cd "$(dirname "$0")" && pwd)"

case "$ACTION" in
    start)
        echo "Starting Nessus on https://localhost:8834 ..."
        docker compose -f "$TOOLS_DIR/nessus-compose.yml" up -d
        echo "  Nessus → https://localhost:8834  (allow ~2 min to start)"
        echo "  Stop with: $0 stop"
        ;;
    stop)
        echo "Stopping Nessus..."
        docker compose -f "$TOOLS_DIR/nessus-compose.yml" down
        ;;
    status)
        docker compose -f "$TOOLS_DIR/nessus-compose.yml" ps
        ;;
    *)
        echo "Usage: $0 [start|stop|status]"
        exit 1
        ;;
esac
EOF
    chmod +x "$HOME/Tools/start-nessus.sh"
    ok "launcher created → ~/Tools/start-nessus.sh"

    mark_done "nessus"
fi

# =============================================================================
# 20. GOOGLE CLOUD CLI — apt repo + keyring (cloud engagement tooling)
#     Sits alongside the az / aws CLIs for GCP-scoped assessments.
# =============================================================================
if is_done "gcloud"; then
    skip_section "Google Cloud CLI" "☁️"
else
    section "Google Cloud CLI" "☁️"

    if ! command -v gcloud >/dev/null 2>&1; then
        spin "install gcloud prerequisites" \
            sudo apt-get install -y -qq ca-certificates gnupg curl

        # Keyring is rewritten each run — --yes stops gpg prompting on re-run.
        spin "add google cloud GPG key" \
            bash -c 'curl -fsSL https://packages.cloud.google.com/apt/doc/apt-key.gpg \
                | sudo gpg --dearmor --yes -o /usr/share/keyrings/cloud.google.gpg'

        # tee (not tee -a) so re-runs cannot stack duplicate source lines.
        spin "add google cloud apt source" \
            bash -c 'echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] \
                https://packages.cloud.google.com/apt cloud-sdk main" \
                | sudo tee /etc/apt/sources.list.d/google-cloud-sdk.list > /dev/null'

        spin "apt update (google cloud source)"  sudo apt-get update -qq
        spin "install google-cloud-cli"  sudo apt-get install -y -qq google-cloud-cli
        ok "gcloud installed — authenticate with: gcloud auth login"
    else
        info "gcloud already installed — skipping"
    fi

    mark_done "gcloud"
fi

# =============================================================================
# 21. PYENV — per-project python versions independent of the system interpreter
#     Build deps come first; without them CPython compiles with missing
#     modules (no ssl / sqlite3 / lzma) and pip breaks in confusing ways.
# =============================================================================
if is_done "pyenv"; then
    skip_section "pyenv" "🐍"
else
    section "pyenv" "🐍"

    spin "install python build dependencies" \
        sudo apt-get install -y -qq build-essential libssl-dev zlib1g-dev libbz2-dev \
            libreadline-dev libsqlite3-dev libncursesw5-dev xz-utils tk-dev \
            libxml2-dev libxmlsec1-dev libffi-dev liblzma-dev

    if [ ! -d "$PYENV_ROOT_DIR" ]; then
        spin "install pyenv"  bash -c 'curl -fsSL https://pyenv.run | bash'
    else
        info "pyenv already present — skipping installer"
    fi

    export PYENV_ROOT="$PYENV_ROOT_DIR"
    export PATH="$PYENV_ROOT/bin:$PATH"

    if command -v pyenv >/dev/null 2>&1; then
        eval "$(pyenv init -)"

        # Written to .zshenv, not .zshrc — section 7 symlinks .zshrc from the
        # dotfiles repo and would silently clobber anything appended here.
        for rc in "$HOME/.bashrc" "$HOME/.zshenv"; do
            if [ -f "$rc" ] && ! grep -qF 'PYENV_ROOT' "$rc"; then
                cat >> "$rc" << 'EOF'

# pyenv
export PYENV_ROOT="$HOME/.pyenv"
[ -d "$PYENV_ROOT/bin" ] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
EOF
                ok "added pyenv init → $rc"
            fi
        done

        # Resolve the newest patch release for the requested minor series.
        PYENV_PY_FULL=$(pyenv install --list 2>/dev/null \
            | tr -d ' ' \
            | grep -E "^${PYENV_PY}\.[0-9]+$" \
            | tail -1)

        if [ -z "${PYENV_PY_FULL:-}" ]; then
            warn "no python ${PYENV_PY}.x available from pyenv — skipping build"
        elif pyenv versions --bare 2>/dev/null | grep -qx "$PYENV_PY_FULL"; then
            info "python ${PYENV_PY_FULL} already built — skipping"
        else
            info "building python ${PYENV_PY_FULL} from source (this takes a few minutes)"
            spin "pyenv install ${PYENV_PY_FULL}"  pyenv install -s "$PYENV_PY_FULL"
        fi
    else
        warn "pyenv not on PATH after install — skipping python build"
    fi

    mark_done "pyenv"
fi

# =============================================================================
# 22. PROWLER — cloud security posture scanning (AWS / Azure / GCP / K8s)
#     Installed via pipx against the pyenv interpreter rather than the system
#     python, so an Ubuntu python upgrade cannot orphan the venv.
# =============================================================================
if is_done "prowler"; then
    skip_section "Prowler" "🛡️"
else
    section "Prowler" "🛡️"

    export PYENV_ROOT="$PYENV_ROOT_DIR"
    export PATH="$PYENV_ROOT/bin:$HOME/.local/bin:$PATH"

    PROWLER_PY=""
    if command -v pyenv >/dev/null 2>&1; then
        # Absolute interpreter path — shims depend on an interactive shell.
        PROWLER_PY=$(find "$PYENV_ROOT/versions" -maxdepth 1 -type d \
            -name "${PYENV_PY}.*" 2>/dev/null | sort -V | tail -1)
        [ -n "$PROWLER_PY" ] && PROWLER_PY="$PROWLER_PY/bin/python"
    fi

    if [ -n "$PROWLER_PY" ] && [ -x "$PROWLER_PY" ]; then
        info "using interpreter: $PROWLER_PY"
    else
        warn "no pyenv ${PYENV_PY}.x found — falling back to system python3"
        PROWLER_PY=$(command -v python3)
    fi

    if ! command -v prowler >/dev/null 2>&1; then
        spin "pipx install prowler"  pipx install prowler --python "$PROWLER_PY"
    else
        info "prowler already installed — running upgrade instead"
        spin_soft "pipx upgrade prowler"  pipx upgrade prowler
    fi

    mark_done "prowler"
fi

# =============================================================================
# 23. CLAUDE CODE — AI coding assistant (https://claude.ai/code)
# =============================================================================
if is_done "claude_code"; then
    skip_section "Claude Code" "🤖"
else
    section "Claude Code" "🤖"
    spin_soft "install Claude Code" bash -c 'curl -fsSL https://claude.ai/install.sh | bash'

    # The installer normally appends PATH to ~/.zshrc; add a fallback in case
    # the file is a symlink that the installer skipped or wrote elsewhere.
    if ! grep -q '\.claude/bin' "${HOME}/.zshrc" 2>/dev/null; then
        printf '\n# Claude Code\nexport PATH="$HOME/.claude/bin:$PATH"\n' >> "${HOME}/.zshrc"
        ok "added ~/.claude/bin to PATH in ~/.zshrc"
    else
        info "~/.claude/bin already present in ~/.zshrc"
    fi

    mark_done "claude_code"
fi

# =============================================================================
# 24. SHARPCOLLECTION — Flangvik's compiled offensive C# binaries
#     Sparse-checkout so we only ever pull NetFramework_4.7_x86 (the repo
#     ships every framework/arch combo and is large if cloned in full).
# =============================================================================
if is_done "sharpcollection"; then
    skip_section "SharpCollection" "🗡️"
else
    section "SharpCollection" "🗡️"

    SHARPCOLLECTION_DIR="$EXEGOL_RES/bin/SharpCollection"
    SHARPCOLLECTION_SUBDIR="NetFramework_4.7_x86"

    if [ ! -d "$SHARPCOLLECTION_DIR/.git" ]; then
        spin "clone SharpCollection (sparse: ${SHARPCOLLECTION_SUBDIR})" \
            git clone --filter=blob:none --no-checkout --depth 1 \
                https://github.com/Flangvik/SharpCollection.git "$SHARPCOLLECTION_DIR"
        (
            cd "$SHARPCOLLECTION_DIR"
            git sparse-checkout init --cone
            git sparse-checkout set "$SHARPCOLLECTION_SUBDIR"
            git checkout master
        ) >>"$LOG" 2>&1
        ok "SharpCollection (${SHARPCOLLECTION_SUBDIR}) cloned → $SHARPCOLLECTION_DIR"
    else
        info "SharpCollection already present — pulling latest"
        spin_soft "update SharpCollection"  git -C "$SHARPCOLLECTION_DIR" pull --depth 1
    fi

    mark_done "sharpcollection"
fi

# =============================================================================
# 25. LIGOLO-NG — latest proxy + agent binaries, every published platform
#     Agents get pushed onto whatever the target happens to be (Windows,
#     Linux, any arch), so we pull every asset rather than guessing one.
# =============================================================================
if is_done "ligolo_ng"; then
    skip_section "Ligolo-ng" "🧦"
else
    section "Ligolo-ng" "🧦"

    LIGOLO_DIR="$EXEGOL_RES/bin/ligolo-ng"
    mkdir -p "$LIGOLO_DIR/proxy" "$LIGOLO_DIR/agent"

    info "Resolving latest ligolo-ng release..."
    LIGOLO_JSON=""
    if LIGOLO_JSON=$(_gh_api "https://api.github.com/repos/nicocha30/ligolo-ng/releases/latest"); then
        LIGOLO_VERSION=$(printf '%s' "$LIGOLO_JSON" | jq -r '.tag_name // empty')
    else
        LIGOLO_VERSION=""
    fi

    if [ -z "$LIGOLO_VERSION" ]; then
        warn "Could not resolve latest ligolo-ng release — will retry on next run (not checkpointed)"
    else
        info "ligolo-ng release: $LIGOLO_VERSION"

        LIGOLO_COUNT=0
        LIGOLO_FAILED=0
        while IFS=$'\t' read -r asset_name asset_url; do
            [ -z "$asset_name" ] && continue
            case "$asset_name" in
                *agent*) kind="agent" ;;
                *proxy*) kind="proxy" ;;
                *)       continue ;;
            esac

            # e.g. ligolo-ng_agent_0.7.5_linux_amd64.tar.gz -> linux_amd64
            plat=$(printf '%s' "$asset_name" \
                | sed -E "s/^ligolo-ng_${kind}_[^_]+_//; s/\.(tar\.gz|zip)\$//")
            dest="$LIGOLO_DIR/$kind/$plat"
            mkdir -p "$dest"

            if [ -f "$dest/.version" ] && [ "$(cat "$dest/.version")" = "$LIGOLO_VERSION" ]; then
                continue
            fi

            # A single stalled/broken asset must not take the whole run down —
            # `if spin ...` keeps the failure inside this iteration (spin
            # already prints the ✘ and captured error output) instead of
            # exiting the script under set -e; we just skip that one platform
            # and keep going.
            if spin "download $asset_name" \
                wget -q --timeout=30 --tries=3 --waitretry=3 "$asset_url" -O "/tmp/$asset_name"
            then
                case "$asset_name" in
                    *.tar.gz) tar -xzf "/tmp/$asset_name" -C "$dest" ;;
                    *.zip)    unzip -qo "/tmp/$asset_name" -d "$dest" ;;
                esac
                rm -f "/tmp/$asset_name"
                chmod +x "$dest"/agent "$dest"/proxy 2>/dev/null || true
                echo "$LIGOLO_VERSION" > "$dest/.version"
                LIGOLO_COUNT=$(( LIGOLO_COUNT + 1 ))
            else
                rm -f "/tmp/$asset_name"
                LIGOLO_FAILED=$(( LIGOLO_FAILED + 1 ))
            fi
        done < <(printf '%s' "$LIGOLO_JSON" \
            | jq -r '.assets[] | select(.name | test("^ligolo-ng_(agent|proxy)_")) | "\(.name)\t\(.browser_download_url)"')

        if [ "$LIGOLO_COUNT" -gt 0 ]; then
            ok "ligolo-ng ${LIGOLO_VERSION}: ${LIGOLO_COUNT} platform binaries → $LIGOLO_DIR/{agent,proxy}/<platform>/"
        else
            info "ligolo-ng ${LIGOLO_VERSION} — all platform binaries already up to date"
        fi

        if [ "$LIGOLO_FAILED" -gt 0 ]; then
            warn "${LIGOLO_FAILED} ligolo-ng asset(s) failed to download — re-run the script to retry just those (not checkpointed)"
        else
            mark_done "ligolo_ng"
        fi
    fi
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
printf "  ${BOLD}${YELLOW}⚠  Neovim — after first launch${RESET}\n"
printf "  ${DIM}lazy.nvim installs plugins on first open; markdown-preview.nvim runs npm install${RESET}\n"
printf "  ${DIM}and leaves a dirty yarn.lock that blocks future updates. Clear it once:${RESET}\n"
printf "  ${CYAN}→${RESET}  ${BOLD}git -C ~/.local/share/nvim/lazy/markdown-preview.nvim checkout -- app/yarn.lock${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  A note on the zsh integration${RESET}\n"
printf "  ${DIM}Exegol APPENDS my-resources/setup/zsh/zshrc to its own zshrc — it does NOT${RESET}\n"
printf "  ${DIM}replace it. If you see plugin double-load or theme weirdness, edit${RESET}\n"
printf "  ${DIM}~/.exegol/my-resources/setup/zsh/zshrc on the HOST and recreate the container.${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Per-box vars live in ${RESET}${BOLD}/workspace/.env${RESET}${DIM} — set with ${RESET}${BOLD}update_var box 10.10.10.5${RESET}\n"
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
printf "  ${BOLD}${YELLOW}⚠  Pentest Services (HackTricks, RevShells, Nessus)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Edit ${RESET}${BOLD}~/Tools/.env${RESET}${DIM} with your Nessus activation code, username, and password${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Start all:  ${RESET}${BOLD}~/Tools/start-services.sh start${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}HackTricks  → ${RESET}${BOLD}http://localhost:3337${RESET}${DIM}   (allow ~5 min to build)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}RevShells   → ${RESET}${BOLD}http://localhost:9988${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Nessus      → ${RESET}${BOLD}https://localhost:8834${RESET}${DIM}  (allow ~2 min to start)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Nessus only: ${RESET}${BOLD}~/Tools/start-nessus.sh start${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}☁️  Cloud tooling (gcloud / pyenv / prowler)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Authenticate GCP:${RESET}  ${BOLD}gcloud auth login && gcloud auth application-default login${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}pyenv needs a new shell before ${RESET}${BOLD}pyenv${RESET}${DIM} is on PATH.${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Prowler is pinned to the pyenv ${RESET}${BOLD}%s${RESET}${DIM} interpreter, not system python.${RESET}\n" "$PYENV_PY"
printf "  ${CYAN}→${RESET}  ${DIM}Run a scan:${RESET}  ${BOLD}prowler gcp${RESET}${DIM} / ${RESET}${BOLD}prowler aws${RESET}${DIM} / ${RESET}${BOLD}prowler azure${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}🗡️  SharpCollection & Ligolo-ng${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}SharpCollection (NetFramework_4.7_x86):${RESET} ${BOLD}/opt/my-resources/bin/SharpCollection/NetFramework_4.7_x86${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Ligolo-ng proxy (run inside container):${RESET} ${BOLD}/opt/my-resources/bin/ligolo-ng/proxy/<platform>/proxy${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Ligolo-ng agents (push to targets):${RESET}     ${BOLD}/opt/my-resources/bin/ligolo-ng/agent/<platform>/agent${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Both re-download only when a newer GitHub release is published.${RESET}\n"
printf "\n"
