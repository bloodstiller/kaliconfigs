#!/usr/bin/env bash

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
BG_GREEN="${ESC}[42m"
BG_BLUE="${ESC}[44m"

# ══════════════════════════════════════════════════════════════════════════════
#  STEP TRACKING
# ══════════════════════════════════════════════════════════════════════════════
TOTAL_STEPS=18
CURRENT_STEP=0
SCRIPT_START=$(date +%s)

# ══════════════════════════════════════════════════════════════════════════════
#  LOG FILE — all command output written here, terminal stays clean
# ══════════════════════════════════════════════════════════════════════════════
LOG="$HOME/setup-$(date +%Y%m%d-%H%M%S).log"
touch "$LOG"
log() { printf "[%s] %s\n" "$(date +%H:%M:%S)" "$*" >> "$LOG"; }

# ══════════════════════════════════════════════════════════════════════════════
#  CHECKPOINT SYSTEM
#  Each section marks itself done on completion.
#  Re-running the script skips already-completed sections automatically.
#  To start completely fresh: rm -rf ~/.setup_checkpoints
# ══════════════════════════════════════════════════════════════════════════════
CHECKPOINT_DIR="$HOME/.setup_checkpoints"
mkdir -p "$CHECKPOINT_DIR"
is_done()   { [ -f "$CHECKPOINT_DIR/$1" ]; }
mark_done() { touch "$CHECKPOINT_DIR/$1"; log "CHECKPOINT: $1 complete"; }

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
        cat "$tmpout" | sed 's/^/  /'
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

safe_link() {
    local src="$1" dest="$2"
    if [ -e "$dest" ] || [ -L "$dest" ]; then sudo rm -f "$dest"; fi
    sudo ln -s "$src" "$dest"
    ok "linked $(basename "$src") → $dest"
}

safe_link_user() {
    local src="$1" dest="$2"
    if [ -e "$dest" ] || [ -L "$dest" ]; then rm -f "$dest"; fi
    ln -s "$src" "$dest"
    ok "linked $(basename "$src") → $dest"
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

# ══════════════════════════════════════════════════════════════════════════════
#  SOPS / AGE HELPERS
#  Called from the SSH Secrets section further below.
# ══════════════════════════════════════════════════════════════════════════════

AGE_KEY_FILE="$HOME/.config/sops/age/keys.txt"

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
    printf "\n"   # restore cursor position after silent input

    # Basic sanity check before writing anything to disk
    if ! printf '%s' "$key_content" | grep -q "^AGE-SECRET-KEY-"; then
        printf "    ${RED}✘${RESET}  Input does not look like a valid age private key — aborting.\n"
        log "FAIL: age key validation — missing AGE-SECRET-KEY- prefix"
        return 1
    fi

    # Create the file with correct permissions BEFORE writing content
    # so there is no window where the key exists world-readable
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
        log "WARN: secrets file missing: $secrets_file"
        return 1
    fi

    if [ ! -f "$AGE_KEY_FILE" ]; then
        warn "Age key not found at $AGE_KEY_FILE — skipping SSH key deployment"
        log "WARN: age key missing"
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

    # Use python3 + yaml to extract and write each key.
    # The key map defines: yaml_field → (filename, octal_permissions)
    python3 - "$HOME/.ssh" "$decrypted" <<'PYEOF'
import sys, os, stat, yaml

ssh_dir    = sys.argv[1]
secrets    = yaml.safe_load(sys.argv[2])
keys       = secrets.get('ssh_keys', {})

key_map = {
    'kali_vet_ed25519':     ('kali_vet_ed25519',     0o600),
    'kali_vet_ed25519.pub': ('kali_vet_ed25519.pub', 0o644),
    'sapphireKey_ed25519':  ('sapphireKey_ed25519',  0o600),
    'sapphireKey_ed25519.pub': ('sapphireKey_ed25519.pub', 0o644),
    'work_kali_ed25519':    ('work_kali_ed25519',     0o600),
    'work_kali_ed25519.pub':    ('work_kali_ed25519.pub', 0o644),
    'ssh_config':  ('config', 0o600),
}

deployed = 0
for field, (filename, perms) in key_map.items():
    value = keys.get(field, '').strip()
    if not value:
        continue
    path = os.path.join(ssh_dir, filename)
    # Write with restrictive permissions from the start
    fd = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, perms)
    with os.fdopen(fd, 'w') as f:
        f.write(value + '\n')
    os.chmod(path, perms)   # enforce even if file pre-existed
    print(f"    \033[32m✔\033[0m  Deployed {filename}")
    deployed += 1

if deployed == 0:
    print("    \033[33m⚠\033[0m  No SSH keys found in secrets file — check field names")
    sys.exit(1)
PYEOF

    ok "SSH keys deployed → ~/.ssh/"
    log "SSH keys deployed from $secrets_file"
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

trap 'kill "$SUDO_KEEPALIVE_PID" 2>/dev/null; printf "\n${RED}  ✘  Script interrupted. Log: '"$LOG"'\n${RESET}"' EXIT INT TERM

export DEBIAN_FRONTEND=noninteractive
echo "postfix postfix/main_mailer_type select No configuration" | sudo debconf-set-selections >/dev/null 2>&1

printf "  ${DIM}Log file: %s${RESET}\n\n" "$LOG"

# ══════════════════════════════════════════════════════════════════════════════

if is_done "scaffold"; then
    skip_section "Directory Scaffold" "📂"
else
    section "Directory Scaffold" "📂"
    mkdir -p "$HOME/Tools" "$HOME/Engagements"
    ok "created ~/Tools"
    ok "created ~/Engagements"
    mark_done "scaffold"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "apt"; then
    skip_section "System Update & Core Packages" "📦"
else
    section "System Update & Core Packages" "📦"
    spin "apt update & upgrade"    sudo apt-get update -qq
    spin "install core packages"   sudo apt-get install -y -qq \
        age \
        emacs eza bat ripgrep git tmux gnupg unzip fonts-firacode \
        pkg-config libfuse3-dev python3-dev python3-yaml \
        python3-argcomplete atuin flameshot syncthing syncthingtray \
        golang-go ansifilter docker.io docker-buildx docker-compose \
        ntpsec-ntpdate hugo pandoc awscli codelite ruby-dev pyenv jq tmuxinator \
        alacritty seclists rlwrap azure-cli obsidian jwt
    mark_done "apt"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "docker"; then
    skip_section "Docker Setup" "🐳"
else
    section "Docker Setup" "🐳"
    spin "enable & start Docker"   sudo systemctl enable docker --now
    spin "add $USER to docker group" sudo usermod -aG docker "$USER"
    mark_done "docker"
fi

# ══════════════════════════════════════════════════════════════════════════════
# Export Go path so all go install commands work regardless of login shell state
export PATH="$PATH:$HOME/go/bin"

if is_done "go_tools"; then
    skip_section "Go Tools — Nuclei & Katana" "⚡"
else
    section "Go Tools — Nuclei & Katana" "⚡"
    spin "install nuclei"          go install -v github.com/projectdiscovery/nuclei/v3/cmd/nuclei@latest
    safe_link "$HOME/go/bin/nuclei" /usr/local/bin/nuclei
    spin "update nuclei templates" nuclei -update-templates

    spin "install katana"          bash -c 'CGO_ENABLED=1 go install github.com/projectdiscovery/katana/cmd/katana@latest'
    safe_link "$HOME/go/bin/katana" /usr/local/bin/katana
    mark_done "go_tools"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "cloud_tools"; then
    skip_section "Cloud Tools" "☁️"
else
    section "Cloud Tools" "☁️"
    spin "install cloudfox"        go install github.com/BishopFox/cloudfox@latest
    safe_link "$HOME/go/bin/cloudfox" /usr/bin/cloudfox

    spin "install scoutsuite"      pipx install scoutsuite
    safe_link "$HOME/.local/share/pipx/venvs/scoutsuite/bin/scout" /usr/bin/scout

    spin "pyenv install 3.12"      pyenv install 3.12
    spin "install prowler" \
        bash -c 'export PYENV_VERSION=3.12; pipx install prowler --python "$(pyenv which python)"'

    spin "install roadrecon"       pipx install roadrecon
    mark_done "cloud_tools"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "waymore"; then
    skip_section "Waymore" "🌊"
else
    section "Waymore" "🌊"
    spin "install waymore"         pipx install git+https://github.com/xnl-h4ck3r/waymore.git
    mark_done "waymore"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "bbot"; then
    skip_section "bbot" "🤖"
else
    section "bbot" "🤖"
    if [ ! -d "$HOME/bbot" ]; then
        spin "clone bbot"          git clone https://github.com/blacklanternsecurity/bbot "$HOME/bbot"
    else
        info "bbot already cloned — skipping clone"
    fi
    if [ -f "$HOME/bbot/bbot-docker.sh" ]; then
        chmod +x "$HOME/bbot/bbot-docker.sh"
        safe_link "$HOME/bbot/bbot-docker.sh" /usr/bin/bbot
    else
        warn "bbot-docker.sh not found — check $HOME/bbot manually"
    fi
    mark_done "bbot"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "pmapper"; then
    skip_section "PMapper" "📐"
else
    section "PMapper" "📐"
    if [ ! -d "$HOME/pmapper" ]; then
        spin "clone PMapper"       git clone https://github.com/nccgroup/PMapper.git "$HOME/pmapper"
    else
        info "PMapper already cloned — skipping clone"
    fi
    cd "$HOME/pmapper"
    spin "create venv"             python3 -m venv venv
    spin "pip install"             bash -c '. venv/bin/activate && pip install . -q && deactivate'
    cd "$HOME"
    mark_done "pmapper"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "ohmyzsh"; then
    skip_section "Oh My Zsh & Plugins" "🐚"
else
    section "Oh My Zsh & Plugins" "🐚"
    if [ ! -d "$HOME/.oh-my-zsh" ]; then
        spin "install oh-my-zsh" \
            bash -c 'RUNZSH=no CHSH=no sh -c "$(wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)"'
    else
        info "oh-my-zsh already installed — skipping"
    fi

    ZSH_CUSTOM="${ZSH_CUSTOM:-$HOME/.oh-my-zsh/custom}"
    mkdir -p "$ZSH_CUSTOM/plugins"

    _zsh_plugin() {
        local name="$1" url="$2" dest="$ZSH_CUSTOM/plugins/$1"
        if [ ! -d "$dest" ]; then
            spin "plugin: $name"   git clone --depth 1 "$url" "$dest"
        else
            info "plugin $name already exists — skipping"
        fi
    }
    _zsh_plugin zsh-syntax-highlighting   https://github.com/zsh-users/zsh-syntax-highlighting.git
    _zsh_plugin zsh-autosuggestions       https://github.com/zsh-users/zsh-autosuggestions
    _zsh_plugin fast-syntax-highlighting  https://github.com/zdharma-continuum/fast-syntax-highlighting.git
    _zsh_plugin zsh-autocomplete          https://github.com/marlonrichert/zsh-autocomplete.git
    # Kali ships with zsh as default — no chsh needed
    mark_done "ohmyzsh"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "dotfiles"; then
    skip_section "Dotfiles" "📁"
else
    section "Dotfiles" "📁"
    if [ ! -d "$HOME/.dotfiles" ]; then
        spin "clone kaliconfigs"   git clone https://github.com/bloodstiller/kaliconfigs.git "$HOME/.dotfiles"
    else
        info "dotfiles already cloned — skipping"
    fi
    mark_done "dotfiles"
fi

# ══════════════════════════════════════════════════════════════════════════════

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

# ══════════════════════════════════════════════════════════════════════════════

if is_done "fonts"; then
    skip_section "Nerd Fonts" "🔤"
else
    section "Nerd Fonts" "🔤"
    mkdir -p "$HOME/.local/share/fonts/nerd-fonts"
    cd /tmp
    spin "download Iosevka"        wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/Iosevka.zip
    spin "download CommitMono"     wget -q https://github.com/ryanoasis/nerd-fonts/releases/download/v3.4.0/CommitMono.zip
    spin "unzip Iosevka"           unzip -q Iosevka.zip    -d "$HOME/.local/share/fonts/nerd-fonts/Iosevka"
    spin "unzip CommitMono"        unzip -q CommitMono.zip -d "$HOME/.local/share/fonts/nerd-fonts/CommitMono"
    rm -f Iosevka.zip CommitMono.zip
    spin "refresh font cache"      fc-cache -fv
    cd "$HOME"
    mark_done "fonts"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "misc_tools"; then
    skip_section "Misc Security Tools" "🔧"
else
    section "Misc Security Tools" "🔧"

    mkdir -p "$HOME/.local/bin"

    # Kerbrute
    spin "download kerbrute" \
        wget -q https://github.com/ropnop/kerbrute/releases/download/v1.0.3/kerbrute_linux_amd64 \
             -O "$HOME/.local/bin/kerbrute"
    chmod +x "$HOME/.local/bin/kerbrute"
    ok "kerbrute installed → ~/.local/bin/kerbrute"

    # NFS Security Tooling
    spin "install nfs-security-tooling" \
        pipx install git+https://github.com/hvs-consulting/nfs-security-tooling.git

    # Statistically likely usernames wordlist
    if [ ! -d /usr/share/wordlists/statistically-likely-usernames ]; then
        spin "clone statistically-likely-usernames" \
            sudo git clone https://github.com/insidetrust/statistically-likely-usernames.git \
                /usr/share/wordlists/statistically-likely-usernames
    else
        info "statistically-likely-usernames already present — skipping"
    fi

    # tmux plugin manager
    if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
        spin "install tmux plugin manager" \
            git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
    else
        info "tpm already installed — skipping"
    fi

    mark_done "misc_tools"
fi

# ══════════════════════════════════════════════════════════════════════════════
#  SSH SECRETS — install sops, prompt for age key, decrypt & deploy SSH keys
#
#  Prerequisites (one-time setup on your trusted machine):
#    1.  age-keygen -o ~/.config/sops/age/keys.txt
#    2.  Add the public key to .sops.yaml at the root of kaliconfigs
#    3.  sops --encrypt secrets/ssh_keys_plain.yaml > secrets/ssh_keys.yaml
#    4.  Commit secrets/ssh_keys.yaml — safe to be public
#    5.  Store the private key (keys.txt) in your password manager
#
#  The secrets file must live at:
#    ~/.dotfiles/secrets/ssh_keys.yaml   (i.e. kaliconfigs/secrets/ssh_keys.yaml)
#
#  Expected structure inside the decrypted YAML:
#    ssh_keys:
#        id_ed25519: |
#            -----BEGIN OPENSSH PRIVATE KEY-----
#            ...
#            -----END OPENSSH PRIVATE KEY-----
#        id_ed25519_pub: "ssh-ed25519 AAAA... user@host"
#        id_rsa: |          # optional
#            ...
#        id_rsa_pub: "..."  # optional
# ══════════════════════════════════════════════════════════════════════════════

if is_done "ssh_secrets"; then
    skip_section "SSH Secrets (sops/age)" "🔐"
else
    section "SSH Secrets (sops/age)" "🔐"

    # ── Install sops binary ───────────────────────────────────────────────────
    if ! command -v sops &>/dev/null; then
        info "Fetching latest sops release..."
        SOPS_VERSION=$(curl -s https://api.github.com/repos/getsops/sops/releases/latest \
            | grep '"tag_name"' | cut -d '"' -f4)
        spin "download sops ${SOPS_VERSION}" \
            wget -q "https://github.com/getsops/sops/releases/download/${SOPS_VERSION}/sops-${SOPS_VERSION}.linux.amd64" \
                 -O /tmp/sops-bin
        sudo install -m 755 /tmp/sops-bin /usr/local/bin/sops
        rm -f /tmp/sops-bin
        ok "sops installed → /usr/local/bin/sops"
    else
        info "sops already installed — skipping download"
    fi

    # ── Locate secrets file ───────────────────────────────────────────────────
    SECRETS_FILE="$HOME/.dotfiles/secrets/ssh_keys.yaml"

    if [ ! -f "$SECRETS_FILE" ]; then
        warn "Secrets file not found at $SECRETS_FILE"
        warn "Ensure kaliconfigs contains secrets/ssh_keys.yaml (encrypted with sops)"
        warn "Skipping SSH key deployment — add keys manually later"
    else
        # ── Prompt for age key (only if not already present) ─────────────────
        if [ -f "$AGE_KEY_FILE" ]; then
            info "Age key already present at $AGE_KEY_FILE — skipping prompt"
        else
            read_age_key_securely || {
                warn "Age key entry failed — skipping SSH key deployment"
                mark_done "ssh_secrets"
                # Jump to next section without deploying keys
            }
        fi

        # ── Decrypt and place keys ────────────────────────────────────────────
        if [ -f "$AGE_KEY_FILE" ]; then
            deploy_ssh_keys "$SECRETS_FILE"
        fi
    fi

    mark_done "ssh_secrets"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "dotfile_links"; then
    skip_section "Dotfile Symlinks" "🔗"
else
    section "Dotfile Symlinks" "🔗"
    safe_link_user "$HOME/.dotfiles/Zsh/.zshrc"                    "$HOME/.zshrc"
    safe_link_user "$HOME/.dotfiles/Zsh/.zshenv"                   "$HOME/.zshenv"
    rm -f "$HOME/.config/doom/"*.el
    for f in "$HOME/.dotfiles/Doom/"*.el; do
        safe_link_user "$f" "$HOME/.config/doom/$(basename "$f")"
    done
    safe_link_user "$HOME/.dotfiles/Tmux/.tmux.conf"               "$HOME/.tmux.conf"
    mkdir -p "$HOME/.config/alacritty"
    safe_link_user "$HOME/.dotfiles/alacritty/alacritty.toml"      "$HOME/.config/alacritty/alacritty.toml"
    safe_link_user /usr/share/wordlists                             "$HOME/Wordlists"
    mark_done "dotfile_links"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "doom_sync"; then
    skip_section "Doom Sync & Git Config" "🔄"
else
    section "Doom Sync & Git Config" "🔄"
    printf "    ${DIM}Running doom sync...${RESET}\n\n"
    "$HOME/.config/emacs/bin/doom" sync
    ok "doom sync complete"
    spin "set git user.name"       git config --global user.name  "bloodstiller"
    spin "set git user.email"      git config --global user.email "bloodstiller@bloodstiller.com"
    spin "set dotfiles remote url" git -C "$HOME/.dotfiles" remote set-url origin git@github.com:bloodstiller/kaliconfigs.git
    mark_done "doom_sync"
    mark_done "doom_sync"
fi

# ══════════════════════════════════════════════════════════════════════════════

if is_done "hacktricks_revshells"; then
    skip_section "HackTricks & RevShells" "📖"
else
    section "HackTricks & RevShells" "📖"

    # HackTricks — clone wiki into ~/Tools
    if [ ! -d "$HOME/Tools/hacktricks" ]; then
        spin "clone HackTricks wiki"   git clone https://github.com/HackTricks-wiki/hacktricks "$HOME/Tools/hacktricks"
    else
        info "HackTricks already cloned — skipping"
    fi

    # Reverse Shell Generator — clone and build Docker image into ~/Tools
    if [ ! -d "$HOME/Tools/reverse-shell-generator" ]; then
        spin "clone revshells"         git clone https://github.com/0dayCTF/reverse-shell-generator.git "$HOME/Tools/reverse-shell-generator"
    else
        info "revshells already cloned — skipping"
    fi
    spin "build revshells image"       sg docker -c "docker build -t reverse_shell_generator $HOME/Tools/reverse-shell-generator"

    # ── Service launcher script ───────────────────────────────────────────────
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

    # ── .env template for Nessus credentials ─────────────────────────────────
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
        cat > "$HOME/Tools/start-services.sh" << 'EOF'
#!/usr/bin/env bash
# ─────────────────────────────────────────────────────────
#  Pentest Services Launcher — bloodstiller
#  Usage: ./start-services.sh [start|stop|status]
# ─────────────────────────────────────────────────────────

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

# ══════════════════════════════════════════════════════════════════════════════

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
        warn "VMware share not available — skipping fstab & symlink"
    fi
    mark_done "vmware"
fi

# ══════════════════════════════════════════════════════════════════════════════
#  DONE
# ══════════════════════════════════════════════════════════════════════════════

kill "$SUDO_KEEPALIVE_PID" 2>/dev/null
trap - EXIT INT TERM

elapsed_total=$(( $(date +%s) - SCRIPT_START ))
elapsed_fmt=$(printf '%dm%02ds' $(( elapsed_total / 60 )) $(( elapsed_total % 60 )))

printf "\n"
printf "  ${BOLD}${BG_GREEN}                                                        ${RESET}\n"
printf "  ${BOLD}${BG_GREEN}   ✅  Setup complete in %-6s                         ${RESET}\n" "$elapsed_fmt"
printf "  ${BOLD}${BG_GREEN}                                                        ${RESET}\n"
printf "\n"
printf "  ${DIM}Next steps:${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Log out and back in for Docker group changes to take effect${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Press ${RESET}${BOLD}prefix + I${RESET}${DIM} inside tmux to install TPM plugins${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Run ${RESET}${BOLD}~/Tools/start-services.sh start${RESET}${DIM} to launch HackTricks & RevShells${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}HackTricks  → http://localhost:3337${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}RevShells   → http://localhost:9988${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Full log    → %s${RESET}\n" "$LOG"
printf "  ${CYAN}→${RESET}  ${DIM}cat ../PostInstall/TODO.org${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  Manual installs required:${RESET}\n"
printf "  ${CYAN}→${RESET}  ${BOLD}Burp Suite Professional${RESET}  ${DIM}https://portswigger.net/burp/releases/professional-community-2026-2-4?requestededition=professional&requestedplatform=${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  Before starting services:${RESET}\n"
printf "  ${CYAN}→${RESET}  Edit ${BOLD}~/Tools/.env${RESET}${DIM} and add your Nessus activation code, username & password${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Then run: ${RESET}${BOLD}~/Tools/start-services.sh start${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Nessus will be available at ${RESET}${BOLD}https://localhost:8834${RESET}${DIM} once started${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  SSH secrets / age key:${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Age key persists at ${RESET}${BOLD}~/.config/sops/age/keys.txt${RESET}${DIM} (chmod 600)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Rotate SSH keys: ${RESET}${BOLD}SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt sops ~/.dotfiles/secrets/ssh_keys.yaml${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Re-run SSH deploy only: ${RESET}${BOLD}rm ~/.setup_checkpoints/ssh_secrets && ./setup.sh${RESET}\n"
printf "\n"
