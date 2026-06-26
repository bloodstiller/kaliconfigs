#!/usr/bin/env bash
# =============================================================================
#  Exegol on Ubuntu — Host bootstrap + my-resources provisioning
#  bloodstiller.com
#
#  What this script does, in plain English:
#    1. Brings a fresh Ubuntu VM up to spec: git, docker, pipx, zsh, tmux, etc.
#    2. Installs Exegol (per the official docs: pipx + argcomplete + alias).
#    3. Clones your kaliconfigs dotfiles so the *host* shell feels like home.
#    4. Wires your tmux / zsh configs into ~/.exegol/my-resources/setup/ so
#       EVERY exegol container you ever spin up inherits them automatically.
#    5. Pre-positions API testing tooling (jwt_tool, kiterunner, arjun,
#       mitmproxy2swagger) and the Hacking-APIs wordlist so containers
#       can use them without redoing the work each time.
#    6. Pulls a starting exegol image of your choice.
#
#  Re-runnable: each section checkpoints itself. Wipe ~/.exegol_setup_checkpoints
#  to redo the lot.
# =============================================================================

set -e

# ──────────────────────────────────────────────────────────────────────────────
#  Colours & styles (cribbed from your kali setup.sh so the UX matches)
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
TOTAL_STEPS=12
CURRENT_STEP=0
SCRIPT_START=$(date +%s)

LOG="$HOME/exegol-setup-$(date +%Y%m%d-%H%M%S).log"
touch "$LOG"
log() { printf "[%s] %s\n" "$(date +%H:%M:%S)" "$*" >> "$LOG"; }

# ──────────────────────────────────────────────────────────────────────────────
#  Checkpoints (same pattern as your kali setup.sh — re-run friendly)
# ──────────────────────────────────────────────────────────────────────────────
CHECKPOINT_DIR="$HOME/.exegol_setup_checkpoints"
mkdir -p "$CHECKPOINT_DIR"
is_done()   { [ -f "$CHECKPOINT_DIR/$1" ]; }
mark_done() { touch "$CHECKPOINT_DIR/$1"; log "CHECKPOINT: $1 complete"; }

# ──────────────────────────────────────────────────────────────────────────────
#  Tunables — change these if your repo or dotfile layout differs
# ──────────────────────────────────────────────────────────────────────────────
DOTFILES_REPO="https://github.com/bloodstiller/kaliconfigs.git"
DOTFILES_DIR="$HOME/.dotfiles"
EXEGOL_RES="$HOME/.exegol/my-resources"
WORDLISTS_DIR="$HOME/wordlists"

# Path inside dotfiles repo to each config file — adjust if your repo layout
# differs from your kali setup.sh references.
DF_ZSHRC="$DOTFILES_DIR/Zsh/.zshrc"
DF_ZSHENV="$DOTFILES_DIR/Zsh/.zshenv"
DF_TMUX="$DOTFILES_DIR/Tmux/.tmux.conf"

# ── Burp Suite Pro / JDK (see section 12) ─────────────────────────────────────
# Java 23 is what Greg Scharf's guide uses and is known-good with current Burp
# Pro. Bump to a newer JDK by editing both URL and JDK_DIR — keep them in sync.
JDK_VERSION="23"
JDK_DIR="jdk-23"          # directory name after extracting the tarball
JDK_URL_AMD64="https://download.java.net/java/GA/jdk23/3c5b90190c68498b986a97f276efd28a/37/GPL/openjdk-23_linux-x64_bin.tar.gz"
JDK_URL_ARM64="https://download.java.net/java/GA/jdk23/3c5b90190c68498b986a97f276efd28a/37/GPL/openjdk-23_linux-aarch64_bin.tar.gz"

# ──────────────────────────────────────────────────────────────────────────────
#  Output helpers — identical idiom to your kali setup.sh
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
fail() { printf "    ${RED}✘${RESET}  %s\n" "$1"; log "FAIL: $1"; }

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

safe_link_user() {
    local src="$1" dest="$2"
    if [ -e "$dest" ] || [ -L "$dest" ]; then rm -f "$dest"; fi
    ln -s "$src" "$dest"
    ok "linked $(basename "$src") → $dest"
}

# ──────────────────────────────────────────────────────────────────────────────
#  Pre-flight
# ──────────────────────────────────────────────────────────────────────────────
banner

if [ "$(id -u)" -eq 0 ]; then
    printf "${BOLD}${RED}  ✘  Do not run this script as root. Run as your normal user.${RESET}\n\n"
    exit 1
fi

# Make sure we're on Ubuntu/Debian — the package names assume apt
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
printf "  ${DIM}Log file: %s${RESET}\n\n" "$LOG"

# =============================================================================
# 1. SYSTEM PACKAGES
#    Minimal — we run real tooling inside Exegol containers, not on the host.
# =============================================================================
if is_done "apt"; then
    skip_section "System Update & Host Packages" "📦"
else
    section "System Update & Host Packages" "📦"
    spin "apt update"              sudo apt-get update -qq
    spin "apt upgrade"             sudo apt-get upgrade -y -qq
    spin "install host packages"   sudo apt-get install -y -qq \
        ca-certificates curl wget git unzip jq \
        python3 python3-pip python3-venv pipx python3-argcomplete \
        zsh tmux vim \
        openvpn \
        bash-completion \
        fonts-firacode
    mark_done "apt"
fi

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
        spin "install docker.io"       sudo apt-get install -y -qq docker.io docker-compose
    else
        info "docker already installed — skipping"
    fi
    spin "enable & start docker"   sudo systemctl enable docker --now
    mark_done "docker"
fi

# =============================================================================
# 3. EXEGOL — pipx install per https://docs.exegol.com/first-install
# =============================================================================
if is_done "exegol"; then
    skip_section "Exegol Wrapper" "🧪"
else
    section "Exegol Wrapper" "🧪"
    # Ensure pipx PATH is set up for this user
    spin "pipx ensurepath"         pipx ensurepath
    # Reload PATH for the rest of this script
    export PATH="$HOME/.local/bin:$PATH"

    if ! command -v exegol >/dev/null 2>&1; then
        spin "install exegol via pipx"  pipx install exegol
    else
        info "exegol already installed — running upgrade instead"
        spin_soft "pipx upgrade exegol"  pipx upgrade exegol
    fi

    # Argcomplete for zsh AND bash so tab-completion works on either shell
    ARGCOMPLETE_LINE='eval "$(register-python-argcomplete --no-defaults exegol)"'
    for rc in "$HOME/.bashrc" "$HOME/.zshrc"; do
        if [ -f "$rc" ] && ! grep -qF "register-python-argcomplete --no-defaults exegol" "$rc"; then
            echo "$ARGCOMPLETE_LINE" >> "$rc"
            ok "added exegol argcomplete → $rc"
        fi
    done

    # Sudo alias — Exegol's recommended way to run the wrapper.
    # `sudo -E` preserves the user's environment (notably HOME, so the
    # wrapper still finds ~/.exegol/my-resources/ correctly).
    EXEGOL_BIN="$HOME/.local/bin/exegol"
    ALIAS_LINE="alias exegol='sudo -E $EXEGOL_BIN'"
    for rc in "$HOME/.bashrc" "$HOME/.zshrc"; do
        if [ -f "$rc" ] && ! grep -qF "alias exegol=" "$rc"; then
            echo "$ALIAS_LINE" >> "$rc"
            ok "added exegol sudo-alias → $rc"
        fi
    done

    mark_done "exegol"
fi

# =============================================================================
# 4. DOTFILES — clone your kaliconfigs so the HOST shell feels right
#    (Container-side configs are handled in the my-resources sections below.)
# =============================================================================
if is_done "dotfiles"; then
    skip_section "Dotfiles (kaliconfigs)" "📁"
else
    section "Dotfiles (kaliconfigs)" "📁"
    if [ ! -d "$DOTFILES_DIR" ]; then
        spin "clone kaliconfigs"   git clone "$DOTFILES_REPO" "$DOTFILES_DIR"
    else
        info "dotfiles already cloned — skipping"
    fi
    mark_done "dotfiles"
fi

# =============================================================================
# 5. OH-MY-ZSH + PLUGINS (host shell)
#    Same plugin set as your kali setup.sh.
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
            spin "plugin: $name"   git clone --depth 1 "$url" "$dest"
        else
            info "plugin $name already exists — skipping"
        fi
    }
    _zsh_plugin zsh-syntax-highlighting   https://github.com/zsh-users/zsh-syntax-highlighting.git
    _zsh_plugin zsh-autosuggestions       https://github.com/zsh-users/zsh-autosuggestions
    _zsh_plugin fast-syntax-highlighting  https://github.com/zdharma-continuum/fast-syntax-highlighting.git
    _zsh_plugin fzf-tab                   https://github.com/Aloxaf/fzf-tab.git

    # tmux plugin manager
    if [ ! -d "$HOME/.tmux/plugins/tpm" ]; then
        spin "install tmux plugin manager" \
            git clone https://github.com/tmux-plugins/tpm "$HOME/.tmux/plugins/tpm"
    else
        info "tpm already installed — skipping"
    fi

    # Chsh — Ubuntu (unlike Kali) does not ship zsh as default
    if [ "$(getent passwd "$USER" | cut -d: -f7)" != "$(command -v zsh)" ]; then
        spin "chsh to zsh"  sudo chsh -s "$(command -v zsh)" "$USER"
        warn "Default shell changed to zsh — takes effect on next login."
    fi

    mark_done "ohmyzsh"
fi

# =============================================================================
# 6. HOST DOTFILE SYMLINKS
# =============================================================================
if is_done "dotfile_links"; then
    skip_section "Host Dotfile Symlinks" "🔗"
else
    section "Host Dotfile Symlinks" "🔗"
    [ -f "$DF_ZSHRC" ]  && safe_link_user "$DF_ZSHRC"  "$HOME/.zshrc"  || warn "$DF_ZSHRC not found"
    [ -f "$DF_ZSHENV" ] && safe_link_user "$DF_ZSHENV" "$HOME/.zshenv" || warn "$DF_ZSHENV not found"
    [ -f "$DF_TMUX" ]   && safe_link_user "$DF_TMUX"   "$HOME/.tmux.conf" || warn "$DF_TMUX not found"
    mark_done "dotfile_links"
fi

# =============================================================================
# 7. MY-RESOURCES SCAFFOLD
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
# 8. CONFIG FILES → my-resources/setup/
#    tmux.conf  — overwrites container's ~/.tmux.conf
#    zsh/zshrc  — APPENDED to Exegol's own zshrc (do NOT replace)
#    zsh/aliases — sourced automatically
#    vim/vimrc  — overwrites container's ~/.vimrc
# =============================================================================
if is_done "myresources_configs"; then
    skip_section "Container Configs → my-resources" "⚙️"
else
    section "Container Configs → my-resources" "⚙️"

    # ── tmux ─────────────────────────────────────────────────────────────────
    if [ -f "$DF_TMUX" ]; then
        cp "$DF_TMUX" "$EXEGOL_RES/setup/tmux/tmux.conf"
        ok "tmux.conf  →  my-resources/setup/tmux/tmux.conf"
    else
        warn "no tmux.conf in dotfiles — skipping"
    fi

    # ── zsh ──────────────────────────────────────────────────────────────────
    # IMPORTANT: Exegol APPENDS my-resources/setup/zsh/zshrc to its own zshrc.
    # Do NOT just dump your full kali .zshrc here — it'll duplicate plugin
    # loading and clobber Exegol's PATH tweaks. Instead we strip out Oh-My-Zsh
    # boilerplate and keep only your customisations.
    if [ -f "$DF_ZSHRC" ]; then
        cat > "$EXEGOL_RES/setup/zsh/zshrc" <<EOF
# ──────────────────────────────────────────────────────────────
#  bloodstiller — custom zshrc additions, appended to exegol's
#  Source of truth: $DF_ZSHRC
#  Edit there and re-run exegol-setup.sh to refresh.
# ──────────────────────────────────────────────────────────────
EOF
        # Filter: drop OMZ source/plugins/theme lines (exegol provides its own)
        grep -vE '^(source.*oh-my-zsh\.sh|ZSH=|ZSH_THEME=|plugins=\()' "$DF_ZSHRC" \
            >> "$EXEGOL_RES/setup/zsh/zshrc"
        ok "zshrc (filtered)  →  my-resources/setup/zsh/zshrc"
    fi

    # Aliases file — Exegol sources this automatically. Seed with API testing
    # shortcuts. Most tools (jwt, kiterunner, arjun, kerbrute, etc.) are
    # already in exegol's full image — these aliases just add shortcuts for
    # the ones that ARE bundled, plus shortcuts for mitmproxy2swagger which
    # we install ourselves via python3/requirements.txt.
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

# Burp Suite Pro — backgrounded so the shell stays usable. Output silenced
# (burp itself opens a GUI window via X11). Requires java-burp-setup.sh to
# have been run inside this container at least once. See:
# https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/
alias burp='nohup java -jar /opt/my-resources/bin/BurpSuitePro/burpsuite_pro.jar >/dev/null 2>&1 & disown'

# Reminders for exegol-native tools (so muscle-memory from kali works):
#   jwt           → ticarpi/jwt_tool         (NOT 'jwt_tool')
#   kiterunner    → assetnote/kiterunner     (NOT 'kr')
#   arjun         → s0md3v/Arjun
#   kerbrute      → ropnop/kerbrute
EOF
    ok "zsh aliases  →  my-resources/setup/zsh/aliases"

    # ── vim (optional, but easy win) ─────────────────────────────────────────
    if [ -f "$HOME/.vimrc" ]; then
        cp "$HOME/.vimrc" "$EXEGOL_RES/setup/vim/vimrc"
        ok "vimrc  →  my-resources/setup/vim/vimrc"
    fi

    mark_done "myresources_configs"
fi

# =============================================================================
# 9. APT + PIP customisations for each new container
#    These give every new container the python deps and apt packages we want.
# =============================================================================
if is_done "myresources_pkgs"; then
    skip_section "Container Packages (apt + pip)" "📦"
else
    section "Container Packages (apt + pip)" "📦"

    # APT packages installed in each new container at first start.
    # Exegol's full image already ships golang-go, jq, rlwrap, and basically
    # every common pentest tool. We leave the file empty by default — add
    # things here if you find specific gaps for your engagements.
    cat > "$EXEGOL_RES/setup/apt/packages.list" <<'EOF'
# Extra APT packages for every new exegol container — bloodstiller
# Exegol full/web/ad images already include most things. Add lines below
# only for tools NOT shipped in your chosen image.
EOF
    ok "apt/packages.list seeded (empty by default)"

    # PIP3 packages — only things NOT already in exegol full image.
    # Confirmed already shipped: arjun, mitmproxy. NOT shipped: mitmproxy2swagger.
    cat > "$EXEGOL_RES/setup/python3/requirements.txt" <<'EOF'
# Python packages installed in every new exegol container — bloodstiller
# Only list things NOT already in exegol full image.
mitmproxy2swagger
EOF
    ok "python3/requirements.txt seeded (mitmproxy2swagger only)"

    mark_done "myresources_pkgs"
fi

# =============================================================================
# 10. load_user_setup.sh — runs ONCE per new container at first start
#     With jwt_tool / kiterunner / kerbrute / arjun all shipped in exegol,
#     this is now minimal: nuclei templates refresh + a friendly MOTD.
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

# ── Refresh nuclei templates (nuclei ships with exegol full image) ──────────
if command -v nuclei >/dev/null 2>&1; then
    echo "[+] Updating nuclei templates..."
    nuclei -update-templates -silent || true
fi

# ── Friendly banner reminder ────────────────────────────────────────────────
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
# 11. WORDLISTS
#     SecLists is already in exegol full image at /usr/share/seclists,
#     so we skip it. Hacking-APIs is NOT — clone it once on the host.
# =============================================================================
if is_done "wordlists"; then
    skip_section "Wordlists (Hacking-APIs)" "📚"
else
    section "Wordlists (Hacking-APIs)" "📚"
    mkdir -p "$WORDLISTS_DIR"

    # Inside container, exposed at /opt/my-resources/wordlists/Hacking-APIs-main
    if [ ! -d "$EXEGOL_RES/wordlists/Hacking-APIs-main" ]; then
        cd /tmp
        spin "download Hacking-APIs" \
            wget -q https://github.com/hAPI-hacker/Hacking-APIs/archive/refs/heads/main.zip -O HackingAPIs.zip
        spin "unzip Hacking-APIs"  unzip -q HackingAPIs.zip -d "$EXEGOL_RES/wordlists/"
        rm -f HackingAPIs.zip
        cd "$HOME"
    else
        info "Hacking-APIs already present — skipping"
    fi

    # Also expose on the HOST for convenience (e.g. when prepping payloads
    # outside a container)
    if [ ! -L "$WORDLISTS_DIR/Hacking-APIs" ]; then
        safe_link_user "$EXEGOL_RES/wordlists/Hacking-APIs-main" "$WORDLISTS_DIR/Hacking-APIs"
    fi

    mark_done "wordlists"
fi

# =============================================================================
# 12. BURP SUITE PRO BOOTSTRAP
#     Strategy (cribbed from https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/):
#       — Install + activate Burp Pro ONCE on the host into my-resources/bin/.
#       — Copy ~/.java/.userPrefs/burp/prefs.xml into my-resources/bin/ so
#         that activation propagates to every new container (no extra
#         activations burned).
#       — Each new container needs a newer JDK than Exegol ships; we download
#         the tarball here and ship it via my-resources/bin/.
#       — A helper script (java-burp-setup.sh) is generated for the user to
#         run MANUALLY inside each new container (it's interactive — the
#         java alternatives picker prompts for input — and Greg's guide
#         explicitly warns against running it from exegol's auto-load path).
#
#     This section ONLY scaffolds. The Burp installer itself is login-walled
#     at portswigger.net so the user must download and run it by hand. We
#     just put the JDK tarball and the helper script in place, then print
#     ordered instructions at the end of the script.
# =============================================================================
if is_done "burp_pro"; then
    skip_section "Burp Suite Pro Bootstrap" "🕷️"
else
    section "Burp Suite Pro Bootstrap" "🕷️"

    # ── Architecture detection ───────────────────────────────────────────────
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

        # ── Download JDK tarball into my-resources/bin/ ──────────────────────
        if [ ! -f "$EXEGOL_RES/bin/$JDK_TARBALL" ]; then
            spin "download OpenJDK ${JDK_VERSION}" \
                wget -q "$JDK_URL" -O "$EXEGOL_RES/bin/$JDK_TARBALL"
        else
            info "JDK tarball already present — skipping"
        fi

        # ── Generate the per-container helper script ─────────────────────────
        # Interpolate the tarball name + JDK dir into the heredoc so the
        # script the user runs inside a container is self-contained.
        cat > "$EXEGOL_RES/bin/java-burp-setup.sh" <<EOF
#!/usr/bin/env bash
# =============================================================================
#  java-burp-setup.sh — RUN MANUALLY INSIDE EACH NEW EXEGOL CONTAINER
#  Generated by exegol-setup.sh on $(date -Iseconds)
#  Source: https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/
#
#  What it does, in order:
#    1. Extracts the bundled OpenJDK tarball into /usr/lib/jvm/
#    2. Registers the new JDK with update-alternatives
#    3. Prompts you to pick which java becomes default (INTERACTIVE)
#    4. Drops the activated prefs.xml into /root/.java/.userPrefs/burp/
#
#  Pre-reqs (done once on the HOST before any of this):
#    - Burp Pro installed at /opt/my-resources/bin/BurpSuitePro/
#    - prefs.xml copied to  /opt/my-resources/bin/prefs.xml
#
#  After this runs, launch Burp with:
#    java -jar /opt/my-resources/bin/BurpSuitePro/burpsuite_pro.jar
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
sudo update-alternatives --install /usr/bin/java java "/usr/lib/jvm/\$JDK_DIR/bin/java" 2

# 3. Interactive picker — choose the new JDK as default
sudo update-alternatives --config java

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

        # ── Heads-up if Burp/prefs not yet present (purely informational) ────
        if [ ! -d "$EXEGOL_RES/bin/BurpSuitePro" ]; then
            warn "Burp Pro not yet installed in $EXEGOL_RES/bin/BurpSuitePro/"
            warn "See the final 'Burp Pro — manual steps' block printed below."
        fi
        if [ ! -f "$EXEGOL_RES/bin/prefs.xml" ]; then
            warn "prefs.xml not yet copied to $EXEGOL_RES/bin/prefs.xml"
        fi

        mark_done "burp_pro"
    fi
fi

# =============================================================================
# DONE — pull a starter image
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
printf "  ${CYAN}→${RESET}  ${BOLD}Open a new shell${RESET}${DIM} so the exegol alias + argcomplete take effect.${RESET}\n"
printf "      ${DIM}(also picks up zsh-as-default if this was a fresh install)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}exegol is now an alias for${RESET} ${BOLD}sudo -E ~/.local/bin/exegol${RESET}\n"
printf "      ${DIM}First invocation will prompt for your sudo password.${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Pull your first image:${RESET}    ${BOLD}exegol install full${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Start a container:${RESET}        ${BOLD}exegol start test full${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}If tmux falls back to bash, start with:${RESET} ${BOLD}-e SHELL=/usr/bin/zsh${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}my-resources mounts at:${RESET}    ${BOLD}/opt/my-resources${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Inside container, type${RESET}     ${BOLD}burp${RESET}${DIM} to launch Burp Pro (after one-time setup below)${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Full log:${RESET} %s\n" "$LOG"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  A note on the zsh integration${RESET}\n"
printf "  ${DIM}Exegol APPENDS my-resources/setup/zsh/zshrc to its own zshrc — it does NOT${RESET}\n"
printf "  ${DIM}replace it. If you see plugin double-load or theme weirdness, edit${RESET}\n"
printf "  ${DIM}~/.exegol/my-resources/setup/zsh/zshrc on the HOST and recreate the${RESET}\n"
printf "  ${DIM}container — Exegol's zshrc rules; only put deltas in the my-resources copy.${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}🕷️  Burp Suite Pro — finish these MANUAL steps on the host${RESET}\n"
printf "  ${DIM}The JDK tarball and helper script are already in place. You still need:${RESET}\n"
printf "\n"
printf "  ${CYAN}1.${RESET}  Download the Burp Pro Linux installer from ${BOLD}https://portswigger.net/users/${RESET}\n"
printf "      ${DIM}(login required — license is bound to your account)${RESET}\n"
printf "  ${CYAN}2.${RESET}  ${BOLD}cd ~/.exegol/my-resources/bin/ && bash burpsuite_pro_linux_*.sh${RESET}\n"
printf "      ${DIM}When prompted for install path, point it at:${RESET}\n"
printf "      ${BOLD}~/.exegol/my-resources/bin/BurpSuitePro${RESET}\n"
printf "  ${CYAN}3.${RESET}  Launch Burp on the host once, paste your license key, complete activation${RESET}\n"
printf "      ${DIM}(this burns ONE activation — the only one you'll ever need)${RESET}\n"
printf "  ${CYAN}4.${RESET}  ${BOLD}cp ~/.java/.userPrefs/burp/prefs.xml ~/.exegol/my-resources/bin/${RESET}\n"
printf "      ${DIM}(if the path doesn't exist, try the lowercase variant .userprefs/)${RESET}\n"
printf "\n"
printf "  ${DIM}Then, INSIDE each new container (run ONCE per container):${RESET}\n"
printf "  ${CYAN}→${RESET}  ${BOLD}/opt/my-resources/bin/java-burp-setup.sh${RESET}\n"
printf "      ${DIM}It will prompt you to pick the new JDK from update-alternatives.${RESET}\n"
printf "  ${CYAN}→${RESET}  ${DIM}Launch Burp:${RESET} ${BOLD}java -jar /opt/my-resources/bin/BurpSuitePro/burpsuite_pro.jar${RESET}\n"
printf "\n"
printf "  ${BOLD}${YELLOW}⚠  Other manual / out-of-scope${RESET}\n"
printf "  ${CYAN}→${RESET}  VPN configs (.ovpn) — pass via ${BOLD}exegol start <name> full --vpn <path>${RESET}\n"
printf "  ${CYAN}→${RESET}  SSH keys (sops/age flow from kali setup.sh) — left out by design;${RESET}\n"
printf "  ${DIM}     keep secrets on the host, mount per-container with -V if needed.${RESET}\n"
printf "\n"
