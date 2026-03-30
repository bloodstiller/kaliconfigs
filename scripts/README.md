# bloodstiller — Kali Setup Script

Automated Kali Linux environment setup for penetration testing. Installs and configures tools, dotfiles, cloud security utilities, pentest reference services, and a working Doom Emacs environment in a single run.

---

## Table of Contents

- [Requirements](#requirements)
- [Usage](#usage)
- [What It Installs](#what-it-installs)
- [Directory Structure Created](#directory-structure-created)
- [Manual Installs](#manual-installs)
- [How It Works](#how-it-works)
  - [Checkpoint System](#checkpoint-system)
  - [Log File](#log-file)
  - [Sudo Keepalive](#sudo-keepalive)
  - [Progress & Visual Output](#progress--visual-output)
- [Pentest Services](#pentest-services)
  - [start-services.sh](#start-servicessh)
- [Modifying the Script](#modifying-the-script)
  - [Helper Function Reference](#helper-function-reference)
  - [Adding a New Section](#adding-a-new-section)
  - [Adding a Package to apt](#adding-a-package-to-apt)
  - [Adding a Go Tool](#adding-a-go-tool)
  - [Adding a pipx Tool](#adding-a-pipx-tool)
  - [Adding a Git Clone + Docker Build](#adding-a-git-clone--docker-build)
  - [Adding a Dotfile Symlink](#adding-a-dotfile-symlink)
  - [Changing a Port](#changing-a-port)
  - [Updating TOTAL\_STEPS](#updating-total_steps)
  - [Re-running a Single Section](#re-running-a-single-section)
- [Troubleshooting](#troubleshooting)

---

## Requirements

- Kali Linux (tested on the latest rolling release)
- Run as your **normal user**, not root
- Internet connection
- VMware Tools installed if you want the shared folder section to work (non-fatal if absent)

---

## Usage

```bash
# Clone your configs repo or copy the script, then:
chmod +x setup.sh
./setup.sh
```

You will be prompted for your sudo password once at the start. It is kept alive automatically for the duration of the script — you will not be prompted again.

**To re-run after a partial failure**, just run `./setup.sh` again. Sections that completed successfully will be skipped automatically.

**To start completely from scratch:**

```bash
rm -rf ~/.setup_checkpoints
./setup.sh
```

---

## What It Installs

| Step | Section | What Happens |
|------|---------|--------------|
| 1 | Directory Scaffold | Creates `~/Tools` and `~/Engagements` |
| 2 | System Packages | `apt` update/upgrade + core tool install including `pyenv` |
| 3 | Docker | Enables Docker daemon, adds user to `docker` group |
| 4 | Go Tools | Nuclei, Katana — symlinked to `/usr/local/bin` |
| 5 | Cloud Tools | CloudFox, ScoutSuite, Prowler, Roadrecon |
| 6 | Waymore | Web archive tool via pipx |
| 7 | bbot | Docker-based recon tool, symlinked to `/usr/bin/bbot` |
| 8 | PMapper | AWS IAM privilege escalation mapper (Python venv) |
| 9 | Oh My Zsh | Framework + 4 plugins (syntax highlighting, autosuggestions, fast-syntax-highlighting, autocomplete) |
| 10 | Dotfiles | Clones `bloodstiller/kaliconfigs` to `~/.dotfiles` |
| 11 | Doom Emacs | Clones and runs `doom install` interactively |
| 12 | Nerd Fonts | Iosevka + CommitMono Nerd Font variants |
| 13 | Misc Security Tools | Kerbrute, statistically-likely-usernames wordlist, tmux plugin manager |
| 14 | SSH Key | Generates `~/.ssh/id_ed25519` if not present |
| 15 | Dotfile Symlinks | Links zshrc, Doom config, tmux, Alacritty, Wordlists |
| 16 | Doom Sync & Git Config | Runs `doom sync` interactively, sets global git identity |
| 17 | HackTricks & RevShells | Clones both repos, builds revshells Docker image, creates `~/Tools/start-services.sh` |
| 18 | VMware Shared Folder | Mounts `.host:/` at `/mnt/hgfs`, updates fstab (non-fatal if not a VMware guest) |

### apt packages installed

```
emacs  eza  bat  ripgrep  git  tmux  gnupg  unzip  fonts-firacode
python3-argcomplete  atuin  flameshot  syncthing  syncthingtray
golang-go  ansifilter  docker.io  docker-buildx  docker-compose
ntpsec-ntpdate  hugo  pandoc  awscli  codelite  ruby-dev  pyenv
```

> `evil-winrm`, `netexec`, and `pipx` are already present on Kali by default and are not reinstalled.

---

## Directory Structure Created

```
~/
├── .dotfiles/              ← kaliconfigs repo (symlink source)
├── .setup_checkpoints/     ← checkpoint flags (one file per completed section)
├── .tmux/plugins/tpm/      ← tmux plugin manager
├── .local/bin/kerbrute     ← kerbrute binary
├── .ssh/id_ed25519         ← generated SSH key
├── Engagements/            ← client engagement working directory
├── Tools/
│   ├── hacktricks/         ← HackTricks wiki clone
│   ├── reverse-shell-generator/  ← revshells source + Docker image
│   └── start-services.sh   ← launcher for HackTricks & RevShells
├── Wordlists -> /usr/share/wordlists
└── VMShare -> /mnt/hgfs/VMShare   (VMware guests only)
```

---

## Manual Installs

The following tools require a manual download and cannot be automated due to licensing or account requirements. The script prints these reminders at the end of every run.

### Nessus Professional

Download the Kali/Debian `.deb` installer from:

> https://www.tenable.com/downloads/nessus?loginAttempted=true

Then install with:

```bash
sudo dpkg -i Nessus-*-debian10_amd64.deb
sudo systemctl enable nessusd --now
```

Nessus will be available at `https://localhost:8834` once the daemon starts. You will need a valid Nessus Professional licence key to activate it.

### Burp Suite Professional

Download the Linux installer from:

> https://portswigger.net/burp/releases/professional-community-2026-2-4?requestededition=professional&requestedplatform=

Then install with:

```bash
chmod +x burpsuite_pro_linux_*.sh
./burpsuite_pro_linux_*.sh
```

You will need a valid Burp Suite Professional licence key to activate it.

---

## How It Works

### Checkpoint System

Every section is wrapped in an `is_done` / `mark_done` guard:

```bash
if is_done "section_name"; then
    skip_section "Section Label" "🔧"
else
    section "Section Label" "🔧"
    # ... work ...
    mark_done "section_name"
fi
```

`mark_done` creates an empty file at `~/.setup_checkpoints/section_name`. `is_done` checks for that file. If the script is interrupted mid-section, the checkpoint for that section is **not** written, so it will re-run cleanly next time.

To force a single section to re-run without resetting everything:

```bash
rm ~/.setup_checkpoints/section_name
./setup.sh
```

Valid checkpoint names (matching the script):

| Checkpoint name | Section |
|----------------|---------|
| `scaffold` | Directory Scaffold |
| `apt` | System Packages |
| `docker` | Docker Setup |
| `go_tools` | Go Tools |
| `cloud_tools` | Cloud Tools |
| `waymore` | Waymore |
| `bbot` | bbot |
| `pmapper` | PMapper |
| `ohmyzsh` | Oh My Zsh |
| `dotfiles` | Dotfiles |
| `doom` | Doom Emacs |
| `fonts` | Nerd Fonts |
| `misc_tools` | Misc Security Tools |
| `ssh_key` | SSH Key |
| `dotfile_links` | Dotfile Symlinks |
| `doom_sync` | Doom Sync & Git |
| `hacktricks_revshells` | HackTricks & RevShells |
| `vmware` | VMware Shared Folder |

---

### Log File

Every command's output is silently captured and written to a timestamped log:

```
~/setup-YYYYMMDD-HHMMSS.log
```

The terminal only shows the spinner and pass/fail status. If a command fails, the error output is shown inline **and** written to the log. The log path is printed at the start of the run and again in the final summary.

To tail the log in another terminal while the script runs:

```bash
tail -f ~/setup-*.log
```

---

### Sudo Keepalive

The script prompts for sudo once, then starts a background loop that refreshes the credential every 50 seconds:

```bash
sudo -v
(while true; do sudo -v; sleep 50; done) &
SUDO_KEEPALIVE_PID=$!
trap 'kill "$SUDO_KEEPALIVE_PID" 2>/dev/null' EXIT INT TERM
```

The loop is killed automatically when the script exits, whether that is a clean finish, an error, or `Ctrl+C`.

---

### Progress & Visual Output

Each section prints a labelled header with elapsed time and a filled progress bar:

```
  [ 📦  System Packages  ]  +0m08s
  [████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░]  22%  step 4/18
```

Each command inside a section uses one of these output helpers:

| Function | Colour | When to use |
|----------|--------|-------------|
| `spin "label" cmd` | Cyan spinner → Green ✔ / Red ✘ | Any backgroundable command |
| `spin_soft "label" cmd` | Same, but non-fatal on failure | Optional/environmental steps |
| `ok "message"` | Green ✔ | Manual success confirmation |
| `info "message"` | Cyan → | Informational, no action taken |
| `warn "message"` | Yellow ⚠ | Non-fatal warning |

---

## Pentest Services

### start-services.sh

Located at `~/Tools/start-services.sh` after setup completes.

```bash
# Start both services
~/Tools/start-services.sh start

# Stop both services
~/Tools/start-services.sh stop

# Check running status
~/Tools/start-services.sh status
```

| Service | URL | Notes |
|---------|-----|-------|
| HackTricks | http://localhost:3337 | Allow ~5 minutes on first start to build the mdbook |
| Reverse Shell Generator | http://localhost:9988 | Available immediately |

Port 80 is intentionally left free for your own use during engagements.

To change either port, edit `start-services.sh` directly — see [Changing a Port](#changing-a-port).

---

## Modifying the Script

### Helper Function Reference

```bash
spin "label" command [args...]
```
Runs a command in the background with a Braille spinner. On success prints a green ✔. On failure prints a red ✘, dumps the command's output inline, and exits the script (because of `set -e`). All output is also written to the log.

```bash
spin_soft "label" command [args...]
```
Same as `spin` but failure is non-fatal — prints a yellow ⚠ warning and continues. Use this for steps that are environment-dependent (e.g. VMware mount, optional clones).

```bash
safe_link "/path/to/source" "/path/to/dest"
```
Creates a symlink with `sudo`. Removes the destination first if it already exists (file or broken link), preventing `ln: already exists` errors.

```bash
safe_link_user "/path/to/source" "/path/to/dest"
```
Same as `safe_link` but without `sudo` — for symlinks inside your home directory.

```bash
ok "message"     # green ✔  — use after manual steps to confirm success
info "message"   # cyan  →  — informational, no action
warn "message"   # yellow ⚠ — non-fatal notice
```

---

### Adding a New Section

This is the standard pattern. Copy it and fill in your content:

```bash
# 1. Choose a short lowercase checkpoint name with no spaces
if is_done "my_tool"; then
    skip_section "My Tool" "🔧"
else
    section "My Tool" "🔧"

    # Your install steps here
    spin "install my-tool"   some_command --with args
    spin "configure my-tool" another_command

    mark_done "my_tool"
fi
```

Then increment `TOTAL_STEPS` near the top of the script — see [Updating TOTAL_STEPS](#updating-total_steps).

The emoji in `section` and `skip_section` is purely decorative — pick anything that makes the section easy to spot while it is running.

---

### Adding a Package to apt

Find the existing apt install block and add your package to the list:

```bash
spin "install core packages"   sudo apt-get install -y -qq \
    emacs eza bat ripgrep git tmux \
    ...
    your-new-package            # ← add here
```

This does **not** require a new section or a new checkpoint — it is part of the `apt` checkpoint. If you need the package on a machine that has already passed the `apt` checkpoint, either add it separately in its own section or reset just that checkpoint:

```bash
rm ~/.setup_checkpoints/apt
./setup.sh
```

---

### Adding a Go Tool

Go tools follow this pattern. Add inside an existing section or create a new one:

```bash
spin "install mytool"   go install github.com/author/mytool@latest
safe_link "$HOME/go/bin/mytool" /usr/local/bin/mytool
```

`$HOME/go/bin` is exported onto `PATH` early in the script so `go install` and the resulting binaries are always findable regardless of login shell state.

If the tool needs `CGO_ENABLED=1` (like Katana does), wrap it:

```bash
spin "install mytool"   bash -c 'CGO_ENABLED=1 go install github.com/author/mytool@latest'
```

---

### Adding a pipx Tool

```bash
spin "install mytool"   pipx install mytool
```

If the tool installs a binary you want on the system path:

```bash
spin "install mytool"   pipx install mytool
safe_link "$HOME/.local/share/pipx/venvs/mytool/bin/mytool-binary" /usr/bin/mytool-binary
```

The pipx venv path is always `~/.local/share/pipx/venvs/<package-name>/bin/<binary>`. If you are unsure of the binary name after install, check with:

```bash
ls ~/.local/share/pipx/venvs/mytool/bin/
```

---

### Adding a Git Clone + Docker Build

Follow the pattern used for the RevShells section. Note the use of `sg docker` — this runs the build under the `docker` group without requiring a re-login, which is important since the group is added earlier in the same script run:

```bash
# Clone (idempotent — skips if already present)
if [ ! -d "$HOME/Tools/myrepo" ]; then
    spin "clone myrepo"   git clone https://github.com/author/myrepo.git "$HOME/Tools/myrepo"
else
    info "myrepo already cloned — skipping"
fi

# Build Docker image using sg docker so the group is active without re-login
spin "build myrepo image"   sg docker -c "docker build -t myrepo_image $HOME/Tools/myrepo"
```

If you want the service managed by `start-services.sh`, add `start`/`stop`/`status` entries to `~/Tools/start-services.sh` following the same `case` pattern already there.

---

### Adding a Dotfile Symlink

For symlinks inside your home directory use `safe_link_user`. Add inside the `dotfile_links` section:

```bash
safe_link_user "$HOME/.dotfiles/MyApp/myconfig.toml"  "$HOME/.config/myapp/myconfig.toml"
```

For symlinks that need to land in a system directory use `safe_link` (uses `sudo`):

```bash
safe_link "$HOME/.dotfiles/scripts/mytool" /usr/local/bin/mytool
```

Both functions handle the case where the destination already exists — they remove it first, so re-running is always safe.

---

### Changing a Port

Ports are set in `~/Tools/start-services.sh`, which is generated during the `hacktricks_revshells` section. Edit the file directly after setup:

```bash
# HackTricks: change 3337 to your preferred port
-p 3337:3000 \   # format is HOST_PORT:CONTAINER_PORT

# RevShells: change 9988 to your preferred port
-p 9988:80 \
```

If you want the change to take effect when the setup script runs (e.g. you are building a new VM), edit the `cat > "$HOME/Tools/start-services.sh"` heredoc inside `setup.sh` before running it. Then reset the checkpoint so the section re-runs:

```bash
rm ~/.setup_checkpoints/hacktricks_revshells
./setup.sh
```

---

### Updating TOTAL_STEPS

The progress bar percentage is calculated from `TOTAL_STEPS` at the top of the script. It must equal the number of `section` + `skip_section` calls that will execute — which is always every section, since `skip_section` also increments `CURRENT_STEP`.

Current value: **18**

Every time you add or remove a section, update this number:

```bash
# Near the top of setup.sh
TOTAL_STEPS=18   # ← change to 19 if you added one section, 17 if you removed one
```

Getting this wrong does not break anything — the progress bar will just hit 100% early or never quite reach it.

---

### Re-running a Single Section

To force exactly one section to re-run without touching anything else:

```bash
# Example: re-run the cloud tools section only
rm ~/.setup_checkpoints/cloud_tools
./setup.sh
```

All other sections will show `(already done — skipping)` and the target section will run normally.

---

## Troubleshooting

**Script exits immediately after sudo prompt**
Make sure you are not running as root: `whoami` should return your normal username, not `root`.

**A spinner hangs indefinitely**
The command running inside `spin` is likely waiting for interactive input. This usually means a tool's installer is prompting for something. Check the log file in another terminal:
```bash
tail -f ~/setup-*.log
```
If confirmed, the command needs a non-interactive flag or a `debconf-set-selections` pre-seed. Kill the script with `Ctrl+C`, fix the command, and re-run.

**`doom install` or `doom sync` hangs**
These run interactively (outside of `spin`) and stream their own output directly to the terminal so you can respond to any prompts. They can take 5–15 minutes on a fresh install. If they appear stuck, check that your network is up and that GitHub is reachable.

**Docker group change not taking effect after setup**
The Docker group membership is applied during the script but only takes effect in new login sessions. The revshells Docker build works around this during the script run using `sg docker`, but for your own use after setup completes you will need to log out and back in (or run `newgrp docker` in your current shell).

**Docker build fails during the revshells section**
The most common cause is the Docker daemon not being fully started yet. Try:
```bash
sudo systemctl restart docker
rm ~/.setup_checkpoints/hacktricks_revshells
./setup.sh
```

**VMware section reports "not available"**
This is expected on bare-metal installs or in non-VMware VMs. The section is marked non-fatal (`spin_soft`) and the checkpoint is still written so it will not re-run.

**Progress bar percentage looks wrong**
`TOTAL_STEPS` at the top of the script does not match the actual number of sections. Count the `section`/`skip_section` pairs and update the value — see [Updating TOTAL_STEPS](#updating-total_steps).
