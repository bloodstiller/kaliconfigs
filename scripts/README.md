# bloodstiller — Kali Setup Script

Automated Kali Linux environment setup for penetration testing. Installs and configures tools, dotfiles, cloud security utilities, pentest reference services, and a working Doom Emacs environment in a single run. SSH keys are stored encrypted in the repo and deployed automatically at setup time using [sops](https://github.com/getsops/sops) and [age](https://github.com/FiloSottile/age).

---

## Table of Contents

- [Requirements](#requirements)
- [Usage](#usage)
- [SSH Secrets Setup (One-Time)](#ssh-secrets-setup-one-time)
  - [Overview](#overview)
  - [1. Generate an age keypair](#1-generate-an-age-keypair)
  - [2. Configure sops to use your public key](#2-configure-sops-to-use-your-public-key)
  - [3. Create and encrypt your secrets file](#3-create-and-encrypt-your-secrets-file)
  - [4. Commit the encrypted file](#4-commit-the-encrypted-file)
  - [5. Store the private key in your password manager](#5-store-the-private-key-in-your-password-manager)
  - [Rotating SSH Keys](#rotating-ssh-keys)
  - [Adding a New Key to the Secrets File](#adding-a-new-key-to-the-secrets-file)
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
  - [Adding a Secret to the Encrypted Vault](#adding-a-secret-to-the-encrypted-vault)
  - [Changing a Port](#changing-a-port)
  - [Updating TOTAL\_STEPS](#updating-total_steps)
  - [Re-running a Single Section](#re-running-a-single-section)
- [Troubleshooting](#troubleshooting)

---

## Requirements

- Kali Linux (tested on the latest rolling release)
- Run as your **normal user**, not root
- Internet connection
- An age private key stored in your password manager (see [SSH Secrets Setup](#ssh-secrets-setup-one-time))
- VMware Tools installed if you want the shared folder section to work (non-fatal if absent)

---

## Usage

```bash
# Clone your configs repo or copy the script, then:
chmod +x setup.sh
./setup.sh
```

You will be prompted for your sudo password once at the start. It is kept alive automatically for the duration of the script — you will not be prompted again.

When the script reaches the **SSH Secrets** section it will pause and ask you to paste your age private key. Retrieve it from your password manager, paste it into the terminal, and press `Ctrl+D` on a blank line. Input is not echoed.

**To re-run after a partial failure**, just run `./setup.sh` again. Sections that completed successfully will be skipped automatically.

**To start completely from scratch:**

```bash
rm -rf ~/.setup_checkpoints
./setup.sh
```

---

## SSH Secrets Setup (One-Time)

This is performed **once on your trusted machine** before committing to the repo. After this, every new Kali install just needs your age private key from your password manager.

### Overview

```
age private key  ←  stored in password manager only
      ↓
sops encrypts your SSH keys  →  encrypted file committed to GitHub (safe to be public)
      ↓
setup.sh: paste age key → sops decrypts → SSH keys placed at ~/.ssh/
```

The encrypted file is safe to commit to a public repo. The only secret you manage is the age private key, which never touches the repo.

---

### 1. Generate an age keypair

```bash
# Install age if not already present
sudo apt install age -y

# Generate the keypair — output goes to keys.txt
age-keygen -o ~/.config/sops/age/keys.txt
chmod 600 ~/.config/sops/age/keys.txt
```

The output will include a line like:

```
# public key: age1xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
Public key: age1xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

Copy the public key — you need it in the next step.

---

### 2. Configure sops to use your public key

Create `.sops.yaml` at the root of your `kaliconfigs` repo:

```yaml
# .sops.yaml
creation_rules:
  - path_regex: secrets/.*\.yaml$
    age: age1xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

Replace the `age1...` value with your actual public key. Commit this file — it contains only the public key and is safe to be public.

---

### 3. Create and encrypt your secrets file

```bash
mkdir -p secrets

# Create a plaintext staging file (never committed)
cat > /tmp/ssh_secrets_plain.yaml << 'EOF'
ssh_keys:
    id_ed25519: |
        -----BEGIN OPENSSH PRIVATE KEY-----
        <paste your private key content here>
        -----END OPENSSH PRIVATE KEY-----
    id_ed25519_pub: "ssh-ed25519 AAAAC3... user@host"
    id_rsa: |
        -----BEGIN OPENSSH PRIVATE KEY-----
        <paste second key here if needed>
        -----END OPENSSH PRIVATE KEY-----
    id_rsa_pub: "ssh-rsa AAAA... user@host"
EOF

# Encrypt — sops reads .sops.yaml automatically
SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt \
  sops --encrypt /tmp/ssh_secrets_plain.yaml > secrets/ssh_keys.yaml

# Verify — values should show ENC[AES256_GCM,...] not your keys
cat secrets/ssh_keys.yaml

# Shred the plaintext staging file
shred -u /tmp/ssh_secrets_plain.yaml
```

The supported field names map directly to filenames on disk:

| YAML field | Deployed to | Permissions |
|------------|-------------|-------------|
| `id_ed25519` | `~/.ssh/id_ed25519` | `600` |
| `id_ed25519_pub` | `~/.ssh/id_ed25519.pub` | `644` |
| `id_rsa` | `~/.ssh/id_rsa` | `600` |
| `id_rsa_pub` | `~/.ssh/id_rsa.pub` | `644` |

Fields are optional — omit any you do not need.

---

### 4. Commit the encrypted file

```bash
git add .sops.yaml secrets/ssh_keys.yaml
git commit -m "add encrypted SSH keys"
git push
```

Do **not** add `secrets/ssh_keys_plain.yaml` or any unencrypted version. Add this to your `.gitignore` as a safeguard:

```
# .gitignore
secrets/*_plain.yaml
secrets/*_plaintext.yaml
```

---

### 5. Store the private key in your password manager

Open `~/.config/sops/age/keys.txt` and save the entire contents as a secure note in Bitwarden, 1Password, or your preferred manager. The file looks like:

```
# created: 2026-01-01T00:00:00Z
# public key: age1xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
AGE-SECRET-KEY-1XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
```

Save the whole block. When `setup.sh` asks for the age key, paste this entire block and press `Ctrl+D`.

The private key file on your trusted machine can be left in place for ongoing `sops` editing. On new machines it is written to `~/.config/sops/age/keys.txt` by the setup script and persists for future secret rotations.

---

### Rotating SSH Keys

To update the encrypted secrets file in-place — sops decrypts, opens `$EDITOR`, and re-encrypts on save:

```bash
SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt \
  sops ~/.dotfiles/secrets/ssh_keys.yaml

# Then commit the updated file
git add secrets/ssh_keys.yaml
git commit -m "rotate SSH keys"
git push
```

To re-deploy after rotating (on a machine that has already run setup):

```bash
rm ~/.setup_checkpoints/ssh_secrets
./setup.sh
```

---

### Adding a New Key to the Secrets File

Open the encrypted file for editing, add the new field, save and close:

```bash
SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt \
  sops ~/.dotfiles/secrets/ssh_keys.yaml
```

Then add a corresponding entry to the `key_map` dict inside the `deploy_ssh_keys` function in `setup.sh`:

```python
key_map = {
    'id_ed25519':        ('id_ed25519',        0o600),
    'id_ed25519_pub':    ('id_ed25519.pub',     0o644),
    'id_rsa':            ('id_rsa',             0o600),
    'id_rsa_pub':        ('id_rsa.pub',         0o644),
    'id_ecdsa':          ('id_ecdsa',           0o600),   # ← example addition
    'id_ecdsa_pub':      ('id_ecdsa.pub',       0o644),   # ← example addition
}
```

---

## What It Installs

| Step | Section | What Happens |
|------|---------|--------------|
| 1 | Directory Scaffold | Creates `~/Tools` and `~/Engagements` |
| 2 | System Packages | `apt` update/upgrade + core tool install including `age` and `python3-yaml` |
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
| 14 | SSH Secrets (sops/age) | Installs sops binary, prompts for age key, decrypts and deploys SSH keys from `secrets/ssh_keys.yaml` |
| 15 | Dotfile Symlinks | Links zshrc, Doom config, tmux, Alacritty, Wordlists |
| 16 | Doom Sync & Git Config | Runs `doom sync` interactively, sets global git identity |
| 17 | HackTricks & RevShells | Clones both repos, builds revshells Docker image, creates `~/Tools/start-services.sh` |
| 18 | VMware Shared Folder | Mounts `.host:/` at `/mnt/hgfs`, updates fstab (non-fatal if not a VMware guest) |

### apt packages installed

```
age  python3-yaml
emacs  eza  bat  ripgrep  git  tmux  gnupg  unzip  fonts-firacode
python3-argcomplete  atuin  flameshot  syncthing  syncthingtray
golang-go  ansifilter  docker.io  docker-buildx  docker-compose
ntpsec-ntpdate  hugo  pandoc  awscli  codelite  ruby-dev  pyenv  jq  alacritty  seclists
```

> `evil-winrm`, `netexec`, and `pipx` are already present on Kali by default and are not reinstalled.

---

## Directory Structure Created

```
~/
├── .config/sops/age/keys.txt   ← age private key (chmod 600, written at setup time)
├── .dotfiles/                  ← kaliconfigs repo (symlink source)
│   ├── secrets/
│   │   └── ssh_keys.yaml       ← sops-encrypted SSH keys (committed to repo)
│   └── .sops.yaml              ← sops config with age public key
├── .setup_checkpoints/         ← checkpoint flags (one file per completed section)
├── .tmux/plugins/tpm/          ← tmux plugin manager
├── .local/bin/kerbrute         ← kerbrute binary
├── .ssh/
│   ├── id_ed25519              ← deployed from encrypted vault (chmod 600)
│   ├── id_ed25519.pub          ← deployed from encrypted vault (chmod 644)
│   ├── id_rsa                  ← deployed from encrypted vault (chmod 600, if present)
│   └── id_rsa.pub              ← deployed from encrypted vault (chmod 644, if present)
├── Engagements/                ← client engagement working directory
├── Tools/
│   ├── hacktricks/             ← HackTricks wiki clone
│   ├── reverse-shell-generator/  ← revshells source + Docker image
│   ├── docker-compose.yml      ← service definitions
│   ├── .env                    ← Nessus credentials (chmod 600, fill in before use)
│   └── start-services.sh       ← launcher for HackTricks, RevShells & Nessus
├── Wordlists -> /usr/share/wordlists
└── VMShare -> /mnt/hgfs/VMShare   (VMware guests only)
```

---

## Manual Installs

The following tool requires a manual download and cannot be automated due to licensing requirements. The script prints this reminder at the end of every run.

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
| `ssh_secrets` | SSH Secrets (sops/age) |
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
  [ 🔐  SSH Secrets (sops/age)  ]  +4m12s
  [████████████████████░░░░░░░░░░░░░░░░░░░░]  77%  step 14/18
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
# Start all services
~/Tools/start-services.sh start

# Stop all services
~/Tools/start-services.sh stop

# Check running status
~/Tools/start-services.sh status
```

| Service | URL | Notes |
|---------|-----|-------|
| HackTricks | http://localhost:3337 | Allow ~5 minutes on first start to build the mdbook |
| Reverse Shell Generator | http://localhost:9988 | Available immediately |
| Nessus | https://localhost:8834 | Docker image (`tenable/nessus`); requires `.env` filled in before first start; allow ~2 min |

Nessus runs as a Docker container — no manual `.deb` install required. Before starting services for the first time, edit `~/Tools/.env` with your Nessus credentials and activation code:

```bash
# ~/Tools/.env
ACTIVATION_CODE=your-activation-code-here
USERNAME=admin
PASSWORD=changeme
```

On first start Nessus will pull the image, initialise the database, and apply the activation code automatically. Subsequent starts are instant.

Port 80 is intentionally left free for your own use during engagements.

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

Copy this pattern and fill in your content:

```bash
# 1. Choose a short lowercase checkpoint name with no spaces
if is_done "my_tool"; then
    skip_section "My Tool" "🔧"
else
    section "My Tool" "🔧"

    # Your install steps here
    spin "install my-tool"   some_command --with args
    spin "configure my-tool" another_command
    ok "my-tool configured"

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
    age python3-yaml \
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

### Adding a Secret to the Encrypted Vault

To add a new secret (e.g. an API key or additional SSH key), open the encrypted file for in-place editing — sops decrypts it, opens `$EDITOR`, and re-encrypts on save:

```bash
SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt \
  sops ~/.dotfiles/secrets/ssh_keys.yaml
```

Add your new field under `ssh_keys:` (or a new top-level key if adding a different secret type):

```yaml
ssh_keys:
    id_ed25519: |
        ...existing key...
    id_ecdsa: |                  # ← new field
        -----BEGIN OPENSSH PRIVATE KEY-----
        ...
        -----END OPENSSH PRIVATE KEY-----
    id_ecdsa_pub: "ecdsa-sha2-nistp256 AAAA..."
```

Then add the corresponding entry to the `key_map` dict inside `deploy_ssh_keys` in `setup.sh`:

```python
key_map = {
    'id_ed25519':     ('id_ed25519',     0o600),
    'id_ed25519_pub': ('id_ed25519.pub', 0o644),
    'id_ecdsa':       ('id_ecdsa',       0o600),   # ← add this
    'id_ecdsa_pub':   ('id_ecdsa.pub',   0o644),   # ← and this
}
```

Commit the updated encrypted file:

```bash
git add secrets/ssh_keys.yaml
git commit -m "add ecdsa key to vault"
git push
```

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

Ports are set in the `docker-compose.yml` heredoc inside the `hacktricks_revshells` section of `setup.sh`. The format is `HOST_PORT:CONTAINER_PORT`:

```yaml
services:
  hacktricks:
    ports:
      - "3337:3000"    # change 3337 to your preferred host port

  revshells:
    ports:
      - "9988:80"      # change 9988 to your preferred host port

  nessus:
    ports:
      - "8834:8834"    # change the first 8834 to your preferred host port
```

After editing `setup.sh`, reset the checkpoint so the section re-runs and regenerates the compose file:

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
# Example: re-run the SSH secrets section only (e.g. after rotating keys)
rm ~/.setup_checkpoints/ssh_secrets
./setup.sh
```

All other sections will show `(already done — skipping)` and the target section will run normally.

---

## Troubleshooting

**Script exits immediately after sudo prompt**
Make sure you are not running as root: `whoami` should return your normal username, not `root`.

**Age key prompt: input appears on screen**
The `stty -echo` call that silences input requires an interactive terminal. If you see characters echoed, your terminal emulator is overriding `stty`. Switch to a standard terminal (Alacritty, xterm) and re-run. The key is still accepted correctly — it just should not be visible.

**"Input does not look like a valid age private key"**
The validator checks for a line starting with `AGE-SECRET-KEY-`. Make sure you are pasting the entire key file including the `AGE-SECRET-KEY-1...` line, not just the public key or a partial paste. Retrieve the full contents of `keys.txt` from your password manager.

**"Secrets file not found at ~/.dotfiles/secrets/ssh_keys.yaml"**
The dotfiles clone happened earlier in the same run. Either the `secrets/ssh_keys.yaml` file has not been committed to the repo yet (see [SSH Secrets Setup](#ssh-secrets-setup-one-time)), or the clone failed. Check the log and run `git -C ~/.dotfiles log --oneline -5` to confirm the file is present.

**sops decryption fails**
The most common causes are: wrong age key pasted, or the public key in `.sops.yaml` does not match the private key you provided. Verify with:

```bash
SOPS_AGE_KEY_FILE=~/.config/sops/age/keys.txt \
  sops --decrypt ~/.dotfiles/secrets/ssh_keys.yaml
```

If it prints your keys in plaintext, the key is correct. If it errors, regenerate the encrypted file with the correct keypair (see [SSH Secrets Setup](#ssh-secrets-setup-one-time)).

**A spinner hangs indefinitely**
The command running inside `spin` is likely waiting for interactive input. Check the log file in another terminal:

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
