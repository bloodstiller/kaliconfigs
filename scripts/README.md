# exegol-setup

> One-shot Ubuntu VM bootstrap for [Exegol](https://exegol.com), the Docker-based pentest environment. Installs Exegol, mirrors your dotfiles into every container via `my-resources`, bootstraps Burp Suite Pro across containers without burning extra licence activations, and stands up host-local pentest services (HackTricks, RevShells, Nessus).

Companion script to the Kali workflow in [`bloodstiller/kaliconfigs`](https://github.com/bloodstiller/kaliconfigs). Built for the use case "I'm on a Windows employer laptop, I run an Ubuntu VM, and I want my Kali muscle-memory to follow me into Exegol containers."

## What it does

26 checkpointed sections. Each one is re-runnable; completion is tracked as marker files in `~/.exegol_setup_checkpoints/`. Re-running the script only does the sections that haven't completed yet — see [Re-running sections](#re-running-sections) for targeting one section directly.

| § | Step | Touches |
|---|---|---|
| 1 | System Update & Host Packages | apt update/upgrade; `age`, `git`, `curl`, `wget`, `jq`, `python3`/`pipx`, `zsh`, `tmux`, `vim`, `eza`, `atuin`, `bat`, `fd-find`, `fzf`, `keepassxc`, `emacs`, `btop`, `gnupg`, `openvpn`, Nerd Font base, `flameshot`, `syncthing`, `hugo`, `pandoc`, Alacritty, Kitty, `luarocks`, Node/npm; Neovim via snap; creates `~/Tools` and `~/Engagements` (always runs — no checkpoint guard) |
| 2 | Obsidian | Latest Obsidian `.deb` resolved via GitHub API and installed; verifies against the release's `latest-linux.yml` sha512 manifest when one is published (Obsidian doesn't always publish one — installs unverified with a loud warning if so) |
| 3 | Docker Engine | Installs `docker-ce` via the official Docker apt repo; `systemctl enable` (no docker-group membership — see below). Install-once; relies on `apt upgrade` for updates |
| 4 | Exegol Wrapper | `pipx install exegol`, argcomplete for zsh + bash, `sudo -E` alias. `pipx upgrade exegol` then runs on **every** invocation of the script (not just first install) |
| 5 | Dotfiles | Clones `kaliconfigs` to `~/.dotfiles` (stall-protected clone; not auto-updated on rerun — it's your own working copy) |
| 6 | Oh My Zsh & Plugins | OMZ + `zsh-syntax-highlighting`, `zsh-autosuggestions`, `fast-syntax-highlighting`, `fzf-tab`; tmux plugin manager (tpm); `chsh` to zsh. Plugins and tpm fetch+reset to latest whenever this section is deliberately re-run |
| 7 | Doom Emacs | Clones `doomemacs`, runs `doom install`; fetches+resets to latest on a deliberate re-run |
| 8 | Host Dotfile Symlinks | Links `~/.zshrc`, `~/.zshenv`, `~/.tmux.conf`; Doom `.el` files; Alacritty + Kitty configs; `~/.config/nvim` |
| 9 | my-resources Scaffold | Creates full `~/.exegol/my-resources/` tree (`bin/`, `setup/zsh`, `tmux`, `nvim`, `vim`, `apt`, `python3`, `firefox`, `arsenal-cheats`, `wordlists`) |
| 10 | Container Configs | Copies `tmux.conf`, `nvim/` config, zsh `aliases`, `vimrc` into `my-resources/setup/` |
| 11 | Container Packages | Seeds `apt/packages.list` and `python3/requirements.txt` (`mitmproxy2swagger`) |
| 12 | load_user_setup.sh | Generates per-container first-run script: nuclei template refresh, Hacking-APIs symlink, nvim config symlink, `goclone` install, MOTD |
| 13 | Wordlists | Clones Hacking-APIs to `my-resources/wordlists/`; symlinks to `~/wordlists/`; fetch+reset on rerun |
| 14 | Burp Suite Pro Bootstrap | Resolves + downloads the latest Eclipse Temurin JDK (LTS pinned via `JDK_FEATURE`) via the Adoptium API, sha256-verified against the checksum Adoptium returns alongside the URL; generates `java-burp-setup.sh` |
| 15 | Nerd Fonts | Downloads Iosevka, CommitMono, UbuntuMono from `ryanoasis/nerd-fonts`, verified against the release's `SHA-256.txt` when published; runs `fc-cache` |
| 16 | SSH Secrets | Installs `sops` (sha256-verified against the release's checksums manifest); prompts for age private key (no echo); decrypts `secrets/ssh_keys.yaml` and deploys keys to `~/.ssh/` with correct permissions — the decrypted material is written to a private, trap-cleaned scratch file rather than passed as a process argument |
| 17 | Doom Sync & Git Config | `doom sync`; sets `git config user.name/email`; switches dotfiles remote to SSH URL |
| 18 | VMware Shared Folder | Mounts `/mnt/hgfs` via `vmhgfs-fuse`; adds fstab entry; symlinks `~/Pentest` (non-fatal — safe to skip on bare metal) |
| 19 | HackTricks & RevShells | Clones HackTricks wiki + reverse-shell-generator (fetch+reset on rerun); builds Docker image; creates `docker-compose.yml` + `start-services.sh` launcher |
| 20 | Nessus | Docker Compose service for `tenable/nessus`; local `.env` credential template (`chmod 600`, gitignored — kept out of the sops vault deliberately, see below); `start-nessus.sh` launcher |
| 21 | Google Cloud CLI | Installs `google-cloud-cli` via the official apt repo/keyring |
| 22 | pyenv | Builds Python build-deps, installs pyenv, builds the pinned `PYENV_PY` interpreter from source |
| 23 | Prowler | `pipx install prowler` against the pyenv interpreter (falls back to system python3); upgrades on rerun if already installed |
| 24 | Claude Code | Installs Claude Code via the official installer; ensures `~/.claude/bin` on PATH |
| 25 | SharpCollection | Sparse-checkout of `NetFramework_4.7_x86` from Flangvik's SharpCollection; `fetch --depth 1 && reset --hard` on rerun (checkpoint only marks done on success) |
| 26 | Ligolo-ng | Latest proxy + agent binaries for every published platform, sha256-verified against the release's checksums manifest when published; per-platform `.version` stamps mean only stale platforms re-download on a deliberate rerun |

At the end of a run, the same closing instructions printed to the terminal are also written to `~/Tools/NEXT_STEPS.md`, and resolved tool versions (Obsidian, JDK, sops, Nerd Fonts, ligolo-ng) are recorded in `~/Tools/INSTALLED_VERSIONS.txt`.

## What it deliberately doesn't do

Anything Exegol's `full` image already ships gets dropped — duplicating ships you nowhere except slower builds. Tools like `impacket`, `netexec`, `bloodhound-ce`, `certipy`, `nuclei`, `ffuf`, `gobuster`, `feroxbuster`, `metasploit`, `hashcat`, `hydra`, and SecLists (`/usr/share/seclists`) are already in Exegol containers — this script only adds what's genuinely missing: `mitmproxy2swagger`, the `Hacking-APIs` wordlist, Burp Pro (manually licensed, propagated via `prefs.xml`), your custom tmux/zsh/Doom configs, and a handful of host-local services (HackTricks, RevShells, Nessus) that make more sense running once on the host than inside every container.

## Prerequisites

- Fresh Ubuntu / Debian VM (22.04+)
- Regular user with `sudo` rights (the script refuses to run as root)
- Internet access
- (For Burp Pro) a PortSwigger account with a Burp Pro licence
- (For SSH Secrets) an age private key for the `kaliconfigs` sops vault, if you want SSH keys deployed automatically

If you're not on Ubuntu/Debian, the script aborts at the pre-flight check. Edit the `apt-get` calls to your package manager and remove the abort if you want to adapt it.

## Quick start

```bash
git clone https://github.com/bloodstiller/kaliconfigs.git ~/.dotfiles
bash ~/.dotfiles/scripts/exegol-setup.sh
```

When it finishes, open a new shell and:

```bash
exegol install full          # ~15 GB pull
exegol start test full       # spawn your first container
```

`exegol` is aliased to `sudo -E ~/.local/bin/exegol`, so the first invocation will prompt for sudo.

## CLI flags

```bash
./exegol-setup.sh [--only=<checkpoint>] [--force] [--list] [-h|--help]
```

- `--list` — print every valid checkpoint name and exit.
- `--only=<name>` — run only that one section; every other checkpoint is treated as already-done for this invocation. Section 1 (System Update & Host Packages) has no checkpoint guard and always runs regardless.
- `--force` — clear **all** checkpoints before running (full re-run from scratch). Combined with `--only`, clears just that one checkpoint.
- `-h` / `--help` — usage.

The progress bar isn't meaningful under `--only` (it jumps straight to that section's step number) — that's expected, not a bug.

## Burp Suite Pro setup

The script resolves and downloads the latest Eclipse Temurin JDK for the LTS pinned in `JDK_FEATURE` (currently 21) into `~/.exegol/my-resources/bin/`, verifies it against the checksum the Adoptium API returns alongside the download URL, and generates a helper script `java-burp-setup.sh`. The actual Burp install is interactive and login-walled at PortSwigger, so it stays manual.

### Once on the host

```bash
# 1. Download the installer from https://portswigger.net/users/

# 2. Install into the my-resources tree (so containers can reach it)
cd ~/.exegol/my-resources/bin/
bash burpsuite_pro_linux_*.sh        # point install path at ~/.exegol/my-resources/bin/BurpSuitePro/

# 3. Launch on host, paste licence key, activate (uses 1 activation)

# 4. Copy activation prefs into my-resources
cp ~/.java/.userPrefs/burp/prefs.xml ~/.exegol/my-resources/bin/
```

### Once per new container

Inside the container:

```bash
/opt/my-resources/bin/java-burp-setup.sh
```

This is interactive (`update-alternatives --config java` prompts for input), which is why it's not in `load_user_setup.sh`.

After that, just type `burp` (alias seeded by this script) to launch.

Approach is based on [Greg Scharf's writeup](https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/).

## Why `sudo -E` and not the docker group

Exegol's own docs warn against the docker group: a docker group member can mount `/` into a container and become root anyway, so the "convenience" of skipping `sudo` is illusory. `sudo -E` preserves `$HOME` so the wrapper still resolves `~/.exegol/my-resources/` correctly while running docker calls as root.

If you prefer the docker group anyway, edit section 3 (Docker Engine) of the script to re-add `usermod -aG docker "$USER"` and remove the alias-write block in section 4 (Exegol Wrapper).

## Secrets: SSH keys vs. Nessus credentials

SSH keys go through a full [sops](https://github.com/getsops/sops)/[age](https://github.com/FiloSottile/age) encrypted-vault flow: `secrets/ssh_keys.yaml` in `kaliconfigs` is sops-encrypted (safe to commit), decrypted at setup time with your age private key, and written straight to `~/.ssh/` with correct per-file permissions. The decrypted material never touches a process argument — it's written to a `mktemp` scratch file (0600, trap-cleaned) that a Python heredoc reads by path.

Nessus credentials (`~/Tools/.env`) stay a local plaintext template instead — deliberately, not by oversight. It's host-local, already `chmod 600`, gitignored, and never fleet-distributed the way the SSH keys are; folding it into the sops vault would raise the setup bar for no real security gain on an already permission-locked single-host file.

## Customisation

Tunables at the top of the script:

```bash
DOTFILES_REPO      # default: https://github.com/bloodstiller/kaliconfigs.git
DOTFILES_DIR       # default: $HOME/.dotfiles
EXEGOL_RES         # default: $HOME/.exegol/my-resources
WORDLISTS_DIR      # default: $HOME/wordlists
DF_ZSHRC           # default: $DOTFILES_DIR/Zsh/.zshrc
DF_ZSHENV          # default: $DOTFILES_DIR/Zsh/.zshenv
DF_TMUX            # default: $DOTFILES_DIR/Tmux/.tmux.conf
AGE_KEY_FILE       # default: $HOME/.config/sops/age/keys.txt
PYENV_ROOT_DIR     # default: $HOME/.pyenv
PYENV_PY           # default: 3.12 — interpreter series Prowler is pinned to
JDK_FEATURE        # default: 21 — JDK LTS resolved via the Adoptium API
```

`GITHUB_TOKEN` (optional, prompted for at pre-flight, never written to disk) raises the unauthenticated GitHub API rate limit from 60/hr to 5000/hr — worth setting if you're iterating on the script itself and re-running sections that hit `api.github.com` (Obsidian, Nerd Fonts, sops, ligolo-ng).

### Re-running sections

```bash
./exegol-setup.sh --only=<checkpoint_name>          # run just one section
./exegol-setup.sh --force --only=<checkpoint_name>   # force it even if already done
./exegol-setup.sh --list                             # see valid checkpoint names
```

Equivalent manual form (what `--only`/`--force` do under the hood):

```bash
rm ~/.exegol_setup_checkpoints/<checkpoint_name>
bash exegol-setup.sh
```

### Starting completely fresh

```bash
./exegol-setup.sh --force
# or: rm -rf ~/.exegol_setup_checkpoints
```

## Per-engagement container pattern

```bash
exegol start CLIENT-NAME full \
    -w "$HOME/Engagements/CLIENT-NAME" \
    --vpn "$HOME/Engagements/CLIENT-NAME/vpn.ovpn" \
    -e SHELL=/usr/bin/zsh \
    -e CLIENT=CLIENT-NAME \
    -e DOMAIN=client.local
```

The `-e SHELL=/usr/bin/zsh` is the workaround for Greg's tmux-falls-back-to-bash bug on Ubuntu hosts.

## Troubleshooting

### `exegol: command not found`

`pipx ensurepath` only takes effect in new shells. Open a new terminal or `source ~/.zshrc`.

### Burp Pro won't launch / wrong Java

Inside container: `sudo update-alternatives --config java`, pick the Temurin JDK listed. If it isn't listed, `java-burp-setup.sh` was never run — run it.

### Burp launches but asks for licence

`prefs.xml` either wasn't copied to `~/.exegol/my-resources/bin/` on the host, or wasn't applied inside the container. Run `java-burp-setup.sh` again, or manually:

```bash
mkdir -p /root/.java/.userPrefs/burp
cp /opt/my-resources/bin/prefs.xml /root/.java/.userPrefs/burp/
```

### tmux starts bash instead of zsh

Recreate the container with `-e SHELL=/usr/bin/zsh`.

### Plugin double-load / theme weirdness in container

The OMZ filter in the config-copy section didn't strip `source oh-my-zsh.sh` / `ZSH_THEME=` / `plugins=(...)` from your `.zshrc`. Check `~/.exegol/my-resources/setup/zsh/zshrc` and remove any of those lines by hand.

### A download or checksum step failed

Downloads (JDK, sops, Nerd Fonts, Obsidian, ligolo-ng) are checksum-verified where the upstream project publishes a manifest, and time out/retry rather than hanging forever. A checksum mismatch aborts that step (or, for ligolo-ng's per-platform loop, just that one platform) rather than silently continuing — re-run with `--only=<checkpoint>` once the network issue clears. A missing manifest (e.g. Obsidian doesn't reliably publish one) is a loud warning, not a failure.

### Inspecting what happened during a run

```bash
tail -f ~/exegol-setup-*.log            # live during a run — path is also printed at start/end
cat ~/Tools/NEXT_STEPS.md               # the closing instructions, saved to disk
cat ~/Tools/INSTALLED_VERSIONS.txt      # resolved tool versions from the last run
```

Logs older than 30 days are pruned automatically at the start of each run.

### Inspecting container init

```bash
zcat /var/log/exegol/load_setups.log.gz
zgrep error /var/log/exegol/load_setups.log.gz
```

### `arsenal` errors with `TIOCSTI`

Known Exegol issue. On the host:

```bash
sudo sysctl -w dev.tty.legacy_tiocsti=1
```

## Layout

```
~/.dotfiles/scripts/
├── README.md            # this file
└── exegol-setup.sh      # the bootstrap script
```

## Credits

- [Exegol](https://github.com/ThePorgs/Exegol) by ThePorgs — the framework this script provisions
- [Greg Scharf's Burp Pro guide](https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/) — the prefs.xml propagation trick
- [Greg Scharf's Exegol setup guide](https://blog.gregscharf.com/2023/04/01/exegol-hacking-framework-setup/) — early my-resources layout reference
- [Exegol docs: my-resources](https://docs.exegol.com/images/my-resources) — canonical reference for the customisation surface

## Licence

Personal use. Adapt freely.
