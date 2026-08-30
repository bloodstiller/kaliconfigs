# bloodstiller/kaliconfigs

Dotfiles and one-shot Ubuntu provisioning for [Exegol](https://exegol.com)-based pentest workflows.

One-shot Ubuntu VM bootstrap that installs Exegol, mirrors dotfiles into every container via `my-resources`, and bootstraps Burp Suite Pro across containers without burning extra licence activations. Built for "I'm on a Windows employer laptop, I run an Ubuntu VM, and I want my Kali muscle-memory to follow me into Exegol containers."

Companion to the Kali workflow in [`bloodstiller/kaliconfigs`](https://github.com/bloodstiller/kaliconfigs).

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

Re-run any single section directly instead of the whole script: `./exegol-setup.sh --only=<checkpoint>` (add `--force` to redo one that already completed; `--list` shows valid names).

For full details, Burp Pro setup, customisation tunables, secrets handling, and troubleshooting see **[scripts/README.md](scripts/README.md)**.

## Prerequisites

- Fresh Ubuntu / Debian VM (22.04+)
- Regular user with `sudo` rights
- Internet access
- (For Burp Pro section) a PortSwigger account with a Burp Pro licence

## What exegol-setup.sh does

26 checkpointed sections. Each is re-runnable; completion is tracked in `~/.exegol_setup_checkpoints/`.

| § | Step | What it does |
|---|---|---|
| 1 | System Update & Host Packages | apt update/upgrade; installs git, curl, python3/pipx, zsh, tmux, emacs, age, ripgrep, fzf, eza, atuin, bat, fd-find, Node/npm, Alacritty, Kitty, luarocks; Neovim via snap; creates `~/Tools` and `~/Engagements` |
| 2 | Obsidian | Fetches and installs the latest Obsidian `.deb` from GitHub releases, checksum-verified when a manifest is published |
| 3 | Docker Engine | Installs `docker-ce` via the official Docker apt repo; `systemctl enable` (no docker group — see `scripts/README.md`) |
| 4 | Exegol Wrapper | `pipx install exegol`, argcomplete for zsh + bash, `sudo -E` alias; `pipx upgrade exegol` runs on every invocation |
| 5 | Dotfiles | Clones `kaliconfigs` to `~/.dotfiles` |
| 6 | Oh My Zsh & Plugins | OMZ + `zsh-syntax-highlighting`, `zsh-autosuggestions`, `fast-syntax-highlighting`, `fzf-tab`; tmux plugin manager (tpm); `chsh` to zsh — plugins/tpm update to latest on a deliberate rerun |
| 7 | Doom Emacs | Clones `doomemacs`, runs `doom install`; updates to latest on a deliberate rerun |
| 8 | Host Dotfile Symlinks | Links `~/.zshrc`, `~/.zshenv`, `~/.tmux.conf`; Doom `.el` files; Alacritty + Kitty configs; `~/.config/nvim` |
| 9 | my-resources Scaffold | Creates full `~/.exegol/my-resources/` tree (`bin/`, `setup/zsh`, `tmux`, `nvim`, `vim`, `apt`, `python3`, `firefox`, `arsenal-cheats`, `wordlists`) |
| 10 | Container Configs | Copies `tmux.conf`, `nvim/` config, zsh `aliases`, `vimrc` into `my-resources/setup/` |
| 11 | Container Packages | Seeds `apt/packages.list` (`eza`) and `python3/requirements.txt` (`mitmproxy2swagger`) |
| 12 | load_user_setup.sh | Generates per-container first-run script: nuclei template refresh, Hacking-APIs symlink, nvim config symlink, `goclone` install, MOTD |
| 13 | Wordlists | Clones Hacking-APIs to `my-resources/wordlists/`; symlinks to `~/wordlists/` |
| 14 | Burp Suite Pro Bootstrap | Resolves + downloads the latest Eclipse Temurin JDK (LTS pinned) via the Adoptium API, checksum-verified; generates `java-burp-setup.sh` |
| 15 | Nerd Fonts | Downloads Iosevka, CommitMono, UbuntuMono from ryanoasis/nerd-fonts, checksum-verified when a manifest is published; runs `fc-cache` |
| 16 | SSH Secrets | Installs `sops` (checksum-verified); prompts for age private key (no echo); decrypts `secrets/ssh_keys.yaml` and deploys keys to `~/.ssh/` with correct permissions |
| 17 | Doom Sync & Git Config | `doom sync`; sets `git config user.name/email`; switches dotfiles remote to SSH URL |
| 18 | VMware Shared Folder | Mounts `/mnt/hgfs` via `vmhgfs-fuse`; adds fstab entry; symlinks `~/Pentest` (non-fatal — safe to skip on bare metal) |
| 19 | HackTricks & RevShells | Clones HackTricks wiki + reverse-shell-generator; builds Docker image; creates `docker-compose.yml` + `start-services.sh` launcher |
| 20 | Nessus | Docker Compose service for `tenable/nessus`; local `.env` credential template; `start-nessus.sh` launcher |
| 21 | Google Cloud CLI | Installs `google-cloud-cli` via the official apt repo/keyring |
| 22 | pyenv | Installs pyenv, builds the pinned Python interpreter Prowler runs against |
| 23 | Prowler | `pipx install prowler` for cloud security posture scanning (AWS/Azure/GCP/K8s) |
| 24 | Claude Code | Installs Claude Code via the official installer; ensures `~/.claude/bin` on PATH |
| 25 | SharpCollection | Sparse-checkout of Flangvik's compiled offensive C# binaries |
| 26 | Ligolo-ng | Latest proxy + agent binaries for every published platform, checksum-verified when a manifest is published |

At the end of a run, closing instructions are also saved to `~/Tools/NEXT_STEPS.md` and resolved tool versions to `~/Tools/INSTALLED_VERSIONS.txt`.

Full script docs, customisation tunables, secrets handling, re-running individual sections, and troubleshooting: **[scripts/README.md](scripts/README.md)**.

## Repository layout

```
~/.dotfiles/
├── scripts/
│   ├── exegol-setup.sh      # main provisioning script (run this first)
│   └── README.md            # full setup docs, Burp Pro, troubleshooting
├── nvim/                    # Neovim config (lazy.nvim, LSP, Telescope, DAP)
│   └── README.md            # keybind cheatsheet and customisation guide
├── Doom/                    # Doom Emacs config
│   └── README.org
├── Tmux/                    # tmux config
│   └── README.org
├── Zsh/                     # .zshrc and .zshenv
├── alacritty/               # Alacritty terminal config
├── kitty/                   # Kitty terminal config
└── pentest-tools/           # version-pinned offensive tool manifest + fetch scripts
    └── README.md
```

## Dotfiles

`exegol-setup.sh` §8 (Host Dotfile Symlinks) symlinks `~/.zshrc`, `~/.zshenv`, and `~/.tmux.conf` from the directories below.

| Directory | Config | More info |
|-----------|--------|-----------|
| `nvim/` | Neovim — lazy.nvim, Catppuccin, Telescope, LSP via Mason, DAP, Harpoon | [nvim/README.md](nvim/README.md) |
| `Doom/` | Doom Emacs | [Doom/README.org](Doom/README.org) |
| `Tmux/` | tmux | [Tmux/README.org](Tmux/README.org) |
| `Zsh/` | `.zshrc`, `.zshenv` | — |
| `alacritty/` | Alacritty terminal | — |
| `kitty/` | Kitty terminal | — |

## Pentest tools

`pentest-tools/` holds a version-pinned manifest of offensive binaries (chisel, ligolo-ng, linpeas, mimikatz, SharpHound, impacket-static, Rubeus, and more) with fetch scripts for Linux and Windows. The repo stores the manifest, not the binaries — binaries are pulled on demand into a git-ignored `vendor/` directory.

See [pentest-tools/README.md](pentest-tools/README.md) for the full tool list, fetch instructions, and how to add or update tools.

## Credits

- [Exegol](https://github.com/ThePorgs/Exegol) by ThePorgs — the framework this script provisions
- [Greg Scharf's Burp Pro guide](https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/) — the prefs.xml propagation trick
- [Greg Scharf's Exegol setup guide](https://blog.gregscharf.com/2023/04/01/exegol-hacking-framework-setup/) — early my-resources layout reference
- [Exegol docs: my-resources](https://docs.exegol.com/images/my-resources) — canonical reference for the customisation surface

## Licence

Personal use. Adapt freely.
