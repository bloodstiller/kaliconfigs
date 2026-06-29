# exegol-setup

> One-shot Ubuntu VM bootstrap for [Exegol](https://exegol.com), the Docker-based pentest environment. Installs Exegol, mirrors your dotfiles into every container via `my-resources`, and bootstraps Burp Suite Pro across containers without burning extra licence activations.

Companion script to the Kali workflow in [`bloodstiller/kaliconfigs`](https://github.com/bloodstiller/kaliconfigs). Built for the use case "I'm on a Windows employer laptop, I run an Ubuntu VM, and I want my Kali muscle-memory to follow me into Exegol containers."

## What it does

Twelve checkpointed sections. Each one is re-runnable; completion is tracked in `~/.exegol_setup_checkpoints/`.

| § | Step | Touches |
|---|---|---|
| 1 | apt update + host packages | `git`, `curl`, `python3`, `pipx`, `zsh`, `tmux`, `vim`, `openvpn`, `argcomplete`, `fonts-firacode` |
| 2 | Docker engine | install + `systemctl enable` (no docker-group membership — see below) |
| 3 | Exegol wrapper | `pipx install exegol`, argcomplete, `sudo -E` alias for `.bashrc`/`.zshrc` |
| 4 | Dotfiles | clones `kaliconfigs` to `~/.dotfiles` |
| 5 | Oh-My-Zsh + plugins | host shell setup: `zsh-syntax-highlighting`, `zsh-autosuggestions`, `fast-syntax-highlighting`, `fzf-tab`, `tpm`, `chsh` |
| 6 | Host dotfile symlinks | `~/.zshrc`, `~/.zshenv`, `~/.tmux.conf` |
| 7 | my-resources scaffold | full directory tree under `~/.exegol/my-resources/` |
| 8 | Container configs | `tmux.conf`, filtered `zshrc`, `aliases`, `vimrc` |
| 9 | Container packages | `apt/packages.list`, `python3/requirements.txt` |
| 10 | `load_user_setup.sh` | per-container init: `nuclei` template refresh + MOTD |
| 11 | Wordlists | Hacking-APIs cloned to `my-resources/wordlists/` |
| 12 | Burp Pro bootstrap | OpenJDK download, `java-burp-setup.sh` generation |

## What it deliberately doesn't do

Anything Exegol's `full` image already ships gets dropped — duplicating ships you nowhere except slower builds. Confirmed already present in Exegol and removed from this script vs the original Kali workflow:

- **Tools** — `jwt` (jwt_tool), `kiterunner`, `arjun`, `kerbrute`, `impacket`, `netexec`, `bloodhound`/`bloodhound-ce`, `certipy`, `bloodyAD`, `mitm6`, `nuclei`, `katana`, `ffuf`, `gobuster`, `feroxbuster`, `dirsearch`, `burpsuite` (community), `mitmproxy`, `metasploit`, `hashcat`, `hydra`
- **Wordlists** — `SecLists` (at `/usr/share/seclists`)
- **Host-only stuff** — Doom Emacs, Alacritty, HackTricks docker stack, BloodHound CE docker-compose, Nessus container, VMware shared folder, sops/age SSH key flow

Only thing that's actually added on top of `exegol full`:

- `mitmproxy2swagger` (via `setup/python3/requirements.txt`)
- `Hacking-APIs` wordlist
- Burp Pro (manually licensed, propagated via `prefs.xml`)
- Custom tmux/zsh configs from `kaliconfigs`

## Prerequisites

- Fresh Ubuntu / Debian VM
- Regular user with `sudo` rights
- Internet access
- (For Burp Pro section) a PortSwigger account with a Burp Pro licence

If you're not on Ubuntu/Debian, the script aborts at the start. Edit `apt-get` calls to your package manager and remove the abort if you want to adapt it.

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

`exegol` is aliased to `sudo -E ~/.local/bin/exegol`, so first invocation will prompt for sudo.

## Burp Suite Pro setup

The script auto-installs OpenJDK 23 into `~/.exegol/my-resources/bin/` and generates a helper script `java-burp-setup.sh`. The actual Burp install is interactive and login-walled at PortSwigger, so it stays manual.

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

If you prefer the docker group anyway, edit section 2 of the script to re-add `usermod -aG docker "$USER"` and remove the alias-write block in section 3.

## Customisation

Tunables at the top of the script:

```bash
DOTFILES_REPO   # default: https://github.com/bloodstiller/kaliconfigs.git
DOTFILES_DIR    # default: $HOME/.dotfiles
EXEGOL_RES      # default: $HOME/.exegol/my-resources
WORDLISTS_DIR   # default: $HOME/wordlists
DF_ZSHRC        # default: $DOTFILES_DIR/Zsh/.zshrc
DF_ZSHENV       # default: $DOTFILES_DIR/Zsh/.zshenv
DF_TMUX         # default: $DOTFILES_DIR/Tmux/.tmux.conf
JDK_VERSION     # default: "23"
JDK_DIR         # default: "jdk-23"
JDK_URL_AMD64   # OpenJDK 23 amd64 tarball URL
JDK_URL_ARM64   # OpenJDK 23 arm64 tarball URL
```

### Re-running a single section

```bash
rm ~/.exegol_setup_checkpoints/<checkpoint_name>
bash exegol-setup.sh
```

Checkpoint names: `apt`, `docker`, `exegol`, `dotfiles`, `ohmyzsh`, `dotfile_links`, `myresources_scaffold`, `myresources_configs`, `myresources_pkgs`, `load_user_setup`, `wordlists`, `burp_pro`.

### Starting completely fresh

```bash
rm -rf ~/.exegol_setup_checkpoints
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

Inside container: `sudo update-alternatives --config java`, pick `/usr/lib/jvm/jdk-23/bin/java`. If `jdk-23` isn't listed, `java-burp-setup.sh` was never run — run it.

### Burp launches but asks for licence

`prefs.xml` either wasn't copied to `~/.exegol/my-resources/bin/` on the host, or wasn't applied inside the container. Run `java-burp-setup.sh` again, or manually:

```bash
mkdir -p /root/.java/.userPrefs/burp
cp /opt/my-resources/bin/prefs.xml /root/.java/.userPrefs/burp/
```

### tmux starts bash instead of zsh

Recreate the container with `-e SHELL=/usr/bin/zsh`.

### Plugin double-load / theme weirdness in container

The OMZ filter in section 8 didn't strip `source oh-my-zsh.sh` / `ZSH_THEME=` / `plugins=(...)` from your `.zshrc`. Check `~/.exegol/my-resources/setup/zsh/zshrc` and remove any of those lines by hand.

### Inspecting what happened during container init

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
exegol-setup/
├── README.md                # this file
├── exegol-setup.sh          # the bootstrap script
└── docs/
    └── exegol-setup.md      # Obsidian-formatted reference note (optional)
```

## Credits

- [Exegol](https://github.com/ThePorgs/Exegol) by ThePorgs — the framework this script provisions
- [Greg Scharf's Burp Pro guide](https://blog.gregscharf.com/2025/07/23/burp-suite-pro-install-in-exegol/) — the prefs.xml propagation trick
- [Greg Scharf's Exegol setup guide](https://blog.gregscharf.com/2023/04/01/exegol-hacking-framework-setup/) — early my-resources layout reference
- [Exegol docs: my-resources](https://docs.exegol.com/images/my-resources) — canonical reference for the customisation surface

## Licence

Personal use. Adapt freely.
