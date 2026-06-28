# 🔥 Bloodstiller's Neovim Config

A Doom Emacs-inspired Neovim configuration — modular, fast, and pentest-friendly.

## Structure

```
~/.config/nvim/
├── init.lua                    # Entry point
└── lua/
    ├── core/
    │   ├── options.lua         # Vim options / sane defaults
    │   ├── keymaps.lua         # Global keymaps (SPC leader)
    │   ├── autocmds.lua        # Autocommands
    │   └── lazy.lua            # lazy.nvim bootstrap + loader
    └── plugins/
        ├── colorscheme.lua     # Catppuccin (swap freely)
        ├── ui.lua              # lualine, bufferline, noice, dashboard, which-key
        ├── telescope.lua       # Fuzzy finder (SPC f …)
        ├── treesitter.lua      # Syntax + text objects
        ├── lsp.lua             # mason + lspconfig + none-ls + Trouble
        ├── completion.lua      # nvim-cmp + LuaSnip
        ├── editor.lua          # neo-tree, gitsigns, autopairs, flash, todo, …
        └── extras.lua          # REST.nvim, harpoon, DAP, markdown-preview
```

## Install

```bash
# Back up any existing config
mv ~/.config/nvim ~/.config/nvim.bak

# Copy this config
cp -r /path/to/this/nvim-config ~/.config/nvim

# Launch — lazy.nvim auto-installs on first run
nvim
```

On first launch, lazy.nvim will clone itself and install all plugins.
Mason will auto-install LSP servers listed in `plugins/lsp.lua`.

## Requirements

| Tool       | Purpose                       |
|------------|-------------------------------|
| Neovim ≥ 0.10 | Core                      |
| git        | lazy.nvim / plugin cloning    |
| make       | telescope-fzf-native          |
| ripgrep    | Telescope live_grep           |
| fd         | Telescope find_files          |
| node / npm | markdown-preview, some LSPs   |
| A Nerd Font | Icons (e.g. JetBrainsMono NF) |

```bash
# Arch / Manjaro
sudo pacman -S neovim ripgrep fd nodejs npm

# Debian / Ubuntu
sudo apt install neovim ripgrep fd-find nodejs npm

# macOS
brew install neovim ripgrep fd node
```

## Key Bindings Cheatsheet

Leader = `Space` | Local leader = `,`

### Navigation
| Key         | Action                        |
|-------------|-------------------------------|
| `SPC f f`   | Find files                    |
| `SPC f r`   | Recent files                  |
| `SPC f g`   | Live grep (ripgrep)           |
| `SPC f b`   | Open buffers                  |
| `SPC /`     | Fuzzy search current buffer   |
| `SPC e`     | Toggle file explorer          |
| `s`         | Flash jump                    |
| `]h / [h`   | Next / prev git hunk          |
| `]d / [d`   | Next / prev diagnostic        |

### LSP
| Key         | Action                        |
|-------------|-------------------------------|
| `gd`        | Go to definition              |
| `K`         | Hover docs                    |
| `gr`        | References (Telescope)        |
| `SPC c r`   | Rename symbol                 |
| `SPC c a`   | Code action                   |
| `SPC c f`   | Format buffer                 |

### Buffers / Windows
| Key         | Action                        |
|-------------|-------------------------------|
| `SPC b d`   | Delete buffer                 |
| `SPC w v`   | Vertical split                |
| `SPC w s`   | Horizontal split              |
| `C-h/j/k/l` | Navigate windows              |
| `Shift-h/l` | Prev / next buffer            |

### Git
| Key         | Action                        |
|-------------|-------------------------------|
| `SPC g g`   | Fugitive status               |
| `SPC g h s` | Stage hunk                    |
| `SPC g h p` | Preview hunk                  |
| `SPC g c`   | Git commits (Telescope)       |

### Pentest Extras
| Key         | Action                        |
|-------------|-------------------------------|
| `SPC r r`   | Run HTTP request (REST.nvim)  |
| `SPC h a`   | Harpoon add file              |
| `SPC h h`   | Harpoon menu                  |
| `SPC 1-4`   | Jump to harpoon slot          |
| `SPC d b`   | Toggle breakpoint (DAP)       |

### UI Toggles
| Key         | Action                        |
|-------------|-------------------------------|
| `SPC u z`   | Zen mode                      |
| `SPC u w`   | Toggle wrap                   |
| `SPC u s`   | Toggle spell check            |
| `SPC ?`     | Show ALL keymaps (which-key)  |

## Customising

- **Add an LSP server**: add to `ensure_installed` in `plugins/lsp.lua`
- **Change theme**: edit `plugins/colorscheme.lua`
- **Add a plugin**: create a new file in `lua/plugins/` returning a lazy spec table
- **Obsidian wiki editing**: uncomment the `obsidian.nvim` block in `plugins/extras.lua`
