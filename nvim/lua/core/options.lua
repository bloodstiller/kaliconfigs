-- ============================================================
-- core/options.lua — Sane defaults (Doom-style)
-- ============================================================

local opt = vim.opt

-- ── Leader ──────────────────────────────────────────────────
vim.g.mapleader      = " "
vim.g.maplocalleader = ","

-- ── UI ──────────────────────────────────────────────────────
opt.number         = true        -- absolute line number on current line
opt.relativenumber = true        -- relative numbers everywhere else
opt.cursorline     = true        -- highlight current line
opt.signcolumn     = "yes"       -- always show gutter (no layout jumps)
opt.colorcolumn    = "100"       -- soft ruler at 100 chars
opt.termguicolors  = true        -- 24-bit colour
opt.showmode       = false       -- mode shown by statusline instead
opt.laststatus     = 3           -- single global statusline
opt.cmdheight      = 1
opt.pumheight      = 10          -- completion popup max height
opt.scrolloff      = 8           -- keep 8 lines visible around cursor
opt.sidescrolloff  = 8

-- ── Editing ─────────────────────────────────────────────────
opt.expandtab   = true           -- spaces not tabs
opt.tabstop     = 4
opt.shiftwidth  = 4
opt.softtabstop = 4
opt.smartindent = true
opt.wrap        = false          -- no line wrap by default
opt.linebreak   = true           -- if wrap enabled, break at word boundary
opt.breakindent = true

-- ── Search ──────────────────────────────────────────────────
opt.hlsearch   = true
opt.incsearch  = true
opt.ignorecase = true
opt.smartcase  = true            -- case-sensitive if uppercase present

-- ── Splits ──────────────────────────────────────────────────
opt.splitbelow = true
opt.splitright = true

-- ── Files / undo ────────────────────────────────────────────
opt.swapfile = false
opt.backup   = false
opt.undofile = true
opt.undodir  = vim.fn.stdpath("data") .. "/undo"

-- ── Performance ─────────────────────────────────────────────
opt.updatetime  = 200            -- faster CursorHold (LSP hover, etc.)
opt.timeoutlen  = 300            -- key timeout (which-key feels snappy)

-- ── Clipboard ───────────────────────────────────────────────
-- Only sync with system clipboard if a provider is available
-- Install xclip or xsel on Linux: sudo apt install xclip
if vim.fn.executable("xclip") == 1
  or vim.fn.executable("xsel") == 1
  or vim.fn.executable("wl-copy") == 1
  or vim.fn.has("mac") == 1 then
  opt.clipboard = "unnamedplus"
end

-- ── Folds ───────────────────────────────────────────────────
opt.foldmethod     = "expr"
opt.foldexpr       = "nvim_treesitter#foldexpr()"
opt.foldenable     = false       -- open all folds by default
opt.foldlevel      = 99

-- ── Misc ────────────────────────────────────────────────────
opt.mouse       = "a"
opt.conceallevel = 2             -- nicer markdown / org rendering
opt.list        = true           -- show whitespace hints
opt.listchars   = { tab = "→ ", trail = "·", nbsp = "␣" }
opt.fillchars   = { eob = " " }  -- hide ~ end-of-buffer markers
opt.shortmess:append("sI")       -- suppress intro message
