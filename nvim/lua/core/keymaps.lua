-- ============================================================
-- core/keymaps.lua — Doom-inspired keybindings
-- ============================================================
-- Convention: SPC = global leader, , = local leader
-- All mappings use which-key groups (defined in plugins/which-key.lua)

local map = function(mode, lhs, rhs, opts)
  opts = vim.tbl_extend("force", { silent = true, noremap = true }, opts or {})
  vim.keymap.set(mode, lhs, rhs, opts)
end

-- ── Escape ──────────────────────────────────────────────────
map("i", "jk", "<Esc>",  { desc = "Exit insert mode" })
map("i", "jj", "<Esc>",  { desc = "Exit insert mode" })

-- ── Window navigation (no <C-w> prefix needed) ───────────────
map("n", "<C-h>", "<C-w>h", { desc = "Move to left window" })
map("n", "<C-j>", "<C-w>j", { desc = "Move to lower window" })
map("n", "<C-k>", "<C-w>k", { desc = "Move to upper window" })
map("n", "<C-l>", "<C-w>l", { desc = "Move to right window" })

-- ── Resize windows ──────────────────────────────────────────
map("n", "<C-Up>",    "<cmd>resize +2<CR>",          { desc = "Increase height" })
map("n", "<C-Down>",  "<cmd>resize -2<CR>",          { desc = "Decrease height" })
map("n", "<C-Left>",  "<cmd>vertical resize -2<CR>", { desc = "Decrease width" })
map("n", "<C-Right>", "<cmd>vertical resize +2<CR>", { desc = "Increase width" })

-- ── Buffer navigation ───────────────────────────────────────
map("n", "<S-l>", "<cmd>bnext<CR>",     { desc = "Next buffer" })
map("n", "<S-h>", "<cmd>bprevious<CR>", { desc = "Prev buffer" })

-- ── Stay in indent mode after tab ───────────────────────────
map("v", "<", "<gv", { desc = "Unindent (stay selected)" })
map("v", ">", ">gv", { desc = "Indent (stay selected)" })

-- ── Move visual selection up/down ───────────────────────────
map("v", "J", ":m '>+1<CR>gv=gv", { desc = "Move selection down" })
map("v", "K", ":m '<-2<CR>gv=gv", { desc = "Move selection up" })

-- ── Keep cursor centred when jumping ────────────────────────
map("n", "<C-d>", "<C-d>zz", { desc = "Half-page down (centred)" })
map("n", "<C-u>", "<C-u>zz", { desc = "Half-page up (centred)" })
map("n", "n",     "nzzzv",   { desc = "Next search result (centred)" })
map("n", "N",     "Nzzzv",   { desc = "Prev search result (centred)" })

-- ── Paste without clobbering yank register ──────────────────
map("x", "<leader>p", [["_dP]], { desc = "Paste (preserve register)" })

-- ── Delete to black hole ────────────────────────────────────
-- Use <leader>D to avoid conflicting with <leader>d* (DAP)
map({ "n", "v" }, "<leader>D", [["_d]], { desc = "Delete (no register)" })

-- ── Clear search highlight ──────────────────────────────────
map("n", "<Esc>", "<cmd>noh<CR>", { desc = "Clear highlights" })

-- ── Quick save / quit ───────────────────────────────────────
-- <leader>q is reserved as a GROUP prefix (session keys use SPC q s/l/d)
-- Use SPC w q to close a window, SPC Q to force-quit all
map("n", "<leader>w", "<cmd>w<CR>",   { desc = "Save file" })
map("n", "<leader>Q", "<cmd>qa!<CR>", { desc = "Quit all (no save)" })

-- ────────────────────────────────────────────────────────────
-- SPC-prefixed groups (Doom-style)
-- Groups are registered in plugins/which-key.lua
-- Individual plugin mappings live next to their plugin spec
-- ────────────────────────────────────────────────────────────

-- ── [b] Buffers ─────────────────────────────────────────────
map("n", "<leader>bd", "<cmd>bd<CR>",       { desc = "Delete buffer" })
map("n", "<leader>bD", "<cmd>bd!<CR>",      { desc = "Force delete buffer" })
map("n", "<leader>bn", "<cmd>bnext<CR>",    { desc = "Next buffer" })
map("n", "<leader>bp", "<cmd>bprevious<CR>",{ desc = "Prev buffer" })
map("n", "<leader>bk", "<cmd>bd<CR>",       { desc = "Kill buffer" })

-- ── [w] Windows ─────────────────────────────────────────────
map("n", "<leader>wv", "<cmd>vsplit<CR>",   { desc = "Vertical split" })
map("n", "<leader>ws", "<cmd>split<CR>",    { desc = "Horizontal split" })
map("n", "<leader>wc", "<cmd>close<CR>",    { desc = "Close window" })
map("n", "<leader>wo", "<cmd>only<CR>",     { desc = "Close others" })
map("n", "<leader>wh", "<C-w>h",            { desc = "Go left" })
map("n", "<leader>wj", "<C-w>j",            { desc = "Go down" })
map("n", "<leader>wk", "<C-w>k",            { desc = "Go up" })
map("n", "<leader>wl", "<C-w>l",            { desc = "Go right" })
map("n", "<leader>w=", "<C-w>=",            { desc = "Balance windows" })

-- ── [t] Tabs ────────────────────────────────────────────────
map("n", "<leader>tn", "<cmd>tabnew<CR>",   { desc = "New tab" })
map("n", "<leader>tc", "<cmd>tabclose<CR>", { desc = "Close tab" })
map("n", "<leader>tl", "<cmd>tabnext<CR>",  { desc = "Next tab" })
map("n", "<leader>th", "<cmd>tabprev<CR>",  { desc = "Prev tab" })

-- ── [u] UI toggles ──────────────────────────────────────────
map("n", "<leader>un", "<cmd>set number!<CR>",         { desc = "Toggle line numbers" })
map("n", "<leader>ur", "<cmd>set relativenumber!<CR>", { desc = "Toggle relative numbers" })
map("n", "<leader>uw", "<cmd>set wrap!<CR>",           { desc = "Toggle wrap" })
map("n", "<leader>us", "<cmd>set spell!<CR>",          { desc = "Toggle spell" })

-- ── [h] Help / which-key ────────────────────────────────────
-- which-key show all is auto-bound to SPC ? / SPC h k
map("n", "<leader>?",  "<cmd>WhichKey<CR>", { desc = "Show all keymaps" })

-- ── Diagnostics ─────────────────────────────────────────────
map("n", "[d", vim.diagnostic.goto_prev, { desc = "Prev diagnostic" })
map("n", "]d", vim.diagnostic.goto_next, { desc = "Next diagnostic" })
map("n", "<leader>le", vim.diagnostic.open_float, { desc = "Show diagnostic" })
map("n", "<leader>lq", vim.diagnostic.setloclist,  { desc = "Diagnostic loclist" })
