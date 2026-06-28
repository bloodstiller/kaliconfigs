-- ============================================================
-- core/autocmds.lua — Quality-of-life autocommands
-- ============================================================

local augroup = function(name)
  return vim.api.nvim_create_augroup("doom_" .. name, { clear = true })
end

-- ── Highlight on yank ────────────────────────────────────────
vim.api.nvim_create_autocmd("TextYankPost", {
  group    = augroup("yank_highlight"),
  callback = function()
    vim.hl.on_yank({ higroup = "Visual", timeout = 200 })
  end,
})

-- ── Return to last cursor position ──────────────────────────
vim.api.nvim_create_autocmd("BufReadPost", {
  group    = augroup("restore_cursor"),
  callback = function()
    local mark = vim.api.nvim_buf_get_mark(0, '"')
    local lcount = vim.api.nvim_buf_line_count(0)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

-- ── Auto-resize splits when terminal is resized ──────────────
vim.api.nvim_create_autocmd("VimResized", {
  group    = augroup("resize_splits"),
  callback = function() vim.cmd("tabdo wincmd =") end,
})

-- ── Remove trailing whitespace on save ──────────────────────
vim.api.nvim_create_autocmd("BufWritePre", {
  group    = augroup("trim_whitespace"),
  pattern  = "*",
  callback = function()
    local pos = vim.api.nvim_win_get_cursor(0)
    vim.cmd([[%s/\s\+$//e]])
    vim.api.nvim_win_set_cursor(0, pos)
  end,
})

-- ── Close certain buffers with just 'q' ─────────────────────
vim.api.nvim_create_autocmd("FileType", {
  group   = augroup("close_with_q"),
  pattern = { "help", "man", "qf", "lspinfo", "checkhealth",
              "notify", "fugitiveblame", "spectre_panel" },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.keymap.set("n", "q", "<cmd>close<CR>",
      { buffer = event.buf, silent = true })
  end,
})

-- ── Disable auto-comment on new lines ───────────────────────
vim.api.nvim_create_autocmd("BufEnter", {
  group    = augroup("no_auto_comment"),
  callback = function()
    vim.opt.formatoptions:remove({ "c", "r", "o" })
  end,
})

-- ── Terminal: enter insert automatically ─────────────────────
vim.api.nvim_create_autocmd("TermOpen", {
  group    = augroup("term_insert"),
  callback = function()
    vim.opt_local.number         = false
    vim.opt_local.relativenumber = false
    vim.cmd("startinsert")
  end,
})

-- ── Markdown / text: enable spellcheck & wrap ────────────────
vim.api.nvim_create_autocmd("FileType", {
  group   = augroup("prose_mode"),
  pattern = { "markdown", "text", "gitcommit" },
  callback = function()
    vim.opt_local.spell     = true
    vim.opt_local.wrap      = true
    vim.opt_local.linebreak = true
  end,
})
