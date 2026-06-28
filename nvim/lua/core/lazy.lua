-- ============================================================
-- core/lazy.lua — Bootstrap lazy.nvim + load all plugin specs
-- ============================================================

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git", "clone", "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  spec = {
    { import = "plugins" },       -- lua/plugins/*.lua
  },
  defaults = { lazy = true },     -- lazy-load everything by default
  install  = { colorscheme = { "catppuccin", "habamax" } },
  rocks = {
    enabled    = false,   -- no plugins need luarocks; suppresses hererocks warnings
  },
  checker  = { enabled = true, notify = false }, -- silent update checks
  change_detection = { notify = false },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip", "tarPlugin", "tohtml", "tutor", "zipPlugin",
        "netrwPlugin",   -- replaced by neo-tree
      },
    },
  },
  ui = {
    border = "rounded",
    icons  = {
      cmd     = " ",
      config  = "",
      event   = "",
      ft      = " ",
      init    = " ",
      keys    = " ",
      plugin  = " ",
      runtime = " ",
      source  = " ",
      start   = "",
      task    = "✔ ",
    },
  },
})
