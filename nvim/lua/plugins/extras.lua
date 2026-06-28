-- ============================================================
-- plugins/extras.lua — Pentest / security extras
-- ============================================================
-- These are optional additions relevant to security work.
-- Comment out anything you don't use.

return {

  -- ── Kulala.nvim — send HTTP requests from .http/.rest files ──
  -- Drop-in rest.nvim replacement, zero build deps (pure Lua)
  {
    "mistweaverco/kulala.nvim",
    ft   = { "http", "rest" },
    keys = {
      { "<leader>rr", function() require("kulala").run()        end, desc = "Run HTTP request" },
      { "<leader>rl", function() require("kulala").replay()     end, desc = "Replay last request" },
      { "<leader>ri", function() require("kulala").inspect()    end, desc = "Inspect request" },
      { "<leader>rs", function() require("kulala").show_stats() end, desc = "Show stats" },
      { "<leader>rb", function() require("kulala").scratchpad() end, desc = "HTTP scratchpad" },
    },
    opts = {
      split_direction   = "horizontal",
      default_view      = "body",   -- "body" | "headers" | "headers_body"
      environment_scope = "b",      -- buffer-local env vars
    },
  },

  -- ── Markdown preview ─────────────────────────────────────────
  {
    "iamcco/markdown-preview.nvim",
    ft      = "markdown",
    build   = "cd app && npm install",
    cmd     = { "MarkdownPreview", "MarkdownPreviewStop" },
    keys    = {
      { "<leader>um", "<cmd>MarkdownPreview<CR>", desc = "Markdown preview" },
    },
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },

  -- ── Obsidian.nvim — optional if editing your wiki in Neovim ─
  -- Uncomment and set vault path if you use nvim as your Obsidian editor
  -- {
  --   "epwalsh/obsidian.nvim",
  --   version      = "*",
  --   ft           = "markdown",
  --   dependencies = "nvim-lua/plenary.nvim",
  --   opts = {
  --     workspaces = {
  --       { name = "wiki", path = "~/wiki" },
  --     },
  --     completion = { nvim_cmp = true },
  --   },
  -- },

  -- ── Harpoon — quick file marks (jump between pentest files) ──
  {
    "ThePrimeagen/harpoon",
    branch       = "harpoon2",
    dependencies = "nvim-lua/plenary.nvim",
    keys = function()
      local h  = require("harpoon")
      local list = h.list
      return {
        { "<leader>ha", function() h:list():add() end,                      desc = "Harpoon add" },
        { "<leader>hh", function() h.ui:toggle_quick_menu(h:list()) end,    desc = "Harpoon menu" },
        { "<leader>1",  function() h:list():select(1) end,                  desc = "Harpoon 1" },
        { "<leader>2",  function() h:list():select(2) end,                  desc = "Harpoon 2" },
        { "<leader>3",  function() h:list():select(3) end,                  desc = "Harpoon 3" },
        { "<leader>4",  function() h:list():select(4) end,                  desc = "Harpoon 4" },
      }
    end,
  },

  -- ── nvim-dap — debugger (useful for script debugging) ───────
  {
    "mfussenegger/nvim-dap",
    keys = {
      { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "Toggle breakpoint" },
      { "<leader>dc", function() require("dap").continue()          end, desc = "DAP continue" },
      { "<leader>di", function() require("dap").step_into()         end, desc = "DAP step into" },
      { "<leader>do", function() require("dap").step_over()         end, desc = "DAP step over" },
      { "<leader>dr", function() require("dap").repl.open()         end, desc = "DAP REPL" },
    },
  },
  {
    "rcarriga/nvim-dap-ui",
    dependencies = { "mfussenegger/nvim-dap", "nvim-neotest/nvim-nio" },
    keys = {
      { "<leader>du", function() require("dapui").toggle() end, desc = "DAP UI" },
    },
    config = function()
      local dap, dapui = require("dap"), require("dapui")
      dapui.setup()
      dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
      dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
    end,
  },
}
