-- ============================================================
-- plugins/ui.lua — UI layer (statusline, buffers, notifications)
-- ============================================================

return {

  -- ── Lualine — statusline ────────────────────────────────────
  {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    opts = {
      options = {
        theme                = "catppuccin",
        globalstatus         = true,
        component_separators = { left = "", right = "" },
        section_separators   = { left = "", right = "" },
        disabled_filetypes   = { statusline = { "dashboard", "lazy" } },
      },
      sections = {
        lualine_a = { { "mode", icon = "" } },
        lualine_b = { "branch", "diff", "diagnostics" },
        lualine_c = { { "filename", path = 1 } },   -- relative path
        lualine_x = { "encoding", "fileformat", "filetype" },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
    },
  },

  -- ── Bufferline — tab-like buffer list ───────────────────────
  {
    "akinsho/bufferline.nvim",
    event        = "VeryLazy",
    version      = "*",
    dependencies = "nvim-tree/nvim-web-devicons",
    opts = {
      options = {
        diagnostics            = "nvim_lsp",
        always_show_bufferline = false,
        offsets = {
          { filetype = "neo-tree", text = "Explorer", highlight = "Directory" },
        },
      },
    },
    keys = {
      { "<leader>bp", "<cmd>BufferLineTogglePin<CR>",            desc = "Toggle pin" },
      { "<leader>bP", "<cmd>BufferLineGroupClose ungrouped<CR>", desc = "Close unpinned" },
    },
  },

  -- ── Noice — fancy command line / notifications ───────────────
  {
    "folke/noice.nvim",
    event        = "VeryLazy",
    dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
    opts = {
      lsp = {
        override = {
          ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
          ["vim.lsp.util.stylize_markdown"]                = true,
          ["cmp.entry.get_documentation"]                  = true,
        },
      },
      presets = {
        bottom_search        = true,
        command_palette      = true,
        long_message_to_split = true,
        inc_rename           = false,
        lsp_doc_border       = true,
      },
    },
    keys = {
      { "<leader>sn",  "<cmd>Noice<CR>",         desc = "Noice log" },
      { "<leader>un",  "<cmd>Noice dismiss<CR>", desc = "Dismiss notifications" },
    },
  },

  -- ── nvim-notify — pretty toast notifications ─────────────────
  {
    "rcarriga/nvim-notify",
    lazy = true,
    opts = {
      timeout  = 3000,
      max_width = 60,
      render   = "wrapped-compact",
      stages   = "fade_in_slide_out",
    },
    init = function()
      vim.notify = require("notify")
    end,
  },

  -- ── Dashboard — Doom-style splash screen ─────────────────────
  {
    "nvimdev/dashboard-nvim",
    event        = "VimEnter",
    dependencies = "nvim-tree/nvim-web-devicons",
    opts = {
      theme = "doom",
      config = {
        header = {
          "",
          " ███╗   ██╗██╗   ██╗██╗███╗   ███╗",
          " ████╗  ██║██║   ██║██║████╗ ████║",
          " ██╔██╗ ██║██║   ██║██║██╔████╔██║",
          " ██║╚██╗██║╚██╗ ██╔╝██║██║╚██╔╝██║",
          " ██║ ╚████║ ╚████╔╝ ██║██║ ╚═╝ ██║",
          " ╚═╝  ╚═══╝  ╚═══╝  ╚═╝╚═╝     ╚═╝",
          "",
          "         The Bloodstiller Edition          ",
          "",
        },
        center = {
          { action = "Telescope find_files",  desc = " Find file",     icon = " ", key = "f" },
          { action = "ene | startinsert",     desc = " New file",      icon = " ", key = "n" },
          { action = "Telescope oldfiles",    desc = " Recent files",  icon = " ", key = "r" },
          { action = "Telescope live_grep",   desc = " Grep",          icon = " ", key = "g" },
          { action = "e $MYVIMRC",            desc = " Config",        icon = " ", key = "c" },
          { action = "Lazy",                  desc = " Plugins",       icon = "󰒲 ", key = "p" },
          { action = "qa",                    desc = " Quit",          icon = " ", key = "q" },
        },
        footer = function()
          local stats = require("lazy").stats()
          return { "⚡ " .. stats.count .. " plugins loaded in "
            .. (math.floor(stats.startuptime * 100 + 0.5) / 100) .. "ms" }
        end,
      },
    },
  },

  -- ── Indent guides ────────────────────────────────────────────
  {
    "lukas-reineke/indent-blankline.nvim",
    event = { "BufReadPost", "BufNewFile" },
    main  = "ibl",
    opts  = {
      indent  = { char = "│" },
      scope   = { enabled = true },
      exclude = {
        filetypes = { "help", "dashboard", "lazy", "mason", "notify" },
      },
    },
  },

  -- ── Which-key ────────────────────────────────────────────────
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts  = {
      plugins = { spelling = true },
      win     = { border = "rounded" },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      -- Register group labels (Doom-style)
      wk.add({
        { "<leader>b",  group = "Buffers"      },
        { "<leader>c",  group = "Code / LSP"   },
        { "<leader>d",  group = "DAP"          },
        { "<leader>f",  group = "Find (Telescope)" },
        { "<leader>g",  group = "Git"          },
        { "<leader>h",  group = "Harpoon"      },
        { "<leader>l",  group = "LSP"          },
        { "<leader>q",  group = "Session"      },
        { "<leader>r",  group = "REST / HTTP"  },
        { "<leader>s",  group = "Search"       },
        { "<leader>t",  group = "Tabs"         },
        { "<leader>u",  group = "UI toggles"   },
        { "<leader>w",  group = "Windows"      },
        { "<leader>x",  group = "Diagnostics"  },
      })
    end,
  },

  -- ── Devicons ─────────────────────────────────────────────────
  { "nvim-tree/nvim-web-devicons", lazy = true },
}
