-- ============================================================
-- plugins/editor.lua — Editor quality-of-life plugins
-- ============================================================

return {

  -- ── Neo-tree — file explorer ─────────────────────────────────
  {
    "nvim-neo-tree/neo-tree.nvim",
    version      = "*",
    cmd          = "Neotree",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    keys = {
      { "<leader>e",  "<cmd>Neotree toggle<CR>",         desc = "Explorer (toggle)" },
      { "<leader>E",  "<cmd>Neotree reveal<CR>",         desc = "Reveal file" },
      { "<leader>be", "<cmd>Neotree buffers reveal float<CR>", desc = "Buffer explorer" },
    },
    opts = {
      close_if_last_window = true,
      popup_border_style   = "rounded",
      window = { width = 30 },
      filesystem = {
        filtered_items = { hide_dotfiles = false, hide_gitignored = true },
        follow_current_file = { enabled = true },
        use_libuv_file_watcher = true,
      },
    },
  },

  -- ── Gitsigns — git gutters & hunk actions ───────────────────
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts  = {
      signs = {
        add          = { text = "▎" },
        change       = { text = "▎" },
        delete       = { text = "" },
        topdelete    = { text = "" },
        changedelete = { text = "▎" },
        untracked    = { text = "▎" },
      },
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        local map = function(mode, l, r, desc)
          vim.keymap.set(mode, l, r, { buffer = bufnr, desc = desc })
        end
        -- Navigation
        map("n", "]h", gs.next_hunk,  "Next hunk")
        map("n", "[h", gs.prev_hunk,  "Prev hunk")
        -- Actions
        map({ "n","v" }, "<leader>ghs", "<cmd>Gitsigns stage_hunk<CR>",  "Stage hunk")
        map({ "n","v" }, "<leader>ghr", "<cmd>Gitsigns reset_hunk<CR>",  "Reset hunk")
        map("n", "<leader>ghS", gs.stage_buffer,          "Stage buffer")
        map("n", "<leader>ghu", gs.undo_stage_hunk,       "Undo stage hunk")
        map("n", "<leader>ghR", gs.reset_buffer,          "Reset buffer")
        map("n", "<leader>ghp", gs.preview_hunk,          "Preview hunk")
        map("n", "<leader>ghb", function() gs.blame_line({ full = true }) end, "Blame line")
        map("n", "<leader>ghd", gs.diffthis,              "Diff this")
        map("n", "<leader>ghD", function() gs.diffthis("~") end, "Diff this ~")
        -- Text object
        map({ "o","x" }, "ih", "<cmd>Gitsigns select_hunk<CR>", "Select hunk")
      end,
    },
  },

  -- ── Fugitive — Git integration ───────────────────────────────
  {
    "tpope/vim-fugitive",
    cmd  = { "Git", "GBrowse", "Gdiffsplit", "Gvdiffsplit" },
    keys = {
      { "<leader>gg", "<cmd>Git<CR>",              desc = "Git status (fugitive)" },
      { "<leader>gd", "<cmd>Gdiffsplit<CR>",       desc = "Diff split" },
      { "<leader>gl", "<cmd>Git log --oneline<CR>", desc = "Git log" },
      { "<leader>gp", "<cmd>Git push<CR>",         desc = "Git push" },
    },
  },

  -- ── Autopairs ────────────────────────────────────────────────
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts  = { check_ts = true },   -- Treesitter-aware pairing
    config = function(_, opts)
      local ap = require("nvim-autopairs")
      ap.setup(opts)
      -- Integrate with nvim-cmp
      local cmp_ap = require("nvim-autopairs.completion.cmp")
      require("cmp").event:on("confirm_done", cmp_ap.on_confirm_done())
    end,
  },

  -- ── Comment.nvim ─────────────────────────────────────────────
  {
    "numToStr/Comment.nvim",
    event = { "BufReadPost", "BufNewFile" },
    opts  = {},
    -- gcc = line comment, gbc = block comment, gc in visual
  },

  -- ── nvim-surround ────────────────────────────────────────────
  {
    "kylechui/nvim-surround",
    version = "*",
    event   = "VeryLazy",
    opts    = {},
    -- cs"' = change surrounding " to '
    -- ds"  = delete surrounding "
    -- ys{motion}' = add surrounding '
  },

  -- ── mini.ai — better text objects ────────────────────────────
  {
    "echasnovski/mini.ai",
    event   = "VeryLazy",
    version = "*",
    opts    = {},
    -- ia, aa = in/around arg  etc.
  },

  -- ── Flash — fast motion / jump ───────────────────────────────
  {
    "folke/flash.nvim",
    event = "VeryLazy",
    opts  = {},
    keys  = {
      { "s",     mode = { "n","x","o" }, function() require("flash").jump()              end, desc = "Flash jump" },
      { "S",     mode = { "n","x","o" }, function() require("flash").treesitter()        end, desc = "Flash treesitter" },
      { "r",     mode = "o",            function() require("flash").remote()             end, desc = "Flash remote" },
      { "R",     mode = { "o","x" },    function() require("flash").treesitter_search()  end, desc = "Flash treesitter search" },
      { "<C-s>", mode = "c",            function() require("flash").toggle()             end, desc = "Toggle flash in search" },
    },
  },

  -- ── Todo-comments ────────────────────────────────────────────
  {
    "folke/todo-comments.nvim",
    event        = { "BufReadPost", "BufNewFile" },
    dependencies = "nvim-lua/plenary.nvim",
    opts         = {},
    keys = {
      { "]t",        function() require("todo-comments").jump_next() end, desc = "Next TODO" },
      { "[t",        function() require("todo-comments").jump_prev() end, desc = "Prev TODO" },
      { "<leader>xt", "<cmd>TodoTrouble<CR>",   desc = "TODO (Trouble)" },
      { "<leader>ft", "<cmd>TodoTelescope<CR>", desc = "TODO (Telescope)" },
    },
  },

  -- ── Spectre — project-wide search & replace ──────────────────
  {
    "nvim-pack/nvim-spectre",
    cmd  = "Spectre",
    keys = {
      { "<leader>sr",  function() require("spectre").open() end,             desc = "Search & replace (project)" },
      { "<leader>sR",  function() require("spectre").open_file_search() end, desc = "Search & replace (file)" },
    },
    opts = { open_cmd = "noswapfile vnew" },
  },

  -- ── vim-illuminate — highlight word under cursor ─────────────
  {
    "RRethy/vim-illuminate",
    event   = { "BufReadPost", "BufNewFile" },
    config  = function()
      require("illuminate").configure({
        delay = 200,
        large_file_cutoff = 2000,
      })
    end,
  },

  -- ── zen-mode — distraction-free writing ─────────────────────
  {
    "folke/zen-mode.nvim",
    cmd  = "ZenMode",
    keys = { { "<leader>uz", "<cmd>ZenMode<CR>", desc = "Zen mode" } },
    opts = { window = { width = 100 } },
  },

  -- ── Persistence — session management ─────────────────────────
  {
    "folke/persistence.nvim",
    event = "BufReadPre",
    opts  = { dir = vim.fn.stdpath("state") .. "/sessions/" },
    keys  = {
      { "<leader>qs", function() require("persistence").load() end,                desc = "Restore session" },
      { "<leader>ql", function() require("persistence").load({ last = true }) end, desc = "Restore last session" },
      { "<leader>qd", function() require("persistence").stop() end,                desc = "Don't save session" },
    },
  },
}
