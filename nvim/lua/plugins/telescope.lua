-- ============================================================
-- plugins/telescope.lua — Fuzzy finder (Doom: SPC f …)
-- ============================================================

return {
  {
    "nvim-telescope/telescope.nvim",
    cmd          = "Telescope",
    version      = false,
    dependencies = {
      "nvim-lua/plenary.nvim",
      {
        "nvim-telescope/telescope-fzf-native.nvim",
        build = "make",
        cond  = function() return vim.fn.executable("make") == 1 end,
      },
      "nvim-telescope/telescope-file-browser.nvim",
      "nvim-telescope/telescope-ui-select.nvim",
    },
    keys = {
      -- ── Find ──────────────────────────────────────────────
      { "<leader>ff", "<cmd>Telescope find_files<CR>",             desc = "Find files" },
      { "<leader>fr", "<cmd>Telescope oldfiles<CR>",               desc = "Recent files" },
      { "<leader>fb", "<cmd>Telescope buffers<CR>",                desc = "Buffers" },
      { "<leader>fg", "<cmd>Telescope live_grep<CR>",              desc = "Live grep" },
      { "<leader>fw", "<cmd>Telescope grep_string<CR>",            desc = "Grep word under cursor" },
      { "<leader>fh", "<cmd>Telescope help_tags<CR>",              desc = "Help tags" },
      { "<leader>fk", "<cmd>Telescope keymaps<CR>",                desc = "Keymaps" },
      { "<leader>fc", "<cmd>Telescope command_history<CR>",        desc = "Command history" },
      { "<leader>fm", "<cmd>Telescope marks<CR>",                  desc = "Marks" },
      { "<leader>fe", "<cmd>Telescope file_browser<CR>",           desc = "File browser" },
      -- ── Search ────────────────────────────────────────────
      { "<leader>sd", "<cmd>Telescope diagnostics<CR>",            desc = "Diagnostics" },
      { "<leader>ss", "<cmd>Telescope lsp_document_symbols<CR>",   desc = "Document symbols" },
      { "<leader>sS", "<cmd>Telescope lsp_workspace_symbols<CR>",  desc = "Workspace symbols" },
      -- ── Git ───────────────────────────────────────────────
      { "<leader>gc", "<cmd>Telescope git_commits<CR>",            desc = "Git commits" },
      { "<leader>gb", "<cmd>Telescope git_branches<CR>",           desc = "Git branches" },
      { "<leader>gs", "<cmd>Telescope git_status<CR>",             desc = "Git status" },
      -- ── Misc ──────────────────────────────────────────────
      { "<leader>:",  "<cmd>Telescope commands<CR>",               desc = "Commands" },
      { "<leader>/",  "<cmd>Telescope current_buffer_fuzzy_find<CR>", desc = "Fuzzy buffer" },
    },
    opts = {
      defaults = {
        prompt_prefix   = "   ",
        selection_caret = " ",
        multi_icon      = " ",
        sorting_strategy = "ascending",
        layout_config   = {
          horizontal = { prompt_position = "top", preview_width = 0.55 },
          vertical   = { mirror = false },
          width      = 0.87,
          height     = 0.80,
        },
        file_ignore_patterns = {
          "%.git/", "node_modules/", "target/", "__pycache__/",
          "%.class", "%.o", "%.pyc",
        },
        mappings = {
          i = {
            ["<C-k>"]   = "move_selection_previous",
            ["<C-j>"]   = "move_selection_next",
            ["<C-q>"]   = "send_selected_to_qflist",
            ["<C-Down>"] = "cycle_history_next",
            ["<C-Up>"]   = "cycle_history_prev",
          },
        },
      },
      extensions = {
        fzf = {
          fuzzy                   = true,
          override_generic_sorter = true,
          override_file_sorter    = true,
          case_mode               = "smart_case",
        },
        -- ui-select theme configured in config() below, after telescope loads
      },
    },
    config = function(_, opts)
      local telescope = require("telescope")
      -- Inject ui-select theme here, where telescope.themes is available
      opts.extensions["ui-select"] = {
        require("telescope.themes").get_dropdown(),
      }
      telescope.setup(opts)
      telescope.load_extension("fzf")
      telescope.load_extension("file_browser")
      telescope.load_extension("ui-select")
    end,
  },
}
