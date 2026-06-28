-- ============================================================
-- plugins/treesitter.lua — Syntax highlighting & text objects
-- ============================================================

return {
  {
    "nvim-treesitter/nvim-treesitter",
    build        = ":TSUpdate",
    event        = { "BufReadPost", "BufNewFile" },
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",
    },
    opts = {
      ensure_installed = {
        "bash", "c", "cmake", "css",
        "dockerfile", "go", "html", "http",
        "javascript", "json", "json5", "jsonc",
        "lua", "make", "markdown", "markdown_inline",
        "python", "regex", "rust", "sql",
        "toml", "typescript", "vim", "vimdoc",
        "yaml",
      },
      auto_install = true,
      highlight    = { enable = true },
      indent       = { enable = true },
      incremental_selection = {
        enable  = true,
        keymaps = {
          init_selection    = "<C-space>",
          node_incremental  = "<C-space>",
          scope_incremental = "<C-s>",
          node_decremental  = "<M-space>",
        },
      },
      textobjects = {
        select = {
          enable    = true,
          lookahead = true,
          keymaps   = {
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ac"] = "@class.outer",
            ["ic"] = "@class.inner",
            ["aa"] = "@parameter.outer",
            ["ia"] = "@parameter.inner",
          },
        },
        move = {
          enable              = true,
          set_jumps           = true,
          goto_next_start     = { ["]f"] = "@function.outer", ["]c"] = "@class.outer" },
          goto_next_end       = { ["]F"] = "@function.outer", ["]C"] = "@class.outer" },
          goto_previous_start = { ["[f"] = "@function.outer", ["[c"] = "@class.outer" },
          goto_previous_end   = { ["[F"] = "@function.outer", ["[C"] = "@class.outer" },
        },
        swap = {
          enable        = true,
          swap_next     = { ["<leader>a"] = "@parameter.inner" },
          swap_previous = { ["<leader>A"] = "@parameter.inner" },
        },
      },
    },
    config = function(_, opts)
      -- Guard: configs module may not exist if treesitter hasn't built yet
      local ok, ts_configs = pcall(require, "nvim-treesitter.configs")
      if not ok then
        vim.notify("nvim-treesitter not ready — run :TSUpdate", vim.log.levels.WARN)
        return
      end
      ts_configs.setup(opts)
    end,
  },

  -- Sticky context header — depends on treesitter being loaded first
  {
    "nvim-treesitter/nvim-treesitter-context",
    event        = { "BufReadPost", "BufNewFile" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    opts         = { max_lines = 3, multiline_threshold = 2 },
  },
}
