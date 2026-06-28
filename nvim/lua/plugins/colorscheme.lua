-- ============================================================
-- plugins/colorscheme.lua — Theme (Catppuccin Mocha)
-- ============================================================
-- Swap the colorscheme call at the bottom to switch themes.
-- Other good options bundled here: tokyonight, kanagawa

return {
  -- Primary theme
  {
    "catppuccin/nvim",
    name     = "catppuccin",
    priority = 1000,   -- load before other plugins
    lazy     = false,
    opts     = {
      flavour             = "mocha",
      background          = { dark = "mocha", light = "latte" },
      transparent_background = false,
      term_colors         = true,
      integrations = {
        cmp          = true,
        gitsigns     = true,
        nvimtree     = false,
        neo_tree     = true,
        telescope    = { enabled = true },
        which_key    = true,
        treesitter   = true,
        mason        = true,
        noice        = true,
        mini         = { enabled = true },
        indent_blankline = { enabled = true },
        lsp_trouble      = true,
      },
    },
    config = function(_, opts)
      require("catppuccin").setup(opts)
      vim.cmd.colorscheme("catppuccin")
    end,
  },

  -- Optional alternatives (uncomment to add, then change colorscheme call above)
  -- { "folke/tokyonight.nvim", lazy = true, priority = 1000 },
  -- { "rebelot/kanagawa.nvim", lazy = true, priority = 1000 },
}
