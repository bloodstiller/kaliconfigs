-- ============================================================
-- plugins/markdown.lua — In-buffer markdown rendering + images
-- ============================================================

return {

  -- ── render-markdown.nvim — render markdown visually in the buffer ──
  {
    "MeanderingProgrammer/render-markdown.nvim",
    ft           = { "markdown" },
    dependencies = { "nvim-treesitter/nvim-treesitter", "nvim-tree/nvim-web-devicons" },
    opts = {
      heading = {
        enabled = true,
        sign    = true,
        icons   = { "󰲡 ", "󰲣 ", "󰲥 ", "󰲧 ", "󰲩 ", "󰲫 " },
      },
      code = {
        enabled = true,
        sign    = false,
        style   = "full",
        border  = "thin",
      },
      dash     = { enabled = true },
      bullet   = { enabled = true },
      checkbox = {
        enabled   = true,
        unchecked = { icon = "󰄱 " },
        checked   = { icon = "󰱒 " },
      },
      table = { enabled = true },
      quote = { enabled = true },
      callout = {
        note   = { raw = "[!NOTE]",    rendered = "󰋽 Note",    highlight = "RenderMarkdownInfo" },
        tip    = { raw = "[!TIP]",     rendered = "󰌶 Tip",     highlight = "RenderMarkdownSuccess" },
        warn   = { raw = "[!WARNING]", rendered = "󰀪 Warning", highlight = "RenderMarkdownWarn" },
        danger = { raw = "[!DANGER]",  rendered = "󱐌 Danger",  highlight = "RenderMarkdownError" },
      },
    },
  },

  -- ── image.nvim — inline images via Kitty Graphics Protocol ──────
  -- Requires Kitty terminal + luarocks magick:
  --   sudo apt install luarocks libmagickwand-dev && luarocks install magick
  {
    "3rd/image.nvim",
    build = false,
    ft    = { "markdown" },
    opts  = {
      backend = "kitty",
      integrations = {
        markdown = {
          enabled                    = true,
          clear_in_insert_mode       = false,
          download_remote_images     = true,
          only_render_image_at_cursor = false,
          filetypes                  = { "markdown" },
        },
      },
      max_width                          = 100,
      max_height                         = 12,
      max_width_window_percentage        = math.huge,
      max_height_window_percentage       = 50,
      window_overlap_clear_enabled       = true,
      window_overlap_clear_ft_ignore     = { "cmp_menu", "cmp_docs", "" },
      editor_only_render_when_focused    = false,
      tmux_show_only_in_active_window    = true,
      hijack_file_patterns               = { "*.png", "*.jpg", "*.gif", "*.webp", "*.avif" },
    },
  },

}
