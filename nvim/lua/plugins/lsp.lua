-- ============================================================
-- plugins/lsp.lua — LSP (mason + native vim.lsp + none-ls)
-- Nvim 0.11+ API: vim.lsp.config / vim.lsp.enable
-- No more require('lspconfig')[server].setup() calls
-- ============================================================

return {

  -- ── Mason: LSP / DAP / linter installer ────────────────────
  {
    "williamboman/mason.nvim",
    cmd  = "Mason",
    keys = { { "<leader>cm", "<cmd>Mason<CR>", desc = "Open Mason" } },
    opts = {
      ui = {
        border = "rounded",
        icons  = {
          package_installed   = "✓",
          package_pending     = "➜",
          package_uninstalled = "✗",
        },
      },
    },
  },

  -- ── mason-lspconfig: installs servers via Mason ─────────────
  -- Only used for installation now, NOT for setup_handlers
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "lua_ls",
        "pyright",
        "bashls",
        "jsonls",
        "yamlls",
        "dockerls",
        "marksman",
      },
      automatic_installation = true,
    },
  },

  -- ── nvim-lspconfig (kept for server definitions only) ───────
  -- We use it purely as a source of default cmd/filetypes/root_markers.
  -- All actual config goes through vim.lsp.config (0.11 API).
  {
    "neovim/nvim-lspconfig",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
      { "folke/neodev.nvim", opts = {} },
    },
    config = function()
      -- ── Capabilities ────────────────────────────────────────
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      -- ── on_attach (runs for every server) ───────────────────
      local on_attach = function(_, bufnr)
        local map = function(keys, func, desc)
          vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
        end
        local lsp = vim.lsp.buf

        map("gd",         lsp.definition,      "Go to definition")
        map("gD",         lsp.declaration,     "Go to declaration")
        map("gr",         "<cmd>Telescope lsp_references<CR>",        "References")
        map("gI",         lsp.implementation,  "Go to implementation")
        map("K",          lsp.hover,           "Hover docs")
        map("<C-k>",      lsp.signature_help,  "Signature help")
        map("<leader>cr", lsp.rename,          "Rename symbol")
        map("<leader>ca", lsp.code_action,     "Code action")
        map("<leader>cf", function() lsp.format({ async = true }) end, "Format buffer")
        map("<leader>lt", lsp.type_definition, "Type definition")
        map("<leader>ld", "<cmd>Telescope lsp_document_symbols<CR>",  "Document symbols")
        map("<leader>lw", "<cmd>Telescope lsp_workspace_symbols<CR>", "Workspace symbols")
      end

      -- ── Per-server settings (0.11 vim.lsp.config API) ───────
      -- vim.lsp.config merges with the server's built-in defaults.
      -- Call vim.lsp.enable() to activate each one.

      vim.lsp.config("lua_ls", {
        capabilities = capabilities,
        on_attach    = on_attach,
        settings = {
          Lua = {
            workspace   = { checkThirdParty = false },
            telemetry   = { enable = false },
            diagnostics = { globals = { "vim" } },
          },
        },
      })

      vim.lsp.config("pyright", {
        capabilities = capabilities,
        on_attach    = on_attach,
      })

      vim.lsp.config("bashls", {
        capabilities = capabilities,
        on_attach    = on_attach,
      })

      vim.lsp.config("jsonls", {
        capabilities = capabilities,
        on_attach    = on_attach,
      })

      vim.lsp.config("yamlls", {
        capabilities = capabilities,
        on_attach    = on_attach,
        settings = {
          yaml = { keyOrdering = false },
        },
      })

      vim.lsp.config("dockerls", {
        capabilities = capabilities,
        on_attach    = on_attach,
      })

      vim.lsp.config("marksman", {
        capabilities = capabilities,
        on_attach    = on_attach,
      })

      -- Enable all configured servers
      vim.lsp.enable({
        "lua_ls",
        "pyright",
        "bashls",
        "jsonls",
        "yamlls",
        "dockerls",
        "marksman",
      })

      -- ── Diagnostics UI ──────────────────────────────────────
      vim.diagnostic.config({
        virtual_text     = { prefix = "●" },
        signs            = true,
        underline        = true,
        update_in_insert = false,
        severity_sort    = true,
        float = {
          focusable = false,
          style     = "minimal",
          border    = "rounded",
          source    = "always",
          header    = "",
          prefix    = "",
        },
      })

      local signs = { Error = " ", Warn = " ", Hint = "󰠠 ", Info = " " }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end
    end,
  },

  -- ── mason-null-ls: auto-install formatters/linters ──────────
  {
    "jay-babu/mason-null-ls.nvim",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = { "williamboman/mason.nvim", "nvimtools/none-ls.nvim" },
    opts = {
      ensure_installed = {
        "stylua",
        "black",
        "isort",
        "shfmt",
        "prettier",
        "shellcheck",
        "hadolint",
      },
      automatic_installation = true,
    },
  },

  -- ── none-ls: formatters & extra linters ─────────────────────
  {
    "nvimtools/none-ls.nvim",
    event        = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "williamboman/mason.nvim",
      "jay-babu/mason-null-ls.nvim",
    },
    config = function()
      local ok, nls = pcall(require, "null-ls")
      if not ok then return end

      -- Helper: only register a source if the binary is executable
      local function exe(name) return vim.fn.executable(name) == 1 end

      local sources = {}

      if exe("stylua")    then table.insert(sources, nls.builtins.formatting.stylua)    end
      if exe("black")     then table.insert(sources, nls.builtins.formatting.black)     end
      if exe("isort")     then table.insert(sources, nls.builtins.formatting.isort)     end
      if exe("shfmt")     then table.insert(sources, nls.builtins.formatting.shfmt)     end
      if exe("prettier")  then
        table.insert(sources, nls.builtins.formatting.prettier.with({
          filetypes = { "json", "yaml", "markdown", "html", "css",
                        "javascript", "typescript" },
        }))
      end
      if exe("shellcheck") then
        table.insert(sources, nls.builtins.diagnostics.shellcheck)
      end
      if exe("hadolint")  then
        table.insert(sources, nls.builtins.diagnostics.hadolint)
      end

      nls.setup({
        sources   = sources,
        on_attach = function(_, bufnr)
          vim.keymap.set("n", "<leader>cf",
            function() vim.lsp.buf.format({ async = true }) end,
            { buffer = bufnr, desc = "Format (none-ls)" })
        end,
      })
    end,
  },

  -- ── Trouble — pretty diagnostics list ───────────────────────
  {
    "folke/trouble.nvim",
    dependencies = "nvim-tree/nvim-web-devicons",
    cmd  = { "Trouble", "TroubleToggle" },
    keys = {
      { "<leader>xx", "<cmd>Trouble diagnostics toggle<CR>",              desc = "Workspace diagnostics" },
      { "<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<CR>", desc = "Buffer diagnostics" },
      { "<leader>xq", "<cmd>Trouble qflist toggle<CR>",                   desc = "Quickfix list" },
      { "<leader>xl", "<cmd>Trouble loclist toggle<CR>",                  desc = "Location list" },
    },
    opts = { use_diagnostic_signs = true },
  },
}
