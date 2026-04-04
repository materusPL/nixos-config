vim.opt.number = true
vim.opt.expandtab = true
vim.opt.cursorline = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2
vim.opt.clipboard:append { 'unnamed', 'unnamedplus' }


--vim.g.cord_defer_startup = true


local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if vim.fn.executable("git") == 1 then
  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable", -- latest stable release
      lazypath,
    })
  end
  vim.opt.rtp:prepend(lazypath)

  local opts = {}
  local plugins = {
    {
      "folke/lazydev.nvim",
      ft = "lua", -- only load on lua files
      opts = {
        library = {
          -- See the configuration section for more details
          -- Load luvit types when the `vim.uv` word is found
          { path = "${3rd}/luv/library", words = { "vim%.uv" } },
        },
      },
    },
    { -- optional cmp completion source for require statements and module annotations
      "hrsh7th/nvim-cmp",
      opts = function(_, opts)
        opts.sources = opts.sources or {}
        table.insert(opts.sources, {
          name = "lazydev",
          group_index = 0, -- set group index to 0 to skip loading LuaLS completions
        })
      end,
    },

    { 
      'https://github.com/neovim/nvim-lspconfig' 
    },
    {
      'stevearc/oil.nvim',
      ---@module 'oil'
      ---@type oil.setupopts
      opts = {},
      -- optional dependencies
      dependencies = { { "nvim-mini/mini.icons", opts = {} } },
      -- dependencies = { "nvim-tree/nvim-web-devicons" }, -- use if you prefer nvim-web-devicons
      -- lazy loading is not recommended because it is very tricky to make it work correctly in all situations.
      lazy = false,
    },
    {
      "nvim-neo-tree/neo-tree.nvim",
      branch = "v3.x",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "muniftanjim/nui.nvim",
        "nvim-tree/nvim-web-devicons", 
      },
      lazy = false, 
    },
    {
      "Mofiqul/dracula.nvim",
      lazy = false,
      priority = 1000,
      opts = {},
      config = function()
        vim.cmd.colorscheme("dracula")
      end
    },
    {
      "romgrk/barbar.nvim",
      init = function() vim.g.barbar_auto_setup = true end,
      dependencies = { "nvim-tree/nvim-web-devicons", "lewis6991/gitsigns.nvim" }
    },
    {
      'lewis6991/gitsigns.nvim'
    },
    {
      'nvim-telescope/telescope.nvim',
      version = '*',
      dependencies = {
        'nvim-lua/plenary.nvim',
      }
    },
    {
      'lambdalisue/vim-suda'
    },
    {
      'vyfor/cord.nvim'
    },
    {
      "dstein64/nvim-scrollview"
    },
    {
      "Isrothy/neominimap.nvim",
      version = "v3.x.x",
      lazy = false,
      init = function()
        vim.opt.wrap = true
        vim.opt.sidescrolloff = 36 
        vim.g.neominimap = {
          auto_enable = false,  
          layout = "float",
          click = {
            enabled = true,
            auto_switch_focus = false,
          },
        }
      end,
    },
    --[[
    {
      "kevinhwang91/nvim-ufo",
      dependencies = {"kevinhwang91/promise-async"},
      opts = {}
    }]]
  }

  
  if vim.fn.executable("tree-sitter") == 1 and MATERUS.NIXOS ~= 1 then
    table.insert(plugins, {
      'nvim-treesitter/nvim-treesitter',
      lazy = false,
      build = ':TSUpdate'
    })
  end
  
  require("lazy").setup(plugins, opts)
  if vim.fn.executable("tree-sitter") == 1 and MATERUS.NIXOS ~= 1 then
    require('nvim-treesitter').setup {
      -- Directory to install parsers and queries to (prepended to `runtimepath` to have priority)
      install_dir = vim.fn.stdpath('data') .. '/site',
      highlight = { enable = true },
      indent = { enable = true },
    }
    require('nvim-treesitter').install { 'lua' }
  end
end
