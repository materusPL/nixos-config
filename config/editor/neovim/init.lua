vim.opt.number = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2
vim.opt.clipboard:append { 'unnamed', 'unnamedplus' } 

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
      "Mofiqul/dracula.nvim",
      lazy = false,
      priority = 1000,
      opts = {}
    },
    {
      "romgrk/barbar.nvim",
      init = function() vim.g.barbar_auto_setup = false end,
      dependencies = {"nvim-tree/nvim-web-devicons","lewis6991/gitsigns.nvim"}
    },

    {
      'nvim-telescope/telescope.nvim', version = '*',
      dependencies = {
        'nvim-lua/plenary.nvim',
      }
    },

    {
      'nvim-treesitter/nvim-treesitter',
      lazy = false,
      build = ':TSUpdate'
    },
    --[[
    {
      "kevinhwang91/nvim-ufo",
      dependencies = {"kevinhwang91/promise-async"},
      opts = {}
    }]]
  }

  require("lazy").setup(plugins, opts)
  if vim.fn.executable("tree-sitter") == 1 then
    require('nvim-treesitter').setup {
      -- Directory to install parsers and queries to (prepended to `runtimepath` to have priority)
      install_dir = vim.fn.stdpath('data') .. '/site',
      highlight = {enable = true},
      indent = { enable = true},
    }
    require('nvim-treesitter').install { 'lua' }
  end
  vim.cmd [[colorscheme dracula]]
end
