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


    --[[
    {
      "kevinhwang91/nvim-ufo",
      dependencies = {"kevinhwang91/promise-async"},
      opts = {}
    }]]
  }

  require("lazy").setup(plugins, opts)

  vim.cmd [[colorscheme dracula]]
  vim.cmd [[set number]]
end
