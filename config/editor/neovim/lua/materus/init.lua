vim.opt.number = true
vim.opt.expandtab = true
vim.opt.cursorline = true
vim.opt.shiftwidth = 2
vim.opt.softtabstop = 2
vim.opt.tabstop = 2
vim.opt.clipboard:append { 'unnamed', 'unnamedplus' }


--vim.g.cord_defer_startup = true

vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/muniftanjim/nui.nvim",
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/mofiqul/dracula.nvim",
  "https://github.com/hrsh7th/nvim-cmp",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/stevearc/oil.nvim",
  "https://github.com/nvim-mini/mini.icons",
  "https://github.com/nvim-neo-tree/neo-tree.nvim",
  "https://github.com/romgrk/barbar.nvim",
  "https://github.com/nvim-telescope/telescope.nvim",
  "https://github.com/lambdalisue/vim-suda",
  "https://github.com/dstein64/nvim-scrollview",
  "https://github.com/vyfor/cord.nvim"
})
vim.cmd.colorscheme("dracula")
  
if vim.fn.executable("tree-sitter") == 1 and MATERUS.NIXOS ~= 1 then
end
