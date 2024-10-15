{ config, lib, pkgs, materusArg, ... }:
let
  cfg = config.materus.profile.editor.neovim;
in
{
  options.materus.profile.editor.neovim.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableTerminalExtra "Enable neovim with materus cfg";
  config = lib.mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      package = materusArg.unstable.neovim-unwrapped;
      coc.enable = true;
      coc.package = materusArg.unstable.vimPlugins.coc-nvim;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;
      defaultEditor = true;


      extraConfig = ''
        set number
      '';
      extraLuaConfig = ''
      '';

      extraPackages = with pkgs;[
      ];

      plugins = with materusArg.unstable.vimPlugins;[
        syntastic
        vim-fugitive
        vim-airline
        vim-nix
        nvim-fzf
        nvim-treesitter.withAllGrammars

      ];
    };
  };

}
