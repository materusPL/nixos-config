{ config, lib, pkgs, materusPkgs, ... }:
let
cfg = config.materus.profile.editor.neovim;
in
{
  options.materus.profile.editor.neovim.enable = materusPkgs.lib.mkBoolOpt config.materus.profile.enableTerminalExtra "Enable neovim with materus cfg";
  config = lib.mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      coc.enable = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;


      extraConfig = ''
      set number
      '';
    

    plugins = with pkgs.vimPlugins;[
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
