{
  config,
  lib,
  pkgs,
  ...
}:
{
  options.mkk.neovim = {
    enable = lib.mkOption {
      default = false;
      type = lib.types.bool;
    };
  };
  config =
    # Home Manager
    lib.mkIf config.mkk.neovim.enable {
      xdg.configFile."nvim/lua/materus".source = "${config.mkk.dir}/config/editor/neovim/lua/materus";
      programs.neovim = {
        enable = true;
        withRuby = true;
        withPython3 = true;
        plugins = [
          pkgs.vimPlugins.nvim-treesitter.withAllGrammars
        ];

        initLua = lib.mkAfter ''
          MATERUS = {
            NIXOS = 1
          }
          require("materus")
        '';
      };
      home.packages = [
        pkgs.fd
        pkgs.ripgrep
        pkgs.tree-sitter
      ];
    };
}
