isHm:
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
    if isHm then
      # Home Manager
      lib.mkIf config.mkk.neovim.enable {
        xdg.configFile."nvim/init.lua".source = "${config.mkk.dir}/config/editor/neovim/init.lua";
        home.packages = [
          pkgs.neovim
          pkgs.neovide
          pkgs.fd
          pkgs.ripgrep
          pkgs.tree-sitter
        ];
      }
    else
      # NixOS
      lib.mkIf config.mkk.neovim.enable {

        environment.systemPackages = [
          pkgs.neovim
          pkgs.neovide
        ];
      };
}
