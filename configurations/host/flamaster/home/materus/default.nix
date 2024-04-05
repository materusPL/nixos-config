{ config, pkgs, materusArg, lib, ... }:
{
  home.stateVersion = "23.05";
  home.homeDirectory = "/home/materus";

  materus.profile = {
    fonts.enable = lib.mkDefault true;
    nixpkgs.enable = lib.mkDefault false;
    enableDesktop = lib.mkDefault true;
    enableTerminal = lib.mkDefault true;
    enableTerminalExtra = lib.mkDefault true;
    enableNixDevel = lib.mkDefault true;

    zsh.enable = true;

  };

  home.packages = [
    pkgs.papirus-icon-theme
    (materusArg.pkgs.polymc.wrap { extraJDKs = [ pkgs.graalvm-ce ]; })
  ];

}
