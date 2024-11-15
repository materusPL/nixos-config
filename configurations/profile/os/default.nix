{ config, pkgs, lib, ... }: {
  imports = [
    ./nix.nix
    ./fonts.nix

    ./shell
    ./games
    ./desktop
  ];

  time.timeZone = lib.mkDefault "Europe/Warsaw";
  i18n.defaultLocale = lib.mkDefault "pl_PL.UTF-8";

  console = {
    enable = lib.mkDefault true;
    earlySetup = lib.mkDefault true;
    font = lib.mkDefault "LatArCyrHeb-16";
    keyMap = lib.mkDefault "pl";
  };
  environment.sessionVariables = {
    MATERUS_CONFIG_DIR = lib.mkDefault "/etc/nixos/";
  };

  programs.tmux = {
    enable = lib.mkDefault true;
    clock24 = lib.mkDefault true;
  };
  environment.systemPackages =
    lib.mkIf config.programs.tmux.enable [ pkgs.tmux.terminfo ];

}

  
