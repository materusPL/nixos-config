{ config, pkgs, lib, materusArg, materusCfg, ... }:
let
  cfg = config.materus.profile.nix;
in
{
  options.materus.profile.nix.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable materus nix settings";
  config.nix = lib.mkIf cfg.enable {

    settings = {
      experimental-features = lib.mkMerge [[ "nix-command" "flakes" "no-url-literals" ]];
      auto-optimise-store = true;
      trusted-users = [ "root" "@wheel" ];

      substituters = [
        "https://nix-community.cachix.org"
        "https://cache.nixos.org/"
        "https://nixerus.cachix.org/"
      ];
      trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" "nixerus.cachix.org-1:2x7sIG7y1vAoxc8BNRJwsfapZsiX4hIl4aTi9V5ZDdE=" ];
    };
  };
}
