{ config, pkgs, materusCfg, ... }:
{

  imports = [
    materusCfg.configInputs.nixos-hardware.nixosModules.raspberry-pi-4
    ./configuration.nix
  ];
}
