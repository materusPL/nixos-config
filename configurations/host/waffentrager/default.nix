{ config, pkgs, materusCfg, ... }:
{

  imports = [
    materusCfg.configInputs.inputs.nixos-hardware.nixosModules.raspberry-pi-4
    ./configuration.nix
  ];
}