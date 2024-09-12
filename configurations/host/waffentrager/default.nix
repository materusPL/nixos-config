{ config, pkgs, materusCfg, ... }:
{

  imports = [
    materusCfg.configInputs.nixos-hardware.nixosModules.raspberry-pi-4
    ./configuration.nix
    ./secrets
    ./services
  ];

  virtualisation.podman.autoPrune.enable = true;
  virtualisation.podman.autoPrune.dates = "daily";
  virtualisation.oci-containers.backend = "podman";

}
