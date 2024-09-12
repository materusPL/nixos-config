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
  virtualisation.podman.defaultNetwork.settings = {
    default_subnet = "10.88.0.0/16";
  };
  virtualisation.oci-containers.backend = "podman";

}
