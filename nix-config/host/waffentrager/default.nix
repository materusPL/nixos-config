{ config, pkgs, ... }:
{

  imports = [
   
    ./configuration.nix
    ./private
    ./services
  ];

  virtualisation.podman.autoPrune.enable = true;
  virtualisation.podman.autoPrune.dates = "daily";
  virtualisation.podman.defaultNetwork.settings = {
    default_subnet = "10.88.0.0/16";
  };
  virtualisation.oci-containers.backend = "podman";

}
