{ ... }:
{
  imports = [
    ./configuration.nix

    ./audio.nix
    ./network.nix
    ./services.nix

    ./virtualization/libvirt.nix
    ./virtualization/vfio.nix
    ./virtualization/containers.nix
    ./hardware-configuration.nix

    ./private
  ];
}
