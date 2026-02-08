{ ... }:
{
  imports = [
    ./nspawn/arch.nix
  ];

  virtualisation.lxc.enable = true;
  virtualisation.lxc.lxcfs.enable = true;

  virtualisation.waydroid.enable = true;
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };
}
