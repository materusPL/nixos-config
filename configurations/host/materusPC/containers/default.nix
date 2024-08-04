{...}:
{
  virtualisation.lxc.enable = true;
  virtualisation.lxc.lxcfs.enable = true;
  virtualisation.lxd.enable = false;

  virtualisation.waydroid.enable = true;
  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };
}