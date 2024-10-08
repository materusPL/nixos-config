# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/profiles/qemu-guest.nix")
    ];

  boot.initrd.availableKernelModules = [ "ata_piix" "virtio_pci" "floppy" "sr_mod" "virtio_blk" ];
  boot.initrd.kernelModules = [ ];
  boot.kernel.sysctl = { "net.ipv4.ip_forward" = 1; "net.ipv6.conf.all.forwarding" = 1; };
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];
  boot.tmp.useTmpfs = true;

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/924b1a69-2256-444f-baf6-d2d9405e451d";
      fsType = "ext4";
    };

  fileSystems."/etc/nixos" =
    {
      device = "/materus/config/nixos-config";
      fsType = "none";
      options = [ "bind" ];
    };

  swapDevices = [
    {
      device = "/swapfile";
      size = 4 * 1024;
    }
  ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = false;
  networking.nameservers = [ "9.9.9.9" "1.1.1.1" "8.8.8.8" ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
