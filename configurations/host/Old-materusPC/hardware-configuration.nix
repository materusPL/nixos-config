# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" "vfio-pci" ];
  boot.extraModulePackages = [ ];
  boot.kernel.sysctl = { "vm.swappiness" = 10; };
  boot.kernelParams = [ "ibt=off" "intel_iommu=on" "iommu=pt" "pcie_acs_override=downstream,multifunction" ];
  fileSystems."/" =
    {
      device = "/dev/disk/by-label/NixOS_Root";
      fsType = "btrfs";
      options = [ "subvol=@" "noatime" "ssd" "space_cache=v2" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-label/NixOS_Root";
      fsType = "btrfs";
      options = [ "subvol=@boot" "ssd" ];
    };

  fileSystems."/materus" =
    {
      device = "/dev/disk/by-label/NixOS_Root";
      fsType = "btrfs";
      options = [ "subvol=@materus" "noatime" "compress=zstd" "ssd" "space_cache=v2" ];
    };

  fileSystems."/nix" =
    {
      device = "/dev/disk/by-label/NixOS_Root";
      fsType = "btrfs";
      options = [ "subvol=@nix" "noatime" "compress=zstd" "ssd" "space_cache=v2" ];
    };

  fileSystems."/home" =
    {
      device = "/dev/disk/by-label/NixOS_Home";
      fsType = "btrfs";
      options = [ "subvol=@home" "nossd" "noatime" "compress=zstd" "space_cache=v2" "autodefrag" ];
    };

  fileSystems."/materus/data" =
    {
      device = "/dev/disk/by-label/NixOS_Home";
      fsType = "btrfs";
      options = [ "subvol=@data" "nossd" "noatime" "compress=zstd" "space_cache=v2" "autodefrag" ];
    };

  fileSystems."/boot/efi" =
    {
      device = "/dev/disk/by-uuid/A5C2-31D1";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-label/NixOS_Swap"; }];

  fileSystems."/etc/nixos" =
    {
      device = "/materus/config/nixos-config";
      fsType = "none";
      options = [ "bind" ];
    };



  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp3s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp6s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkForce true;
}
