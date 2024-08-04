{ config, pkgs, ... }:
{
  imports = [
    ./win-vfio
  ];
  materus.materusArg.materusPC = {
    allCores = "0-31";
    allCoresMask = "ffffffff";
    hostCores = "0-7,16-23";
    hostCoresMask = "00ff00ff";
    vmCores = "8-15,24-31";
    vmCoresMask = "ff00ff00";
  };
  systemd.mounts = [
    {
      where = "/dev/hugepages";
      enable = false;
    }
    {
      where = "/dev/hugepages/hugepages-2048kB";
      enable = true;
      what = "hugetlbfs";
      type = "hugetlbfs";
      options = "pagesize=2M";
      requiredBy = [ "basic.target" ];
    }
    {
      where = "/dev/hugepages/hugepages-1048576kB";
      enable = true;
      what = "hugetlbfs";
      type = "hugetlbfs";
      options = "pagesize=1G";
      requiredBy = [ "basic.target" ];
    }
  ];

  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";
    onShutdown = "shutdown";
    qemu.ovmf.enable = true;
    qemu.ovmf.packages = [ pkgs.OVMFFull.fd ];
    qemu.runAsRoot = true;
    qemu.swtpm.enable = true;
    qemu.package = pkgs.qemu_full;
  };

  virtualisation.spiceUSBRedirection.enable = true;

  environment.systemPackages = with pkgs; [
    virtiofsd
    config.virtualisation.libvirtd.qemu.package
    looking-glass-client
    virt-manager
    libguestfs-with-appliance
  ];

  systemd.services.libvirtd = {
    path =
      let
        env = pkgs.buildEnv {
          name = "qemu-hook-env";
          paths = with pkgs; [
            bash
            libvirt
            kmod
            systemd
            ripgrep
            sd
            coreutils
            sudo
            su
            killall
            procps
            util-linux
            bindfs
            qemu-utils
            psmisc
            procps
          ];
        };
      in
      [ env ];
  };
}
