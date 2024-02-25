{ config, pkgs, ... }:
{
  imports = [
    ./win10
  ];

  systemd.mounts = [
        { where = "/dev/hugepages";
            enable = false;
        }
        { where = "/dev/hugepages/hugepages-2048kB";
            enable  = true;
            what  = "hugetlbfs";
            type  = "hugetlbfs";
            options = "pagesize=2M";
            requiredBy  = [ "basic.target" ];
        }
        { where = "/dev/hugepages/hugepages-1048576kB";
            enable  = true;
            what  = "hugetlbfs";
            type  = "hugetlbfs";
            options = "pagesize=1G";
            requiredBy  = [ "basic.target" ];
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
          ];
        };
      in
      [ env ];
  };
}
