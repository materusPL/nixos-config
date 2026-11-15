{ pkgs, config, ... }:
{

  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";
    onShutdown = "shutdown";
    qemu.runAsRoot = true;
    qemu.swtpm.enable = true;
    qemu.package = pkgs.qemu_full;
    qemu.vhostUserPackages = [ pkgs.virtiofsd ];
  };

  virtualisation.spiceUSBRedirection.enable = true;

  environment.systemPackages = with pkgs; [
    virtiofsd
    config.virtualisation.libvirtd.qemu.package
    (looking-glass-client.overrideAttrs (finalAttrs: previousAttrs: {
          patches = [];
          buildInputs = previousAttrs.buildInputs ++
          [
            pkgs.fuse3
            pkgs.libunwind
            pkgs.elfutils
            pkgs.usbredir
          ];
          version = "B7-826-236efcb1";
          src = pkgs.fetchFromGitHub {
            owner = "gnif";
            repo = "LookingGlass";
            rev = "236efcb155f952f5d7d9fcd5891a3060ad254e68";
            hash = "sha256-NAfV4Z0RZp2IGBzVAFysm53aGMEReT03RIN+45TveUU=";
            fetchSubmodules = true;
          };
        }))
    virt-manager
    libguestfs-with-appliance
  ];

  # Packages for QEMU hooks
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

  # Shared qcow drive
  systemd.services.windows-share-mount = {
    wantedBy = [ "multi-user.target" ];
    path = [
      config.virtualisation.libvirtd.qemu.package
      pkgs.util-linux
      pkgs.kmod
      pkgs.coreutils
    ];
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    unitConfig.ConditionPathExists = "/mkk/data/vm/data.qcow2";
    script = ''
      modprobe nbd max_part=10
      sleep 1
      qemu-nbd -c /dev/nbd10 /mkk/data/vm/data.qcow2 --discard=unmap
      sleep 1
      mount /dev/nbd10p1 /mkk/data/mounts/windows -o uid=1000,gid=100
    '';
    preStop = ''
      umount -r /dev/nbd10p1
      qemu-nbd -d /dev/nbd10
    '';
  };

  #Hugepages
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

  virtualisation.libvirtd.qemu.verbatimConfig = ''
    cgroup_device_acl = [
      "/dev/null", "/dev/full", "/dev/zero",
      "/dev/random", "/dev/urandom",
      "/dev/ptmx", "/dev/kvm", "/dev/kqemu",
      "/dev/rtc","/dev/hpet", "/dev/vfio/vfio",
      "/dev/kvmfr0"
    ]
  '';

}
