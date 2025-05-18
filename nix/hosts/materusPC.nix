# * materusPC
{
  lib,
  pkgs,
  config,
  konfig,
  ...
}:
{
  imports = [
# * CONFIG
# **  Nix System Settings
    {
      nixpkgs.hostPlatform = "x86_64-linux";
      system.copySystemConfiguration = false;
      system.stateVersion = "23.05";
    }
# ** Network
    {
      networking.hostName = "materusPC";
      networking.useDHCP = lib.mkDefault true;
      networking.wireless.iwd.enable = true;
      networking.networkmanager.enable = true;
      #networking.networkmanager.wifi.backend = "iwd";
      networking.firewall.enable = true;

      networking.firewall = {
        logReversePathDrops = false;
        # wireguard trips rpfilter up
        extraCommands = ''
          ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --sport ${konfig.vars.wireguard.ports.materusPC} -j RETURN
          ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --dport ${konfig.vars.wireguard.ports.materusPC} -j RETURN
        '';
        extraStopCommands = ''
          ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --sport ${konfig.vars.wireguard.ports.materusPC} -j RETURN || true
          ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --dport ${konfig.vars.wireguard.ports.materusPC} -j RETURN || true
        '';
      };

    }
# ** Hardware
# *** Filesystems
    {
      zramSwap = {
        enable = true;
        memoryPercent = 25;
      };

      swapDevices = [
        {
          label = "NixOS_Swap";
        }
      ];

      fileSystems."/etc/nixos" = {
        device = "/materus/config/mkk";
        fsType = "none";
        options = [ "bind" ];
        depends = [ "/materus" ];
      };

      fileSystems."/materus" = {
        device = "/dev/disk/by-label/NixOS_Root";
        fsType = "btrfs";
        options = [
          "subvol=@materus"
          "noatime"
          "compress=zstd"
          "ssd"
          "space_cache=v2"
        ];
        neededForBoot = true;
      };

      fileSystems."/" = {
        device = "/dev/disk/by-label/NixOS_Root";
        fsType = "btrfs";
        options = [
          "subvol=@"
          "noatime"
          "ssd"
          "space_cache=v2"
          "compress=zstd"
        ];
      };

      fileSystems."/nix" = {
        device = "/dev/disk/by-label/NixOS_Root";
        fsType = "btrfs";
        options = [
          "subvol=@nix"
          "noatime"
          "compress=zstd"
          "ssd"
          "space_cache=v2"
        ];
      };

      fileSystems."/home" = {
        device = "/dev/disk/by-label/NixOS_Root";
        fsType = "btrfs";
        options = [
          "subvol=@home"
          "noatime"
          "compress=zstd"
          "ssd"
          "space_cache=v2"
        ];
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-label/NixOS_Root";
        fsType = "btrfs";
        options = [
          "subvol=@boot"
          "ssd"
        ];
      };

      fileSystems."/boot/efi" = {
        device = "/dev/disk/by-label/NixOS_EFI";
        fsType = "vfat";
      };
    }
# *** Kernel & Boot
    {
      boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_zen;

      boot.kernelParams = [
        #"rcu_nocbs=${materusArg.materusPC.vmCores}"
        #"nohz_full=${materusArg.materusPC.vmCores}"
        "vfio_iommu_type1.allow_unsafe_interrupts=1"
        "pcie_acs_override=downstream,multifunction"
        #''vfio-pci.ids="1002:744c"''
        "nox2apic"
        "nvme_core.default_ps_max_latency_us=0"
        "nvme_core.io_timeout=255"
        "nvme_core.max_retries=10"
        "nvme_core.shutdown_timeout=10"
        "amd_iommu=on"
        "amdgpu.ppfeaturemask=0xffffffff"
        "amdgpu.runpm=0"
        "iommu=pt"
        "psi=1"
        # Intel Arc A310
        "i915.force_probe=!56a6"
        "xe.force_probe=56a6"
        # Video
        "video=HDMI-A-3:1920x1080@144"
        "video=DP-3:1920x1080@240"
      ];
      boot.kernelModules = [
        "pci-stub"
        "amdgpu"
        "i2c_dev"
        "kvm_amd"
        "vfio"
        "vfio_iommu_type1"
        "vfio-pci"
        "kvmfr"
        "xe"
      ];
      boot.extraModprobeConfig = ''
        options kvm_amd nested=1 avic=1 npt=1 sev=0
        options vfio_iommu_type1 allow_unsafe_interrupts=1
        options kvmfr static_size_mb=64
      '';
      boot.kernel.sysctl = {
        "vm.max_map_count" = 1000000;
        "vm.swappiness" = 10;
        "net.ipv4.ip_forward" = 1;
      };

      boot.initrd.availableKernelModules = [
        "xhci_pci"
        "ahci"
        "nvme"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];
      boot.initrd.kernelModules = [
        "vfio-pci"
        "amdgpu"
      ];

      boot.extraModulePackages = with config.boot.kernelPackages; [
        v4l2loopback
        kvmfr
      ];

      boot.supportedFilesystems = [
        "ntfs"
        "btrfs"
        "vfat"
        "exfat"
        "ext4"
      ];

      boot.tmp.useTmpfs = true;

      #bootloader
      boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.efi.efiSysMountPoint = "/boot/efi";
      boot.loader.grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
        gfxmodeEfi = pkgs.lib.mkDefault "1920x1080@240";
        gfxmodeBios = pkgs.lib.mkDefault "1920x1080@240";
        useOSProber = true;
        memtest86.enable = true;
      };

      boot.plymouth.enable = true;

      boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
    }

# *** Firmware & Others
    {
      hardware.firmware = with pkgs; [
        konfig.nixerusPkgs.amdgpu-pro-libs.firmware.vcn
        konfig.nixerusPkgs.amdgpu-pro-libs.firmware
        linux-firmware
        alsa-firmware
        sof-firmware
      ];

      environment.variables = {
        DISABLE_LAYER_AMD_SWITCHABLE_GRAPHICS_1 = "1";
        #VK_ICD_FILENAMES = "${pkgs.mesa.drivers}/share/vulkan/icd.d/radeon_icd.x86_64.json:${pkgs.driversi686Linux.mesa.drivers}/share/vulkan/icd.d/radeon_icd.i686.json";
        AMD_VULKAN_ICD = "RADV";
        RADV_PERFTEST = "gpl,rt,sam";
        #OCL_ICD_VENDORS = "${pkgs.rocmPackages.clr.icd}/etc/OpenCL/vendors/";
      };
      hardware.cpu.amd.updateMicrocode = lib.mkForce true;

      #extra
      hardware.wooting.enable = true;
      hardware.bluetooth.enable = true;
      #Graphics
      hardware.graphics.enable = true;
      hardware.graphics.enable32Bit = true;
      hardware.graphics.extraPackages = with pkgs; [
        vaapiVdpau
        vpl-gpu-rt
        intel-media-driver
        libvdpau-va-gl
        amdvlk
        vkbasalt
        rocmPackages.clr.icd
        rocmPackages.clr
        konfig.nixerusPkgs.amdgpu-pro-libs.vulkan
        konfig.nixerusPkgs.amdgpu-pro-libs.amf
      ];
      hardware.graphics.extraPackages32 = with pkgs.pkgsi686Linux; [
        vaapiVdpau
        vkbasalt
        pkgs.driversi686Linux.amdvlk
        konfig.nixerusPkgs.i686Linux.amdgpu-pro-libs.vulkan
        libvdpau-va-gl
      ];
      /*
        services.udev.extraRules = ''

          #GPU bar size
          ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x1002", ATTR{device}=="0x744c", ATTR{resource0_resize}="15"
          ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x1002", ATTR{device}=="0x744c", ATTR{resource2_resize}="8"
        '';
      */

      #Trim
      services.fstrim = {
        enable = true;
        interval = "weekly";
      };
    }
# * materusPC END
  ];
}
