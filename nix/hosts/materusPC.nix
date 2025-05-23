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
# ** General Settings
# *** SOPS
    {
      sops.age.generateKey = false;
      sops.gnupg.home = null;
      sops.gnupg.sshKeyPaths = [ ];
      sops.age.sshKeyPaths = [ (konfig.vars.path.mkk + "/host/keys/ssh_host_ed25519_key") ];
      sops.defaultSopsFile = konfig.rootFlake + "/private/materusPC-secrets.yaml";
      #sops.secrets."users/materus" = { neededForUsers = true; };
      sops.secrets.wireguard = { };

      services.openssh.hostKeys = [
        {
          bits = 4096;
          path = konfig.vars.path.mkk + "/host/keys/ssh_host_rsa_key";
          type = "rsa";
        }
        {
          path = konfig.vars.path.mkk + "/host/keys/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
    }
# *** Nix System Settings
    {
      nixpkgs.hostPlatform = "x86_64-linux";
      system.copySystemConfiguration = false;
      system.stateVersion = "23.05";

    }
# *** Users
    {
      users.users.materus = {
        isNormalUser = true;
        extraGroups = [
          "audio"
          "video"
          "render"
          "pipewire"
          "wheel"
          "networkmanager"
          "input"
          "kvm"
          "libvirt-qemu"
          "libvirt"
          "libvirtd"
          "podman"
          "scanner"
          "lp"
        ];
        shell = pkgs.zsh;
        description = "Mateusz Słodkowicz";
        openssh.authorizedKeys.keyFiles = [ ("${konfig.rootFlake}" + "/private/pubkeys/materus.pub") ];
        #hashedPasswordFile = config.sops.secrets."users/materus".path;
      };
    }
# *** Audio
    {
      security.rtkit.enable = true;
      services.pipewire = {
        enable = true;
        audio.enable = true;
        alsa.enable = true;
        alsa.support32Bit = true;
        pulse.enable = true;
        systemWide = true;
        jack.enable = true;
      };

      services.pulseaudio.enable = false;
      environment.sessionVariables =
        let
          makePluginPath =
            format:
            "$HOME/.${format}:"
            + (lib.makeSearchPath format [
              "$HOME/.nix-profile/lib"
              "/run/current-system/sw/lib"
              "/etc/profiles/per-user/$USER/lib"
            ]);
        in
        {
          ALSOFT_DRIVERS = "pulse";

          DSSI_PATH = makePluginPath "dssi";
          LADSPA_PATH = makePluginPath "ladspa";
          LV2_PATH = makePluginPath "lv2";
          LXVST_PATH = makePluginPath "lxvst";
          VST_PATH = makePluginPath "vst";
          VST3_PATH = makePluginPath "vst3";

        };
      services.udev.extraRules = ''
        KERNEL=="rtc0", GROUP="audio"
        KERNEL=="hpet", GROUP="audio"
        DEVPATH=="/devices/virtual/misc/cpu_dma_latency", OWNER="root", GROUP="audio", MODE="0660"
      '';

      environment.systemPackages = with pkgs; [
        openal
        pulseaudio

        reaper
        audacity

        yabridge
        yabridgectl

        vital
        odin2
        surge
        fire
        decent-sampler
        lsp-plugins

      ];
    }
# *** Other
    {
      mkk.os.fonts.enable = true;
    }
# ** Desktop
# *** XDG
    {
      xdg.portal.enable = true;
      xdg.portal.wlr.enable = true;
      xdg.portal.xdgOpenUsePortal = true;
      xdg.portal.extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
      
      xdg.portal.config.common.default = "*";
      

      environment.sessionVariables = {
        XDG_CACHE_HOME = "\${HOME}/.cache";
        XDG_CONFIG_HOME = "\${HOME}/.config";
        XDG_BIN_HOME = "\${HOME}/.local/bin";
        XDG_DATA_HOME = "\${HOME}/.local/share";
      };
    }
# *** KDE Plasma
    {
      environment.plasma6.excludePackages = with pkgs.kdePackages; [
        kwallet
        kwalletmanager
        kwallet-pam
      ];
      environment.variables = {
        # Old fix for black cursor on amdgpu, seems to work fine now
        #KWIN_DRM_NO_AMS = "1";

        #Fix for amdgpu crashes
        KWIN_DRM_USE_MODIFIERS = "0";
        KWIN_DRM_NO_DIRECT_SCANOUT = "1";
        QT_PLUGIN_PATH = [
          "${pkgs.qt6.qtimageformats}/${pkgs.qt6.qtbase.qtPluginPrefix}"
          "${pkgs.kdePackages.ffmpegthumbs}/${pkgs.qt6.qtbase.qtPluginPrefix}"
        ];
        XCURSOR_THEME = "breeze_cursors";
      };
      environment.systemPackages = with pkgs; [
        kdePackages.ark
        kdePackages.kcalc
        kdePackages.kate
      ];
      programs.kdeconnect.enable = true;

      programs.firefox.nativeMessagingHosts.packages = [pkgs.kdePackages.plasma-browser-integration ];
      programs.chromium.enablePlasmaBrowserIntegration = true;
    }
# *** SDDM
    (
      let
        plasma-materus = pkgs.writeScript "plasma-materus" ''
          export KWIN_DRM_DEVICES="/dev/dri/by-path/pci-0000\:53\:00.0-card"
          ${pkgs.kdePackages.plasma-workspace}/libexec/plasma-dbus-run-session-if-needed ${pkgs.kdePackages.plasma-workspace}/bin/startplasma-wayland
        '';

        westonSddm =
          let
            xcfg = config.services.xserver;
          in
          pkgs.writeText "weston.ini" ''
            [core]
            xwayland=false
            shell=fullscreen-shell.so

            [keyboard]
            keymap_model = ${builtins.toString xcfg.xkb.model};
            keymap_layout = ${builtins.toString xcfg.xkb.layout};
            keymap_variant = ${builtins.toString xcfg.xkb.variant};
            keymap_options = ${builtins.toString xcfg.xkb.options};

            [libinput]
            enable-tap = ${builtins.toString xcfg.libinput.mouse.tapping};
            left-handed = ${builtins.toString xcfg.libinput.mouse.leftHanded};

            [output]
            name=DP-3
            mode=1920x1080@240

            [output]
            name=DP-4
            mode=off

            [output]
            name=HDMI-A-3
            mode=off

          '';
      in
      {
        services.displayManager.defaultSession = "plasma-materus";

        services.displayManager.sddm.enable = true;
        services.displayManager.sddm.wayland.enable = true;
        services.displayManager.sddm.wayland.compositor = lib.mkForce "weston";
        services.displayManager.sddm.wayland.compositorCommand = lib.concatStringsSep " " [
          "${lib.getExe pkgs.weston}"
          "--shell=kiosk"
          "-c ${westonSddm}"
        ];
        services.displayManager.sessionPackages = [
          (
            (pkgs.writeTextDir "share/wayland-sessions/plasma-materus.desktop" ''
              [Desktop Entry]
              Name=Plasma (Wayland Materus)
              Comment=Plasma Desktop with KWIN_DRM_DEVICES env
              Exec=${plasma-materus}
              DesktopNames=KDE
              Type=Application
            '').overrideAttrs
            (_: {
              passthru.providedSessions = [ "plasma-materus" ];
            })
          )
        ];

        services.displayManager.sddm.settings = {
          General = {
            InputMethod = "";
          };
          Theme = {
            CursorTheme = "breeze_cursors";
            CursorSize = "24";
          };
        };
      }
    )
# ** Programs & Services
# *** Java
    {
      programs = {
        java.enable = true;
        java.package = pkgs.jdk;
        java.binfmt = true;
      };

      environment.variables = {
        JAVA_8_HOME = "${pkgs.jdk8}/lib/openjdk/";
        JAVA_17_HOME = "${pkgs.jdk17}/lib/openjdk/";
        JAVA_21_HOME = "${pkgs.jdk21}/lib/openjdk/";
      };
    }
# *** Samba
    {
      services.samba-wsdd.enable = true;
      services.samba-wsdd.openFirewall = true;
      services.samba = {
        enable = true;
        package = pkgs.sambaFull;
        openFirewall = true;
        settings = {
          global = {
            "workgroup" = "WORKGROUP";
            "server string" = "smbmaterus";
            "netbios name " = "smbmaterus";
            "security" = "user";
            "hosts allow" = "192.168.122. 127.0.0.1 localhost";
            "hosts deny" = "0.0.0.0/0";
            "guest account" = "nobody";
            "map to guest" = "bad user";
            "allow insecure wide links" = "yes";
          };
          windows = {
            "path" = "/materus/data/VM/windows_shared";
            "browseable" = "yes";
            "read only" = "no";
            "guest ok" = "no";
            "create mask" = "0644";
            "directory mask" = "0755";
            "force user" = "materus";
            "force group" = "users";
            "follow symlinks" = "yes";
            "wide links" = "yes";
          };

        };
      };

    }
# *** XServer
    {
      services.xserver.xkb.layout = "pl";

      services.xserver.enable = true;
      #services.xserver.videoDrivers = [ "amdgpu" "intel" ];

      services.xserver.displayManager.startx.enable = false;

      services.xserver.exportConfiguration = true;
      services.xserver.extraConfig = pkgs.lib.mkDefault ''
        Section "OutputClass"
           Identifier "amd-options"
           Option "TearFree" "True"
           Option "SWCursor" "True"
           Option "VariableRefresh" "true"
           Option "AsyncFlipSecondaries" "true"
           Option "DRI3" "1"
           MatchDriver "amdgpu"
        EndSection
      '';
    }
# *** Input
    {
      environment.sessionVariables = {
        QT_XKB_CONFIG_ROOT = "\${XKB_CONFIG_ROOT}";
        GTK_IM_MODULE = "fcitx";
        QT_IM_MODULE = "fcitx";
        XMODIFIERS = "@im=fcitx";
        SDL_IM_MODULE = "fcitx";

        MOZ_USE_XINPUT2 = "1";
        PATH = [ "\${XDG_BIN_HOME}" ];
      };

      i18n.inputMethod.enable = true;
      i18n.inputMethod.type = "fcitx5";
      i18n.inputMethod.fcitx5.addons = [
        pkgs.kdePackages.fcitx5-configtool
        pkgs.fcitx5-lua
        pkgs.fcitx5-mozc
        pkgs.fcitx5-gtk
        pkgs.kdePackages.fcitx5-qt
      ];

      services.libinput.enable = true;
      services.libinput.mouse = {
        accelProfile = "flat";
      };
    }
# *** Shell
    {
      environment.shellInit = ''
        if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:root &> /dev/null; fi;
        if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:$USER &> /dev/null; fi;
      '';
      environment.enableAllTerminfo = true;
      environment.pathsToLink = [
        "/share/zsh"
        "/share/bash-completion"
        "/share/fish"
      ];
      environment.shells = with pkgs; [
        zsh
        bashInteractive
        fish
      ];
      mkk.os.zsh.enable = true;
    }
# *** Other Services
    {
      systemd.tmpfiles.rules = [ "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}" ];
      services.flatpak.enable = true;
      services.gvfs.enable = true;

      services.dbus.enable = true;
      services.dbus.packages = [ pkgs.gcr ];

      services.printing.enable = true;
      

      services.teamviewer.enable = true;

      services.pcscd.enable = true;

      services.davfs2.enable = true;

      # GPG
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = false;
        enableBrowserSocket = true;

      };

      # SSH
      programs.ssh.startAgent = true;
      services.openssh.enable = true;
      services.openssh.settings.PermitRootLogin = "no";
      services.openssh.settings.PasswordAuthentication = false;
      services.openssh.openFirewall = true;

      # Sunshine
      services.sunshine = {
        enable = true;
        capSysAdmin = true;
        openFirewall = true;
        autoStart = false;
      };
    }
# *** Other Apps
    {
      programs = {
        fish.enable = true;
        command-not-found.enable = false;
        dconf.enable = true;
      };

      programs.firefox.enable = true;
      

      programs.gamemode.enable = true;
      programs.corectrl.enable = true;

      programs.nix-ld.enable = true;
      programs.nix-ld.libraries = with pkgs; [

      ];
      programs.chromium.enable = true;

      environment.systemPackages = with pkgs; [

        (vivaldi.overrideAttrs (oldAttrs: {
          dontWrapQtApps = false;
          dontPatchELF = true;
          nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ pkgs.kdePackages.wrapQtAppsHook ];
        }))

        #(pkgs.lutris.override { extraLibraries = pkgs: with pkgs;  [ pkgs.samba pkgs.jansson pkgs.tdb pkgs.libunwind pkgs.libusb1 pkgs.gnutls pkgs.gtk3 pkgs.pango ]; })
        konfig.nixerusPkgs.amdgpu-pro-libs.prefixes
        (pkgs.bottles.override {
          extraPkgs = pkgs: [
            pkgs.libsForQt5.breeze-qt5
            pkgs.kdePackages.breeze-gtk
            pkgs.nss_latest
          ];
          extraLibraries = pkgs: [
            pkgs.samba
            pkgs.jansson
            pkgs.tdb
            pkgs.libunwind
            pkgs.libusb1
            pkgs.gnutls
            pkgs.gtk3
            pkgs.pango
          ];
        })
        glibc
        glib
        gtk3
        gtk4
        gsettings-desktop-schemas
        kdePackages.dolphin
        vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
        vlc
        vkbasalt-cli

        patchelf
        killall
        util-linux
        xorg.xhost
        nix-top

        gitFull
        curl
        wget

        config.programs.java.package

        nss_latest

        pciutils

        (aspellWithDicts (
          ds: with ds; [
            en
            en-computers
            en-science
            pl
          ]
        ))
        steamtinkerlaunch
        distrobox
        # WebP support
        libwebp
        webp-pixbuf-loader

        # Compression
        p7zip
        unrar
        bzip2
        rar
        unzip
        zstd
        xz
        zip
        gzip

        tree
        mc
        lf
        htop
        nmon
        iftop
        iptraf-ng
        mprocs
        tldr
        bat
        ##config.materus.profile.packages.home-manager
        gcr
        # pgcli
        # litecli
        #zenmonitor

        nix-du

        wineWowPackages.stagingFull
        winetricks
        protontricks
        gnupg
        pinentry
        pinentry-gnome3
        pinentry-curses
        ncurses
        monkeysphere
        gparted

        virt-viewer

        inkscape
        gimp

        git-crypt

        bubblewrap
        bindfs

        binutils

        qbittorrent
        mkvtoolnix
        nicotine-plus
        picard
        opusTools
        aegisub
      ];
    }
# ** Network
# *** Firewall & Others
    {

      services = {
        syncthing = {
          enable = true;
          user = "materus";
          dataDir = "/home/materus";
        };
      };

      networking.hostName = "materusPC";
      networking.useDHCP = lib.mkDefault true;
      networking.wireless.iwd.enable = true;

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

        allowedTCPPorts = [
          24800
          5900
          5357
          4656
          8080
          9943
          9944
          # Syncthing
          22000
          config.services.syncthing.relay.statusPort
          config.services.syncthing.relay.port
        ];
        allowedUDPPorts = [
          (lib.strings.toInt konfig.vars.wireguard.ports.materusPC)
          24800
          5900
          3702
          4656
          6000
          9943
          9944
          # Syncthing
          22000
          21027
          # Zomboid
          17000
          17001
        ];
      };

    }
# *** NetworkManager
    {
      sops.templates."networkmanager.env".content = ''
        WIREGUARD_PRIVATEKEY="${config.sops.placeholder.wireguard}"
      '';
      networking.networkmanager.ensureProfiles.environmentFiles = [
        config.sops.templates."networkmanager.env".path
      ];
      networking.networkmanager.enable = true;
      #networking.networkmanager.wifi.backend = "iwd";

      networking.networkmanager.settings = {
        connectivity = {
          uri = "http://nmcheck.gnome.org/check_network_status.txt";
        };
      };

      networking.networkmanager.ensureProfiles.profiles = {
        wg0 = {
          connection = {
            id = "wg0";
            type = "wireguard";
            interface-name = "wg0";
          };
          wireguard = {
            private-key = "$WIREGUARD_PRIVATEKEY";
          };
          "wireguard-peer.${konfig.vars.wireguard.pubKeys.valkyrie}" = {
            endpoint = "${konfig.vars.ip.valkyrie.ipv4}:${konfig.vars.wireguard.ports.valkyrie}";
            allowed-ips = "${konfig.vars.wireguard.masks.general};";
            persistent-keepalive = "20";
          };
          ipv4 = {
            address1 = "${konfig.vars.wireguard.ip.materusPC}/23";
            dns = "${konfig.vars.wireguard.ip.valkyrie};";
            method = "manual";
            never-default = "true";
          };
          ipv6 = {
            addr-gen-mode = "stable-privacy";
            method = "disabled";
          };
          proxy = { };
        };
      };
    }
# ** Hardware
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
      services.udev = {
        packages = with pkgs; [
          game-devices-udev-rules
        ];
      };
      hardware.uinput.enable = true;
      hardware.steam-hardware.enable = true;

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
# * materusPC END
  ];
}
