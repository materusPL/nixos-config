# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, materusArg, ... }:
let
  nvidia-unpatched = pkgs.stdenv.mkDerivation (f: {
    pname = config.hardware.nvidia.package.name;
    version = config.hardware.nvidia.package.version + "-unpatched";

    src = config.hardware.nvidia.package.src;
    nativeBuildInputs = [
    ];
    sourceRoot = ".";
    buildInputs = [
    ];

    unpackPhase = ''
      VER=${config.hardware.nvidia.package.version}
      PKGNAME=NVIDIA-Linux-x86_64-''${VER}
      cp $src ''${PKGNAME}.run
      chmod +x ./''${PKGNAME}.run
      ./''${PKGNAME}.run --extract-only
      mv ./''${PKGNAME} ./nvidia
    '';

    installPhase = ''
      mkdir -p $out/bin
      mkdir -p $out/lib
      mkdir -p $out/lib32

      mv nvidia $out/nvidia

    '';

  });





  steam = pkgs.steam.override {
    extraPkgs = pkgs: [
      pkgs.nss_latest
      pkgs.libstrangle
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.libGL
      pkgs.libglvnd
      pkgs.gamescope
      pkgs.steamPackages.steam
      pkgs.wqy_zenhei

      pkgs.xorg.libXcursor
      pkgs.xorg.libXi
      pkgs.xorg.libXinerama
      pkgs.xorg.libXScrnSaver
    ];

    extraLibraries = pkgs: [
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.ncurses6
      pkgs.fontconfig
    ];

  };


  grml-config = pkgs.fetchFromGitHub {
    owner = "grml";
    repo = "grml-etc-core";
    rev = "a2cda85d3d56fd5f5a7b954a444fd151318c4680";
    sha256 = "0ap8lmqi45yjyjazdm1v64fz1rfqhkhfpdp2z17ag6hs5wi6i67y";
  };

in
{
  imports =
    [
      # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.supportedFilesystems = [ "ntfs" ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.auto-optimise-store = true;
  nix.settings.substituters = [
    "https://nix-community.cachix.org"
    "https://cache.nixos.org/"
  ];
  nix.settings.trusted-public-keys = [ "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=" ];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.firefox.enablePlasmaBrowserIntegration = true;

  services.xserver.displayManager.startx.enable = true;

  security.sudo.extraRules = [
    { users = [ "materus" ]; commands = [{ command = "${pkgs.podman}/bin/podman"; options = [ "NOPASSWD" ]; }]; }

  ];
  /*security.wrappers.gamescope = {
    owner = "root";
    group = "root";
    capabilities = "cap_sys_nice=eip";
    source = "${pkgs.gamescope.out}/bin/gamescope";
    };*/


  # Use the systemd-boot EFI boot loader.
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    device = "nodev";
    gfxmodeEfi = "1920x1080";
    gfxmodeBios = "1920x1080";
    useOSProber = true;
  };
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelParams = [ "ibt=off" "intel_iommu=on" "iommu=pt" "pcie_acs_override=downstream,multifunction" ];
  boot.kernelModules = [ "kvm-intel" "vfio-pci" ];
  boot.tmpOnTmpfs = true;

  services.flatpak.enable = true;
  services.gvfs.enable = true;

  networking.hostName = "Old-materusPC"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable = true; # Easiest to use and most distros use this by default.

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";


  services.fstrim = {
    enable = true;
    interval = "weekly"; 
};
  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "pl_PL.UTF-8";
  console = {
    font = "lat2-16";
    #     keyMap = "pl";
    useXkbConfig = true; # use xkbOptions in tty.
  };


  networking.extraHosts =
    ''

  '';

  hardware.bluetooth.enable = true;
  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
 /* hardware.opengl.extraPackages = with pkgs; [
    vaapiVdpau
    nvidia-vaapi-driver
    libvdpau-va-gl
  ];
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [
    vaapiVdpau
    nvidia-vaapi-driver
    libvdpau-va-gl
  ];*/
  # Optionally, you may need to select the appropriate driver version for your specific GPU.
  #hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.stable;

  /*
  #GNOME
  services.xserver.displayManager.gdm.wayland = false;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  security.pam.services.gdm.enableGnomeKeyring = true;


  services.gnome.rygel.enable = true;
  services.gnome.sushi.enable = true;
  services.gnome.tracker.enable = true;
  services.gnome.gnome-online-accounts.enable = true;
  services.gnome.gnome-browser-connector.enable = true;
  services.gnome.gnome-settings-daemon.enable = true;
  services.gnome.core-utilities.enable = true;
  services.gnome.core-shell.enable = true;
  services.gnome.core-os-services.enable = true;

  programs.gnome-terminal.enable = true;
  services.gnome.gnome-keyring.enable = true;
  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon gnome2.GConf ];
  */


  #security.pam.services.login.enableKwallet = true;



  #services.xserver.displayManager.lightdm.enable = true;
  #services.xserver.displayManager.lightdm.greeters.enso.enable = true;
  #services.xserver.displayManager.lightdm.greeters.enso.blur = true;

  #services.xserver.desktopManager.xfce.enable = true;
  #xdg.portal.enable = true;
  #xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];


  # Enable the Plasma 5 Desktop Environment.

  

  security.pam.services.sddm.enableKwallet = true;

  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  #services.xserver.desktopManager.plasma5.supportDDC = true;
  services.xserver.desktopManager.plasma5.phononBackend = "gstreamer";
  services.xserver.desktopManager.plasma5.useQtScaling = true;
  services.xserver.desktopManager.plasma5.runUsingSystemd = true;
  #services.xserver.desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [ kwallet ];






  # Configure keymap in X11
  services.xserver.layout = "pl";
  # services.xserver.xkbOptions = {
  #   "eurosign:e";
  #   "caps:escape" # map caps to escape.
  # };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;
  };
  hardware.pulseaudio.enable = false;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;


  virtualisation.podman = {
    enable = true;
    enableNvidia = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };


  virtualisation.libvirtd = {
    enable = true;
    onBoot = "ignore";
    onShutdown = "shutdown";
    qemu.ovmf.enable = true;
    qemu.ovmf.packages = [ pkgs.OVMFFull.fd ];
    qemu.runAsRoot = true;
    qemu.swtpm.enable = true;
  };

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
          ];
        };
      in
      [ env ];
  };

  system.activationScripts.libvirt-hooks.text =
    ''
      ln -Tfs /etc/libvirt/hooks /var/lib/libvirt/hooks
    '';



  environment.etc = {
    "libvirt/hooks/qemu" = {
      text =
        ''
          #!/usr/bin/env bash
                            #
                            # Author: Sebastiaan Meijer (sebastiaan@passthroughpo.st)
                            #
                            # Copy this file to /etc/libvirt/hooks, make sure it's called "qemu".
                            # After this file is installed, restart libvirt.
                            # From now on, you can easily add per-guest qemu hooks.
                            # Add your hooks in /etc/libvirt/hooks/qemu.d/vm_name/hook_name/state_name.
                            # For a list of available hooks, please refer to https://www.libvirt.org/hooks.html
                            #

                            GUEST_NAME="$1"
                            HOOK_NAME="$2"
                            STATE_NAME="$3"
                            MISC="''${@:4}"

                            BASEDIR="$(dirname $0)"

                            HOOKPATH="$BASEDIR/qemu.d/$GUEST_NAME/$HOOK_NAME/$STATE_NAME"

                            set -e # If a script exits with an error, we should as well.

                            # check if it's a non-empty executable file
                            if [ -f "$HOOKPATH" ] && [ -s "$HOOKPATH"] && [ -x "$HOOKPATH" ]; then
                                eval \"$HOOKPATH\" "$@"
                            elif [ -d "$HOOKPATH" ]; then
                                while read file; do
                                    # check for null string
                                    if [ ! -z "$file" ]; then
                                      eval \"$file\" "$@"
                                    fi
                                done <<< "$(find -L "$HOOKPATH" -maxdepth 1 -type f -executable -print;)"
                            fi
        '';
      mode = "0755";
    };

    "libvirt/hooks/kvm.conf" = {
      text =
        ''
          VIRSH_GPU_VIDEO=pci_0000_01_00_0
          VIRSH_GPU_AUDIO=pci_0000_01_00_1
          VIRSH_GPU_USB=pci_0000_01_00_2
          VIRSH_GPU_SERIAL_BUS=pci_0000_01_00_3
        '';
      mode = "0755";
    };

    "libvirt/hooks/qemu.d/win11/prepare/begin/start.sh" = {
      text =
        ''
          #!/usr/bin/env bash
                            # Debugging
                             exec 19>/home/materus/startlogfile
                             BASH_XTRACEFD=19
                             set -x

                             exec 3>&1 4>&2
                             trap 'exec 2>&4 1>&3' 0 1 2 3
                             exec 1>/home/materus/startlogfile.out 2>&1



                            # Stop display manager
                            killall -u materus
                            systemctl stop display-manager.service
                            killall gdm-x-session
                            #systemctl isolate multi-user.target
                            sleep 1


                            # Load variables we defined
                            source "/etc/libvirt/hooks/kvm.conf"

                            # Isolate host to core 0
                            systemctl set-property --runtime -- user.slice AllowedCPUs=0
                            systemctl set-property --runtime -- system.slice AllowedCPUs=0
                            systemctl set-property --runtime -- init.scope AllowedCPUs=0



                            # Unbind VTconsoles
                            for (( i = 0; i < 16; i++))
                            do
                              if test -x /sys/class/vtconsole/vtcon"''${i}"; then
                                  if [ "$(grep -c "frame buffer" /sys/class/vtconsole/vtcon"''${i}"/name)" = 1 ]; then
                                    echo 0 > /sys/class/vtconsole/vtcon"''${i}"/bind
                                      echo "$DATE Unbinding Console ''${i}"
                                  fi
                              fi
                            done

                            # Unbind EFI Framebuffer
                            echo "efi-framebuffer.0" > /sys/bus/platform/drivers/efi-framebuffer/unbind

                            # Avoid race condition
                             sleep 1

                            # Unload NVIDIA kernel modules
                            modprobe -r nvidia_uvm
                            modprobe -r nvidia_drm
                            modprobe -r nvidia_modeset
                            modprobe -r nvidia
                            modprobe -r i2c_nvidia_gpu
                            modprobe -r drm_kms_helper
                            modprobe -r drm

                            # Detach GPU devices from host
                            #virsh nodedev-detach $VIRSH_GPU_VIDEO
                            #virsh nodedev-detach $VIRSH_GPU_AUDIO
                            #virsh nodedev-detach $VIRSH_GPU_USB
                            #virsh nodedev-detach $VIRSH_GPU_SERIAL_BUS

                            # Load vfio module
                            modprobe vfio
                            modprobe vfio_pci
                            modprobe vfio_iommu_type1
        '';
      mode = "0755";
    };

    "libvirt/hooks/qemu.d/win11/release/end/stop.sh" = {
      text =
        ''
          #!/usr/bin/env bash 
          # Debugging
          exec 19>/home/materus/stoplogfile
          BASH_XTRACEFD=19
          set -x

          exec 3>&1 4>&2
          trap 'exec 2>&4 1>&3' 0 1 2 3
          exec 1>/home/materus/stoplogfile.out 2>&1

          # Load variables we defined
          source "/etc/libvirt/hooks/kvm.conf"

          # Unload vfio module
          modprobe -r vfio-pci
          modprobe -r vfio_iommu_type1
          modprobe -r vfio

        

          modprobe drm
          modprobe drm_kms_helper
          modprobe i2c_nvidia_gpu
          modprobe nvidia
          modprobe nvidia_modeset
          modprobe nvidia_drm
          modprobe nvidia_uvm

          # Attach GPU devices from host
          #virsh nodedev-reattach $VIRSH_GPU_VIDEO
          #virsh nodedev-reattach $VIRSH_GPU_AUDIO
          #virsh nodedev-reattach $VIRSH_GPU_USB
          #virsh nodedev-reattach $VIRSH_GPU_SERIAL_BUS

          #echo "0000:01:00.0" > /sys/bus/pci/drivers/nvidia/bind
          # Bind EFI Framebuffer
          echo "efi-framebuffer.0" > /sys/bus/platform/drivers/efi-framebuffer/bind

          # Bind VTconsoles
          echo 1 > /sys/class/vtconsole/vtcon0/bind
          #echo 1 > /sys/class/vtconsole/vtcon1/bind


          # Start display manager
          sleep 1
          systemctl start display-manager.service

          # Return host to all cores
          systemctl set-property --runtime -- user.slice AllowedCPUs=0-3
          systemctl set-property --runtime -- system.slice AllowedCPUs=0-3
          systemctl set-property --runtime -- init.scope AllowedCPUs=0-3
        '';
      /*text = ''
        #!/usr/bin/env bash
        reboot
        '';*/
      mode = "0755";
    };
    "libvirt/vgabios/patched.rom".source = ./vbios.rom;
  };








  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.materus = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "kvm" "input" "libvirt" "libvirtd" "podman" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.zsh;
    description = "Mateusz Słodkowicz";
    #   packages = with pkgs; [
    #     firefox
    #     thunderbird
    #   ];
  };
  environment.sessionVariables = rec {
    XDG_CACHE_HOME = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME = "\${HOME}/.local/bin";
    XDG_DATA_HOME = "\${HOME}/.local/share";

    #SSH_ASKPASS_REQUIRE = "prefer";

    STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\${HOME}/.steam/root/compatibilitytools.d";

    MOZ_USE_XINPUT2 = "1";
    PATH = [
      "\${XDG_BIN_HOME}"
    ];
  };
  environment.shellInit = ''
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:root &> /dev/null; fi;
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:$USER &> /dev/null; fi;
  '';
  # List packages installed in system profile. To search, run:
  # $ nix search wget




  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = [ pkgs.fcitx5-configtool pkgs.fcitx5-lua pkgs.fcitx5-mozc pkgs.libsForQt5.fcitx5-qt ];

  environment.systemPackages = with pkgs; [
    firefox
    steam
    steam.run
    gamescope
    (pkgs.lutris.override { extraLibraries = pkgs: with pkgs;  [ pkgs.libunwind pkgs.libusb1 pkgs.gnutls pkgs.gtk3 pkgs.pango ]; })

    glibc
    appimage-run
    patchelf
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    killall
    xorg.xkill
    xorg.xhost
    nix-top
    gitFull
    curl
    jdk
    nss_latest
    aspell
    aspellDicts.pl
    aspellDicts.en
    aspellDicts.en-computers
    distrobox
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


    pgcli
    litecli

    #zenmonitor

    nix-du
    git-crypt
    ark
    kate
    kwalletcli
    krusader

    wineWowPackages.stagingFull
    winetricks
    protontricks
    openal
    gnupg
    pinentry
    pinentry-gnome
    pinentry-curses
    ncurses
    monkeysphere
    gparted

    inkscape
    gimp
    (materusArg.pkgs.polymc.wrap { extraJDKs = [ pkgs.graalvm-ce ]; })



    virt-manager
    libguestfs

    bubblewrap
    bindfs

    pulseaudio

    binutils
    /*
    gnome3.adwaita-icon-theme
    gnome3.gnome-tweaks
    gnome3.gnome-color-manager
    gnome3.gnome-shell-extensions

    gnomeExtensions.appindicator
    gnomeExtensions.desktop-clock
    gnomeExtensions.gtk4-desktop-icons-ng-ding
    gnomeExtensions.compiz-windows-effect
    gnomeExtensions.burn-my-windows
    gnomeExtensions.user-themes
    gnomeExtensions.gsconnect
    gnomeExtensions.dash-to-panel
    gnomeExtensions.dash-to-dock
    */


  ];

  fonts.fontDir.enable = true;
  fonts.enableDefaultFonts = true;
  fonts.fonts = with pkgs; [
    dejavu_fonts
    hack-font
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
    noto-fonts-cjk-sans
    noto-fonts-cjk-serif
    ubuntu_font_family
    wqy_zenhei
    monocraft
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" "Meslo" "ProFont" ]; })
  ];
  fonts.fontconfig.enable = true;
  fonts.fontconfig.cache32Bit = true;
  fonts.fontconfig.defaultFonts.sansSerif = [ "Noto Sans" "DejaVu Sans" "WenQuanYi Zen Hei" "Noto Color Emoji" ];
  fonts.fontconfig.defaultFonts.serif = [ "Noto Serif" "DejaVu Serif" "WenQuanYi Zen Hei" "Noto Color Emoji"];
  fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" "OpenMoji Color" ];
  fonts.fontconfig.defaultFonts.monospace = [ "FiraCode Nerd Font Mono" "Noto Sans Mono" "WenQuanYi Zen Hei Mono" ];


  environment.enableAllTerminfo = true;
  environment.pathsToLink = [ "/share/zsh" ];
  environment.shells = with pkgs; [ zsh ];
  programs = {
    steam = {
      enable = true;
      dedicatedServer.openFirewall = true;
      remotePlay.openFirewall = true;
    };
    fish.enable = true;
    zsh = {
      enable = true;
      interactiveShellInit = ''
        if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
          source ${grml-config}/etc/zsh/zshrc
        fi
      '';
      promptInit = ''
      
      '';
    };
    java.enable = true;
    command-not-found.enable = false;
    dconf.enable = true;
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.pcscd.enable = true;

  /*systemd.user.services.gpg-agent.serviceConfig.ExecStart = [
    ""
    ''
    ${pkgs.gnupg}/bin/gpg-agent --supervised \
    --pinentry-program ${pkgs.kwalletcli}/bin/pinentry-kwallet
    ''
    ];*/


  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    enableBrowserSocket = true;
  };
  programs.ssh.startAgent = false;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.samba-wsdd.enable = true;
  services.samba = {
    enable = true;
    securityType = "user";
    extraConfig = ''
      workgroup = WORKGROUP
      server string = smbnix
      netbios name = smbnix
      security = user 
      #use sendfile = yes
      #max protocol = smb2
      # note: localhost is the ipv6 localhost ::1
      hosts allow = 192.168.100. 192.168.122. 127.0.0.1 localhost
      hosts deny = 0.0.0.0/0
      guest account = nobody
      map to guest = bad user
    '';
    shares = {
      share = {
        path = "/materus/data/share";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "materus";
        "force group" = "users";
      };
    };
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 24800 5900  5357 4656];
  networking.firewall.allowedUDPPorts = [ 24800 5900  3702 4656];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  system.copySystemConfiguration = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}

