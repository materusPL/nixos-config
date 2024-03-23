{ config, pkgs, materusArg, ... }:

{
  virtualisation.lxc.enable = true;
  virtualisation.lxc.lxcfs.enable = true;
  virtualisation.lxd.enable = true;
  #virtualisation.lxd.recommendedSysctlSettings = true;

  #programs.corectrl.enable = true;
  #programs.corectrl.gpuOverclock.enable = true;
  #programs.corectrl.gpuOverclock.ppfeaturemask = "0xffffffff";
  programs.gamemode.enable = true;




  services.teamviewer.enable = true;

  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
  ];

  services.flatpak.enable = true;
  services.gvfs.enable = true;



  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "pl_PL.UTF-8";
  console = {
    font = "lat2-16";
    #     keyMap = "pl";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
  services.dbus.enable = true;
  services.dbus.packages = [ pkgs.gcr ];


  #services.xserver.displayManager.autoLogin.user = "materus";
  services.xserver.displayManager.startx.enable = false;
  /*
    services.xserver.displayManager.lightdm.enable = true;
    services.xserver.displayManager.lightdm.greeters.enso.enable = true;
    services.xserver.displayManager.lightdm.greeters.enso.blur = true;
  */

  xdg.portal.enable = true;
  xdg.portal.wlr.enable = true;
  xdg.portal.xdgOpenUsePortal = true;

  services.xserver.exportConfiguration = false;
  services.xserver.extraConfig = pkgs.lib.mkDefault ''
    Section "OutputClass"
      Identifier "amd-options"
      Option "TearFree" "True"
      Option "SWCursor" "True"
      Option "VariableRefresh" "true"
      Option "AsyncFlipSecondaries" "true"
      MatchDriver "amdgpu"
    EndSection

  '';




  services.printing.enable = true;




  services.xserver.libinput.enable = true;

  virtualisation.waydroid.enable = false;
  virtualisation.podman = {
    enable = true;
    #enableNvidia = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };





  users.users.materus = {
    isNormalUser = true;
    extraGroups = [ "audio" "video" "render" "pipewire" "wheel" "networkmanager" "input" "kvm" "libvirt-qemu" "libvirt" "libvirtd" "podman" "lxd" ]; # Enable ‘sudo’ for the user.
    shell = pkgs.bashInteractive;
    description = "Mateusz Słodkowicz";
    #   packages = with pkgs; [
    #     firefox
    #     thunderbird
    #   ];
  };

  environment.sessionVariables = {
    XDG_CACHE_HOME = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME = "\${HOME}/.local/bin";
    XDG_DATA_HOME = "\${HOME}/.local/share";
    QT_XKB_CONFIG_ROOT = "\${XKB_CONFIG_ROOT}";
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    XMODIFIERS = "@im=fcitx";
    SDL_IM_MODULE = "fcitx";
    #SDL_AUDIODRIVER = "pipewire";

    #SSH_ASKPASS_REQUIRE = "prefer";

    MOZ_USE_XINPUT2 = "1";
    PATH = [
      "\${XDG_BIN_HOME}"
    ];
  };
  environment.shellInit = ''
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:root &> /dev/null; fi;
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:$USER &> /dev/null; fi;
  '';

  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = [ pkgs.kdePackages.fcitx5-configtool pkgs.fcitx5-lua pkgs.fcitx5-mozc pkgs.fcitx5-gtk pkgs.kdePackages.fcitx5-qt ];


  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  services.pcscd.enable = true;
  services.samba-wsdd.enable = true;

  services.samba = {
    enable = true;
    package = pkgs.sambaFull;
  };


  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
    enableBrowserSocket = true;

  };
  programs.ssh.startAgent = true;
  services.openssh.enable = true;

  environment.enableAllTerminfo = true;
  environment.pathsToLink = [ "/share/zsh" "/share/bash-completion" "/share/fish" ];
  environment.shells = with pkgs; [ zsh bashInteractive fish ];
  programs = {
    fish.enable = true;
    zsh = {
      enable = true;
      interactiveShellInit = ''
        if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
          source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
        fi
      '';
      promptInit = ''

      '';
    };
    java.enable = true;
    java.package = pkgs.graalvm-ce;
    java.binfmt = true;
    command-not-found.enable = false;
    dconf.enable = true;
  };





  /*containers.test = {
    config = { config, pkgs, ... }: { environment.systemPackages = with pkgs; [ wayfire ]; };
    autoStart = false;
    };*/

  environment.systemPackages = with pkgs; [
    gamescope
    #(pkgs.lutris.override { extraLibraries = pkgs: with pkgs;  [ pkgs.samba pkgs.jansson pkgs.tdb pkgs.libunwind pkgs.libusb1 pkgs.gnutls pkgs.gtk3 pkgs.pango ]; })
    materusArg.pkgs.amdgpu-pro-libs.prefixes
    (pkgs.bottles.override { extraPkgs = pkgs: [ pkgs.libsForQt5.breeze-qt5 pkgs.kdePackages.breeze-gtk pkgs.nss_latest ]; extraLibraries = pkgs: [ pkgs.samba pkgs.jansson pkgs.tdb pkgs.libunwind pkgs.libusb1 pkgs.gnutls pkgs.gtk3 pkgs.pango ]; })
    glibc
    glib
    gtk3
    gtk4
    gsettings-desktop-schemas
    kdePackages.dolphin
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.

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

    aspell
    aspellDicts.pl
    aspellDicts.en
    aspellDicts.en-computers
    steamtinkerlaunch
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
    ##config.materus.profile.packages.home-manager
    gcr
    # pgcli
    # litecli
    materusArg.pkgs.alvr
    #zenmonitor

    nix-du

    
    kate
    krusader

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
    config.materus.profile.packages.firefox
  ];







  environment.etc = {


    /*
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
        


      text = ''
      #!/usr/bin/env bash
      reboot
      '';*-/
      mode = "0755";
      };
      "libvirt/vgabios/patched.rom".source = ./vbios.rom;
      }; 
      };
    */
  };
}
