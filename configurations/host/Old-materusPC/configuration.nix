# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, materusArg, ... }:

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

  hardware.bluetooth.enable = true;
  # Enable the X11 windowing system.
  services.xserver.enable = true;


  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;

  materus.profile.steam.enable = true;









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



  virtualisation.podman = {
    enable = true;
    dockerCompat = true;
    dockerSocket.enable = true;
  };


  users.users.materus = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "kvm" "input" "libvirt" "libvirtd" "podman" ];
    shell = pkgs.zsh;
    description = "Mateusz Słodkowicz";

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
    brave

    glibc
    patchelf
    vim
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



    nix-du
    git-crypt


    wineWowPackages.stagingFull
    winetricks
    protontricks
    openal
    gnupg
    ncurses
    monkeysphere
    gparted

    inkscape
    gimp




    virt-manager
    libguestfs

    bubblewrap
    bindfs

    pulseaudio

    binutils


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
  fonts.fontconfig.defaultFonts.serif = [ "Noto Serif" "DejaVu Serif" "WenQuanYi Zen Hei" "Noto Color Emoji" ];
  fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" "OpenMoji Color" ];
  fonts.fontconfig.defaultFonts.monospace = [ "Hack Nerd Font" "Noto Sans Mono" "WenQuanYi Zen Hei Mono" ];


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
    enableSSHSupport = false;
    enableBrowserSocket = true;
  };
  programs.ssh.startAgent = true;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;


  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 24800 5900 5357 4656 ];
  networking.firewall.allowedUDPPorts = [ 24800 5900 3702 4656 ];
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
  system.stateVersion = "23.11"; # Did you read the comment?

}

