# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  lib,
  pkgs,
  materusArgs,
  ...
}:
let
  patchedBwrap = pkgs.bubblewrap.overrideAttrs (o: {
    patches = (o.patches or [ ]) ++ [
      materusArgs.files.patches.bwrap
    ];
  });
in
{
  # Use the systemd-boot EFI boot loader.

  boot.plymouth.enable = true;

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_zen;

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "pl_PL.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "pl";
    useXkbConfig = false; # use xkb.options in tty.
  };
  hardware.graphics = { 
    enable = true;
    extraPackages = with pkgs; [
    ];
  };
  programs.java.enable = true;
  programs.steam = {
    enable = true;
    remotePlay.openFirewall = true;
    dedicatedServer.openFirewall = true;
    extraPackages = with pkgs; [
      vlc
      libva
      libva-utils
      libva-vdpau-driver
      mesa
      mesa-demos
      libvdpau-va-gl
      nss
      materusArgs.inputs.nixerus.packages.x86_64-linux.polymc
    ];
    extraCompatPackages = [
      pkgs.proton-ge-bin
    ];
    package = pkgs.steam.override {
      extraEnv = {
        PRESSURE_VESSEL_FILESYSTEMS_RW="\${XDG_RUNTIME_DIR}/wivrn/comp_ipc";
        #PRESSURE_VESSEL_IMPORT_OPENXR_1_RUNTIMES="1";
      };
      extraLibraries =
        p: with p; [
        ];
    };
  };

  programs.git = {
    enable = true;
    lfs.enable = true;
  };
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
    enableBrowserSocket = true;
  };

  programs.gamescope.enable = true;
  programs.gamescope.capSysNice = true;
  hardware.uinput.enable = true;
  hardware.steam-hardware.enable = true;
  programs.gamemode.enable = true;

  programs.corectrl.enable = true;

  programs.fish.enable = true;
  programs.command-not-found.enable = false;
  programs.dconf.enable = true;

  environment.systemPackages = with pkgs; [
    kdePackages.ark
    kdePackages.kcalc
    vim
    nano
    (vscodium.fhsWithPackages (
      ps: with ps; [
        nixfmt-rfc-style
        nixd
      ]
    ))

    obsidian
    git-crypt

    telegram-desktop
    discord
    thunderbird-latest

    floorp-bin
    brave

    keepassxc

    spotify
    remmina

    mesa-demos
    vulkan-tools

    pciutils

    schroot

    vlc

    fastfetch
  ];

  environment.variables = {
  };
  environment.sessionVariables = {
    STEAM_EXTRA_COMPAT_TOOLS_PATHS = "\${STEAM_EXTRA_COMPAT_TOOLS_PATHS}\${STEAM_EXTRA_COMPAT_TOOLS_PATHS:+:}\${HOME}/.steam/root/compatibilitytools.d";
    XDG_CACHE_HOME = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME = "\${HOME}/.local/bin";
    XDG_DATA_HOME = "\${HOME}/.local/share";
    QT_XKB_CONFIG_ROOT = "\${XKB_CONFIG_ROOT}";
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    XMODIFIERS = "@im=fcitx";
    SDL_IM_MODULE = "fcitx";

    MOZ_USE_XINPUT2 = "1";
    PATH = [ "\${XDG_BIN_HOME}" ];
  };
  environment.shellInit = ''
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:root &> /dev/null; fi;
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:$USER &> /dev/null; fi;
  '';
  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = [
    pkgs.kdePackages.fcitx5-configtool
    pkgs.fcitx5-lua
    pkgs.fcitx5-mozc
    pkgs.fcitx5-gtk
    pkgs.kdePackages.fcitx5-qt
  ];

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
    nushell
  ];

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
    #shell = pkgs.zsh;
    description = "Mateusz Słodkowicz";
    #openssh.authorizedKeys.keyFiles = [ ("${materusArg.cfg.path}" + "/extraFiles/keys/ssh/materus.pub") ];
    #hashedPasswordFile = config.sops.secrets."users/materus".path;
  };

  programs.firefox.enable = true;

  fonts.packages = [
    pkgs.dejavu_fonts
    pkgs.freefont_ttf
    pkgs.gyre-fonts
    pkgs.liberation_ttf
    pkgs.unifont

    pkgs.noto-fonts
    pkgs.noto-fonts-color-emoji
    pkgs.noto-fonts-cjk-sans
    pkgs.noto-fonts-cjk-serif
    pkgs.wqy_zenhei
    pkgs.corefonts
    pkgs.hack-font
    pkgs.nerd-fonts.hack

    pkgs.ubuntu-classic
    pkgs.monocraft
    pkgs.nerd-fonts.droid-sans-mono
    pkgs.nerd-fonts.meslo-lg
    pkgs.nerd-fonts.profont
    pkgs.nerd-fonts.fira-code
  ];
  fonts.enableDefaultPackages = lib.mkDefault true;

  fonts.fontconfig.enable = lib.mkDefault true;
  fonts.fontconfig.cache32Bit = lib.mkDefault true;

  fonts.fontconfig.defaultFonts.sansSerif = [
    "Noto Sans"
    "DejaVu Sans"
    "WenQuanYi Zen Hei"
    "Noto Color Emoji"
  ];
  fonts.fontconfig.defaultFonts.serif = [
    "Noto Serif"
    "DejaVu Serif"
    "WenQuanYi Zen Hei"
    "Noto Color Emoji"
  ];
  fonts.fontconfig.defaultFonts.emoji = [
    "Noto Color Emoji"
    "OpenMoji Color"
  ];
  fonts.fontconfig.defaultFonts.monospace = [
    "Hack Nerd Font"
    "Noto Sans Mono"
    "WenQuanYi Zen Hei Mono"
  ];

  fonts.fontDir.enable = lib.mkDefault true;

  nixpkgs.config.allowUnfree = true;
  nix.settings = {
    experimental-features = lib.mkMerge [
      [
        "nix-command"
        "flakes"
        "no-url-literals"
      ]
    ];
    auto-optimise-store = true;
    trusted-users = [
      "root"
      "@wheel"
    ];

    substituters = [
      "https://nix-community.cachix.org"
      "https://cache.nixos.org/"
      "https://nixerus.cachix.org/"
    ];
    trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "nixerus.cachix.org-1:2x7sIG7y1vAoxc8BNRJwsfapZsiX4hIl4aTi9V5ZDdE="
    ];
  };

  users.defaultUserShell = pkgs.zsh;
  programs.zsh = {
    enable = true;
    enableGlobalCompInit = false;
    interactiveShellInit = ''
      if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
        source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
      fi
    '';
    promptInit = '''';
  };

  systemd.sleep.extraConfig = ''
    AllowSuspend=yes
    AllowHibernation=no
    AllowHybridSleep=no
    AllowSuspendThenHibernate=no
  '';
  
  system.stateVersion = "25.11";
}
