# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  lib,
  pkgs,
  materusArgs,
  mkk,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.tmp.useTmpfs = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    device = "nodev";
    useOSProber = true;
    memtest86.enable = true;
  };
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    modesetting.enable = true;
    open = true;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
    ];
  };
  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_zen;
  networking.hostName = "oldie"; # Define your hostname.
  environment.systemPackages = with pkgs; [
    neovim
    nano
    fastfetch
    brave
  ];
  environment.enableAllTerminfo = true;
  environment.pathsToLink = [
    "/share/zsh"
    "/share/bash-completion"
    "/share/fish"
  ];
  # Configure network connections interactively with nmcli or nmtui.
  networking.networkmanager.enable = true;
  programs.firefox.enable = true;
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
        PRESSURE_VESSEL_FILESYSTEMS_RW = "\${XDG_RUNTIME_DIR}/wivrn/comp_ipc";
        #PRESSURE_VESSEL_IMPORT_OPENXR_1_RUNTIMES="1";
      };
      extraLibraries =
        p: with p; [
        ];
    };
  };
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

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";
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

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  users.defaultUserShell = pkgs.zsh;
  programs.zsh = {
    enable = true;
    enableGlobalCompInit = false;
    interactiveShellInit = ''
      if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
        source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
      fi
    '';
    promptInit = "";
  };

  # Select internationalisation properties.
  i18n.defaultLocale = "pl_PL.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "pl";
    useXkbConfig = false; # use xkb.options in tty.
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

  # Enable the X11 windowing system.
  services.xserver.enable = false;
  services.displayManager = {
    autoLogin.enable = true;
    autoLogin.user = "materus";
    plasma-login-manager.enable = true;
  };
  services.desktopManager.plasma6.enable = true;
  services.desktopManager.plasma6.enableQt5Integration = true;

  programs.ssh.startAgent = true;

  services.syncthing = {
    enable = true;
    user = "materus";
    dataDir = "/home/materus";
  };

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    systemWide = true;
    jack.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.alice = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     tree
  #   ];
  # };

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  # environment.systemPackages = with pkgs; [
  #   vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
  #   wget
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.

  hardware.uinput.enable = true;
  hardware.steam-hardware.enable = true;


  sops.templates."networkmanager.env".content = ''
    WIREGUARD_PRIVATEKEY="${config.sops.placeholder.wg-key}"
  '';
  networking.networkmanager.ensureProfiles.environmentFiles = [
    config.sops.templates."networkmanager.env".path
  ];
  networking.networkmanager.ensureProfiles.profiles = {
    wg0 = {
      connection = {
        id = "PodKos";
        type = "wireguard";
        interface-name = "wg-podkos";
      };
      wireguard = {
        private-key = "$WIREGUARD_PRIVATEKEY";
      };
      "wireguard-peer.${mkk.wireguard.peers.valkyrie.pubKey}" = {
        endpoint = "${mkk.network.valkyrie.ip}:${mkk.wireguard.peers.valkyrie.port}";
        allowed-ips = "${mkk.wireguard.ip-masks.main};${mkk.wireguard.ip-masks.guest};${mkk.wireguard.ip-masks.asia};${mkk.wireguard.peers.valkyrie.ip}/32;";
        persistent-keepalive = "20";
      };
      ipv4 = {
        address1 = "${mkk.wireguard.peers.oldie.ip}/32";
        dns = "${mkk.wireguard.peers.valkyrie.ip};";
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


  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?

}
