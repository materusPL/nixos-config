{
  config,
  pkgs,
  materusArg,
  ...
}:
{

  #services.jackett.enable = true;
  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [

  ];
  programs.chromium.enable = true;
  programs.chromium.enablePlasmaBrowserIntegration = true;

  services.guix.enable = true;

  
  environment.systemPackages = with pkgs; [

    vivaldi

    #(pkgs.lutris.override { extraLibraries = pkgs: with pkgs;  [ pkgs.samba pkgs.jansson pkgs.tdb pkgs.libunwind pkgs.libusb1 pkgs.gnutls pkgs.gtk3 pkgs.pango ]; })
    materusArg.pkgs.amdgpu-pro-libs.prefixes
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
    materusArg.pkgs.alvr
    #zenmonitor

    nix-du

    kdePackages.kate
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

    materusArg.unstable.qbittorrent
    mkvtoolnix
    nicotine-plus
    picard
    opusTools
    aegisub
    audacity
  ];
}
