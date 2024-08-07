{ config, pkgs, lib, materusArg, inputs, ... }:
let
  steamPkg = pkgs.steam.override {
    extraPkgs = pkgs: [
      pkgs.libdecor
      pkgs.obs-studio-plugins.obs-vkcapture
      pkgs.steamcmd
      pkgs.nss_latest
      pkgs.libstrangle
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.libGL
      pkgs.libglvnd
      pkgs.steamPackages.steam
      pkgs.libxcrypt
      pkgs.gnutls
      pkgs.xorg.libXcursor
      pkgs.xorg.libXi
      pkgs.xorg.libXinerama
      pkgs.xorg.libXScrnSaver
      pkgs.xorg.xinput
      pkgs.xorg.xcbutilwm
      pkgs.xorg.xcbutilimage
      pkgs.xorg.xcbutilkeysyms
      pkgs.xorg.xcbutilerrors
      pkgs.xorg.xcbutilrenderutil
      pkgs.xorg.xcbutil
      pkgs.xorg.xwininfo
      pkgs.yad
      pkgs.xdotool
      pkgs.libinput
      pkgs.openvdb
      pkgs.openssl
      pkgs.tbb
      pkgs.gtk4
      pkgs.gtk3
      pkgs.glib
      pkgs.gsettings-desktop-schemas
      pkgs.fuse
      pkgs.samba4Full
      pkgs.tdb
      pkgs.jbig2enc
      pkgs.jbig2dec
      pkgs.vivaldi
      pkgs.x264.lib
      pkgs.steamtinkerlaunch
      pkgs.pipewire
      pkgs.gitFull
      pkgs.git-lfs

    ] ++ config.materus.profile.packages.list.fonts ++ config.materus.profile.steam.extraPkgs;

    extraLibraries = pkgs: [
      pkgs.libkrb5
      pkgs.keyutils
      pkgs.ncurses6
      pkgs.xorg.xinput
      pkgs.libinput
      pkgs.fontconfig
      pkgs.libxcrypt
      pkgs.gnutls
      pkgs.samba
      pkgs.tdb
      pkgs.jemalloc
      pkgs.gperftools
    ] ++
    (with config.hardware.opengl; if pkgs.hostPlatform.is64bit
    then [ package ] ++ extraPackages
    else [ package32 ] ++ extraPackages32);

    extraEnv = config.materus.profile.steam.extraEnv;

  };

  cfg = config.materus.profile.steam;
in
{
  options.materus.profile.steam.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable materus steam settings for OS";
  options.materus.profile.steam.package = lib.mkOption {
    type = lib.types.package;
    default = steamPkg;
    description = "Package used by steam";
  };
  options.materus.profile.steam.extraPkgs = lib.mkOption {
    default = [ ];
    description = "Extra packages for steam";
  };
  options.materus.profile.steam.extraEnv = lib.mkOption {
    default = { };
    description = "Extra Env for steam";
  };


  config = lib.mkIf cfg.enable {
    programs.gamescope.enable = lib.mkDefault true;
    programs.gamescope.capSysNice = lib.mkDefault false;
    hardware.steam-hardware.enable = lib.mkDefault true;
    materus.profile.steam.extraEnv = {
      XDG_DATA_DIRS = "/usr/share:\${XDG_DATA_DIRS}";
      OBS_VKCAPTURE = "1";
    };
    programs.steam = {
      enable = lib.mkDefault true;
      dedicatedServer.openFirewall = lib.mkDefault true;
      remotePlay.openFirewall = lib.mkDefault true;
      gamescopeSession.enable = lib.mkDefault true;
    };
    environment.sessionVariables = rec {
      STEAM_EXTRA_COMPAT_TOOLS_PATHS = lib.mkDefault "\${HOME}/.steam/root/compatibilitytools.d";
    };
    environment.systemPackages = [
      steamPkg
      steamPkg.run

    ];
  };
}
