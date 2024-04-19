{ config, pkgs, lib, ... }:
let
  westonSddm = let xcfg = config.services.xserver; in  pkgs.writeText "weston.ini"
    ''
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
      name=DP-2
      mode=off

      [output]
      name=HDMI-A-3
      mode=off

    '';
in
{
  services.displayManager.defaultSession = "plasma";
  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.sddm.wayland.compositor = lib.mkForce "weston";
  services.displayManager.sddm.wayland.compositorCommand = lib.concatStringsSep " " [
    "${lib.getExe pkgs.weston}"
    "--shell=kiosk"
    "-c ${westonSddm}"
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
  services.desktopManager.plasma6.enable = true;
  services.desktopManager.plasma6.enableQt5Integration = true;
  programs.gnupg.agent.pinentryPackage = lib.mkForce pkgs.pinentry-gnome3;
  environment.plasma6.excludePackages = with pkgs.kdePackages; [ kwallet kwalletmanager kwallet-pam ];

  environment.variables = {
    # Old fix for black cursor on amdgpu, seems to work fine now
    #KWIN_DRM_NO_AMS = "1";

    #Fix for amdgpu crashes
    KWIN_DRM_USE_MODIFIERS = "0";
    KWIN_DRM_NO_DIRECT_SCANOUT = "1";
    QT_PLUGIN_PATH = [ "${pkgs.qt6.qtimageformats}/${pkgs.qt6.qtbase.qtPluginPrefix}" ];
    XCURSOR_THEME = "breeze_cursors";
  };
  environment.systemPackages = with pkgs; [
    kdePackages.ark
  ];

  materus.profile.steam.extraPkgs = [ pkgs.kdePackages.breeze pkgs.kdePackages.breeze-gtk pkgs.kdePackages.dolphin ];
}
