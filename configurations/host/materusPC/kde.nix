{ config, pkgs, lib, ... }:
let
  westonSddm = pkgs.writeText "weston.ini"
    ''
      [core]
      xwayland=true
      shell=fullscreen-shell.so

      [keyboard]
      keymap_layout=pl

      [output]
      name=DP-3
      mode=1920x1080@240

      [output]
      name=DP-2
      mode=off

      [output]
      name=HDMI-A-3
      mode=off
    ''
  ;
in
{
  services.xserver.displayManager.defaultSession = "plasma";
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.settings = {
    General = {
      #DisplayServer = "wayland";
      InputMethod = "";
    };
    Theme = {
      CursorTheme = "breeze_cursors";
      CursorSize = "24";
    };
    Wayland = {
      #CompositorCommand = "${pkgs.weston}/bin/weston  -c ${westonSddm}";

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
  };
  environment.systemPackages = with pkgs; [
    kdePackages.ark
  ];

  materus.profile.steam.extraPkgs = [ pkgs.kdePackages.breeze pkgs.kdePackages.breeze-gtk pkgs.kdePackages.dolphin ];
}
