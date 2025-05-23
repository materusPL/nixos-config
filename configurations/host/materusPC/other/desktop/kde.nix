{ config, pkgs, lib, ... }:
{

  xdg.portal.enable = true;
  xdg.portal.wlr.enable = true;
  xdg.portal.xdgOpenUsePortal = true;
  xdg.portal.extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];

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
    QT_PLUGIN_PATH = [ 
      "${pkgs.qt6.qtimageformats}/${pkgs.qt6.qtbase.qtPluginPrefix}"
      "${pkgs.kdePackages.ffmpegthumbs}/${pkgs.qt6.qtbase.qtPluginPrefix}"
     ];
    XCURSOR_THEME = "breeze_cursors";
  };
  environment.systemPackages = with pkgs; [
    kdePackages.ark
    kdePackages.kcalc
  ];
  programs.kdeconnect.enable = true;
  materus.profile.steam.extraPkgs = [ pkgs.kdePackages.breeze pkgs.kdePackages.breeze-gtk pkgs.kdePackages.dolphin pkgs.vlc pkgs.vkbasalt-cli ];
}
