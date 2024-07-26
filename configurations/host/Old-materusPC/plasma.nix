{ pkgs, ... }:
{

  services.displayManager.sddm.enable = true;
  services.displayManager.sddm.wayland.enable = true;
  services.displayManager.sddm.settings = {
    General = {
      InputMethod = "";
    };
    Theme = {
      CursorTheme = "breeze_cursors";
      CursorSize = "24";
    };
  };
  environment.variables = {
    QT_PLUGIN_PATH = [ "${pkgs.qt6.qtimageformats}/${pkgs.qt6.qtbase.qtPluginPrefix}" ];
    XCURSOR_THEME = "breeze_cursors";
  };
  xdg.portal.enable = true;
  xdg.portal.wlr.enable = true;
  xdg.portal.xdgOpenUsePortal = true;
  xdg.portal.extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
  services.desktopManager.plasma6.enable = true;
  services.desktopManager.plasma6.enableQt5Integration = true;
  environment.plasma6.excludePackages = with pkgs.kdePackages; [ kwallet kwalletmanager kwallet-pam ];
  environment.systemPackages = with pkgs.kdePackages; [
    ark
    kate
  ];
  materus.profile.steam.extraPkgs = [ pkgs.kdePackages.breeze pkgs.kdePackages.breeze-gtk pkgs.kdePackages.dolphin ];
}
