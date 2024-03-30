{ pkgs, ... }:
{

  services.xserver.displayManager.sddm.enable = true;
  services.xserver.displayManager.sddm.wayland.enable = true;
  services.xserver.displayManager.sddm.settings = {
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
  };
  services.desktopManager.plasma6.enable = true;
  services.desktopManager.plasma6.enableQt5Integration = true;
  environment.plasma6.excludePackages = with pkgs.kdePackages; [ kwallet kwalletmanager kwallet-pam ];
  environment.systemPackages = with pkgs.kdePackages; [
    ark
    kate
  ];
  materus.profile.steam.extraPkgs = [ pkgs.kdePackages.breeze pkgs.kdePackages.breeze-gtk pkgs.kdePackages.dolphin ];
}
