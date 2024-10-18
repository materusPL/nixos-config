{ config, pkgs, lib, ... }:
{
  services.displayManager.defaultSession = "gnome";
  
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.desktopManager.gnome.sessionPath = [ pkgs.gnome.gpaste ];

  services.gnome.gnome-online-accounts.enable = true;
  services.gnome.gnome-browser-connector.enable = true;
  services.gnome.core-utilities.enable = true;
  services.gnome.core-shell.enable = true;
  services.gnome.core-os-services.enable = true;

  programs.gnupg.agent.pinentryPackage = lib.mkForce pkgs.pinentry-gnome3;


  programs.gnome-terminal.enable = true;

  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  services.dbus.packages = with pkgs; [ gnome2.GConf ];

  environment.systemPackages = with pkgs; [

    gnome3.adwaita-icon-theme
    gnome3.gnome-tweaks
    gnome3.gnome-color-manager
    gnome3.gnome-shell-extensions

    gnomeExtensions.appindicator
    gnomeExtensions.desktop-clock
    gnomeExtensions.gtk4-desktop-icons-ng-ding
    gnomeExtensions.compiz-windows-effect
    gnomeExtensions.burn-my-windows
    gnomeExtensions.user-themes
    gnomeExtensions.gsconnect

  ];
}
