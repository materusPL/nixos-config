{ pkgs, ... }:
{
  imports = [
    #region KDE
    {
      services.displayManager = {
        autoLogin.enable = true;
        autoLogin.user = "materus";
        sddm.enable = true;
        sddm.wayland.enable = true;
      };

      services.desktopManager.plasma6.enable = true;
      services.desktopManager.plasma6.enableQt5Integration = true;
      environment.variables = {
        #Fix for amdgpu crashes
        KWIN_DRM_USE_MODIFIERS = "0";
        KWIN_DRM_NO_DIRECT_SCANOUT = "1";
        QT_PLUGIN_PATH = [
          "${pkgs.qt6.qtimageformats}/${pkgs.qt6.qtbase.qtPluginPrefix}"
          "${pkgs.kdePackages.ffmpegthumbs}/${pkgs.qt6.qtbase.qtPluginPrefix}"
        ];
        XCURSOR_THEME = "breeze_cursors";
      };
    }
    #endregion
  ];

  #region Printing
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  services.printing = {
    enable = true;
    drivers = with pkgs; [
      cups-filters
      cups-browsed
      hplipWithPlugin
    ];
  };
  #endregion
  programs.kdeconnect.enable = true;
  services.libinput.enable = true;

  services.dbus.enable = true;
  services.dbus.packages = [ pkgs.gcr ];

  services.flatpak.enable = true;
  services.gvfs.enable = true;

  services.davfs2.enable = true;

  programs.ssh.startAgent = true;
  services.openssh.enable = true;
  services.openssh.openFirewall = true;
  #region Sunshine
  services.sunshine = {
    enable = true;
    capSysAdmin = true;
    openFirewall = true;
    autoStart = false;
  };
  #endregion
  #region Syncthing
  services.syncthing = {
    enable = true;
    user = "materus";
    dataDir = "/home/materus";
  };
  #endregion
  #region Samba
  services.samba-wsdd.enable = true;
  services.samba-wsdd.openFirewall = true;
  services.samba = {
    enable = true;
    package = pkgs.sambaFull;
    openFirewall = true;
    settings = {
      global = {
        "workgroup" = "WORKGROUP";
        "server string" = "smbmaterus";
        "netbios name " = "smbmaterus";
        "security" = "user";
        "hosts allow" = "192.168.122. 127.0.0.1 localhost";
        "hosts deny" = "0.0.0.0/0";
        "guest account" = "nobody";
        "map to guest" = "bad user";
        "allow insecure wide links" = "yes";
      };
      windows = {
        "path" = "/mkk/data/share/vm_share/";
        "browseable" = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "materus";
        "force group" = "users";
        "follow symlinks" = "yes";
        "wide links" = "yes";
      };

    };
  };
  #endregion
  #region WiVRn
  services.wivrn = {
    enable = true;
    openFirewall = true;
  };
  #endregion
}
