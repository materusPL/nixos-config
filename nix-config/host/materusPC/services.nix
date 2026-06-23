{ pkgs, mkk, ... }:
{
  imports = [
      #region Suspend/sleep
    {
      systemd.services.pre-suspend = {
        description = "Service description here";
        wantedBy = [ "suspend.target" "sleep.target" ];
        before = [
          "suspend.target"
          "sleep.target"
        ];
        script = ''
          if [ $(systemctl is-active systemd-nspawn@archlinux) = "active" ]; then
            systemctl stop systemd-nspawn@archlinux; 
            sleep 1s;
            while [ $(systemctl is-active systemd-nspawn@archlinux) = "active" ]; do sleep 1s; done;
          fi
          if [ $(systemctl is-active windows-share-mount.service) = "active" ]; then
            systemctl stop windows-share-mount.service
          fi
        '';
        serviceConfig.Type = "oneshot";
      };

      systemd.services.post-suspend = {
        description = "Service description here";
        wantedBy = [ "suspend.target" "sleep.target" ];
        after = [
          "suspend.target"
          "sleep.target"
        ];
        script = ''
           systemctl start windows-share-mount.service
        '';
        serviceConfig.Type = "oneshot";
      };
    }
    #endregion
    #region KDE
    {
      services.displayManager = {
        autoLogin.enable = true;
        autoLogin.user = "materus";
        plasma-login-manager.enable = true;
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
      environment.systemPackages = with pkgs.kdePackages; [
        kolourpaint
        kcolorchooser
        kcolorpicker
        ark
        bluedevil
        bluez-qt
        kcalc
        kcron
      ];
      xdg.portal.enable = true;
      xdg.portal.wlr.enable = true;
      xdg.portal.xdgOpenUsePortal = true;
      xdg.portal.extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
      environment.plasma6.excludePackages = with pkgs.kdePackages; [
        kwallet
        kwalletmanager
        kwallet-pam
      ];
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
    package = mkk.nixerus.pkgs.sunshine;
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
