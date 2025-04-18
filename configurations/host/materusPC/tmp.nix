{
  config,
  pkgs,
  materusArg,
  ...
}:

{

  programs.gamemode.enable = true;
  programs.corectrl.enable = true;

  services.teamviewer.enable = true;

  systemd.tmpfiles.rules = [ "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}" ];

  services.flatpak.enable = true;
  services.gvfs.enable = true;

  services.xserver.xkb.layout = "pl";

  services.xserver.enable = true;
  services.xserver.videoDrivers = [ "amdgpu" ];
  services.dbus.enable = true;
  services.dbus.packages = [ pkgs.gcr ];

  services.xserver.displayManager.startx.enable = false;

  services.xserver.exportConfiguration = true;
  services.xserver.extraConfig = pkgs.lib.mkDefault ''
    Section "OutputClass"
      Identifier "amd-options"
      Option "TearFree" "True"
      Option "SWCursor" "True"
      Option "VariableRefresh" "true"
      Option "AsyncFlipSecondaries" "true"
      Option "DRI3" "1"
      MatchDriver "amdgpu"
    EndSection

  '';

  services.printing.enable = true;

  services.libinput.enable = true;

  environment.sessionVariables = {
    XDG_CACHE_HOME = "\${HOME}/.cache";
    XDG_CONFIG_HOME = "\${HOME}/.config";
    XDG_BIN_HOME = "\${HOME}/.local/bin";
    XDG_DATA_HOME = "\${HOME}/.local/share";
    QT_XKB_CONFIG_ROOT = "\${XKB_CONFIG_ROOT}";
    GTK_IM_MODULE = "fcitx";
    QT_IM_MODULE = "fcitx";
    XMODIFIERS = "@im=fcitx";
    SDL_IM_MODULE = "fcitx";

    MOZ_USE_XINPUT2 = "1";
    PATH = [ "\${XDG_BIN_HOME}" ];
  };
  environment.shellInit = ''
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:root &> /dev/null; fi;
    if ! [ -z "$DISPLAY" ]; then xhost +si:localuser:$USER &> /dev/null; fi;
  '';

  i18n.inputMethod.enabled = "fcitx5";
  i18n.inputMethod.fcitx5.addons = [
    pkgs.kdePackages.fcitx5-configtool
    pkgs.fcitx5-lua
    pkgs.fcitx5-mozc
    pkgs.fcitx5-gtk
    pkgs.kdePackages.fcitx5-qt
  ];

  services.pcscd.enable = true;

  services.samba-wsdd.enable = true;
  services.samba-wsdd.openFirewall = true;
  services.samba = {
    enable = true;
    package = pkgs.sambaFull;
    securityType = "user";
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
        "path" = "/materus/data/VM/windows_shared";
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

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = false;
    enableBrowserSocket = true;

  };
  programs.ssh.startAgent = true;
  services.openssh.enable = true;
  services.openssh.settings.PermitRootLogin = "no";
  services.openssh.settings.PasswordAuthentication = false;
  services.openssh.openFirewall = true;

  services.sunshine = {
    enable = true;
    capSysAdmin = true;
    openFirewall = true;
    autoStart = false;
  };

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplipWithPlugin ];

  environment.enableAllTerminfo = true;
  environment.pathsToLink = [
    "/share/zsh"
    "/share/bash-completion"
    "/share/fish"
  ];
  environment.shells = with pkgs; [
    zsh
    bashInteractive
    fish
  ];
  programs = {
    fish.enable = true;
    command-not-found.enable = false;
    dconf.enable = true;
  };

  materus.profile.browser.enable = true;

  services.davfs2.enable = true;

}
