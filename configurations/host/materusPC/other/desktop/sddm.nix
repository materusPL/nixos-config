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
}