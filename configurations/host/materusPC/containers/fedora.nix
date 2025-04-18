{
  config,
  pkgs,
  lib,
  ...
}:
let
  ttys = [
    9
    10
  ];

in
{
  systemd.nspawn."fedora" = {
    enable = true;
    execConfig = {
      Boot = true;
      SystemCallFilter = [ "@known" ];
      Timezone = "symlink";
      Capability = "all";
      PrivateUsers = "no";
      ResolvConf = "off";
    };

    filesConfig = {
      BindReadOnly = [
        "/nix"

        "/run/current-system"
        "/run/booted-system"
        "/run/opengl-driver"
        "/run/opengl-driver-32"

      ];
      Bind = [
        "/:/run/host-root"

        "/run/udev"

        "/dev/fuse"
        "/dev/snd"
        "/dev/input"
        "/dev/shm"
        "/dev/kfd"
        "/dev/dri"
        "/dev/tty"
        "/dev/tty0"

        "/var/lib/flatpak"

        "/tmp/.X11-unix"

        /materus

      ] ++ lib.lists.forEach ttys (x: "/dev/tty${builtins.toString x}");
    };
    networkConfig = {
      Bridge="br0";
    };
  };
  systemd.services."systemd-nspawn@fedora" = {
    enable = true;
    overrideStrategy = "asDropin";
    serviceConfig = {
      ConditionPathExists="/var/lib/machines/fedora";
      DeviceAllow = [
        "char-tty rwm"
        "char-input rwm"
        "char-drm rwm"
      ];
      
    };
  };
}
