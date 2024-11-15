{ config, pkgs, lib, ... }:
let
  mainMirror = "https://ftp.icm.edu.pl/pub/Linux/dist/archlinux";
  extraMirrors = [ ];
  getty = [ 6 7 ];
  ttys = [ 6 7 8 ] ++ getty;

  startPkgs = lib.strings.concatStringsSep " " [ "base" "base-devel" "dbus" "less" "nano" "bash-completion" ];
  scripts = {
    preStart = pkgs.writeShellScript "arch-pre-start" ''
            if [ ! -d "/var/lib/machines/archlinux" ]; then
              export PATH=''${PATH:+''${PATH}:}${lib.strings.makeBinPath (with pkgs; [ wget coreutils-full gnutar zstd ]) }

              ARCH_IMAGE=$(mktemp)
              trap 'rm $ARCH_IMAGE' EXIT

              wget "${mainMirror}/iso/latest/archlinux-bootstrap-x86_64.tar.zst" -O $ARCH_IMAGE
              mkdir -p /var/lib/machines/archlinux
              trap 'rm -rf /var/lib/machines/archlinux' ERR

              tar -xaf $ARCH_IMAGE -C "/var/lib/machines/archlinux" --strip-components=1 --numeric-owner
              printf 'Server = %s/$repo/os/$arch\n' "${mainMirror}" > /var/lib/machines/archlinux/etc/pacman.d/mirrorlist
              rm "/var/lib/machines/archlinux/etc/resolv.conf"
        
              [ -f "/var/lib/machines/archlinux/etc/securetty" ] && \
      	          printf 'pts/%d\n' $(seq 0 10) >>"/var/lib/machines/archlinux/etc/securetty"

              systemd-machine-id-setup --root="/var/lib/machines/archlinux"
              systemd-nspawn -q --settings=false --system-call-filter=@sandbox -D "/var/lib/machines/archlinux" /bin/sh -c "
                export PATH=/bin
                touch /etc/systemd/do-not-udevadm-trigger-on-update
                pacman-key --init && pacman-key --populate
                pacman -Rs --noconfirm arch-install-scripts
                pacman -Sy --noconfirm --needed ${startPkgs}
                pacman -Syu --noconfirm

                systemctl disable getty@tty1.service
                ${lib.strings.concatStringsSep "\n" (lib.lists.forEach getty (x: "systemctl enable getty@tty${builtins.toString x}.service"))}

          
              "
            fi
    '';
  };
in
{
  systemd.nspawn."archlinux" = {
    enable = true;
    execConfig = {
      Boot = true;
      SystemCallFilter = [ "@known" ];
      Timezone = "bind";
      Capability = "all";
      PrivateUsers="no";
    };

    filesConfig = {
      BindReadOnly = [
        "/etc/resolv.conf:/etc/resolv.conf"

        "/nix"

        "/run/current-system"
        "/run/booted-system"
        "/run/opengl-driver"
        "/run/opengl-driver-32"

      ];
      Bind = [
        "/:/run/host-root"

        "/run/udev"

        "/dev/input"
        "/dev/shm"
        "/dev/kfd"
        "/dev/dri"
        "/dev/tty"
        "/dev/tty0"

        "/tmp/.X11-unix"

        /materus

      ] ++ lib.lists.forEach ttys (x: "/dev/tty${builtins.toString x}");
    };
    networkConfig = {
      Private = false;
    };
  };
  systemd.services."systemd-nspawn@archlinux" = {
    enable = true;
    preStart = "${scripts.preStart}";
    overrideStrategy = "asDropin";
    serviceConfig = {
      DeviceAllow = [ "char-tty rwm" "char-input rwm" "char-drm rwm" ];
    };
  };
}
