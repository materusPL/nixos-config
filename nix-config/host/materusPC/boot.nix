{
  pkgs,
  lib,
  materusArgs,
  mkk,
  ...
}:
{
  boot.supportedFilesystems = [
    "ntfs"
    "btrfs"
    "vfat"
    "exfat"
    "ext4"
  ];

  boot.tmp.useTmpfs = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.grub = {
    enable = true;
    efiSupport = true;
    device = "nodev";
    gfxmodeEfi = pkgs.lib.mkDefault "1920x1080@240";
    gfxmodeBios = pkgs.lib.mkDefault "1920x1080@240";
    useOSProber = true;
    memtest86.enable = true;
  };

  boot.plymouth.enable = true;

  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];
  boot.kernelParams = [ "ip=${mkk.local}" ];
  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackagesFor pkgs.linux_zen;

  boot.initrd = {

    availableKernelModules = [
      "r8169"
      "wireguard"
    ];
    luks.devices."ROOT_1".device = "/dev/disk/by-label/CRYPT_ROOT_1";
    luks.devices."ROOT_2".device = "/dev/disk/by-label/CRYPT_ROOT_2";
    secrets."/etc/secrets/30-wg-initrd.key" = "/mkk/keys/wireguard";
    systemd = {
      enable = true;
      network = {
        netdevs."30-wg-initrd" = {
          netdevConfig = {
            Kind = "wireguard";
            Name = "wg-initrd";
          };
          wireguardConfig = {
            PrivateKeyFile = "/etc/secrets/30-wg-initrd.key";
          };
          wireguardPeers = [
            {
              AllowedIPs = [
                "${mkk.wireguard.ip-masks.main}"
                "${mkk.wireguard.peers.valkyrie.ip}/32"
              ];
              PublicKey = "${mkk.wireguard.peers.valkyrie.pubKey}";
              Endpoint = "${mkk.network.valkyrie.ip}:${mkk.wireguard.peers.valkyrie.port}";
              PersistentKeepalive = 25;
            }
          ];
        };
        networks."30-wg-initrd" = {
          name = "wg-initrd";
          addresses = [ { Address = "${mkk.wireguard.peers.materusPC.ip}/32"; } ];
        };
        networks."10-lan" = {
          matchConfig.Name = "eno1";
          networkConfig.DHCP = "yes";
        };

      };
    };
    network = {
      enable = true;
      flushBeforeStage2 = true;
      ssh = {
        enable = true;
        port = 22;
        authorizedKeyFiles = [ materusArgs.files.ssh-keys.materus ];
        hostKeys = [
          "/mkk/keys/ssh_host_ed25519_key"
          "/mkk/keys/ssh_host_rsa_key"
        ];
      };
    };
  };
}
