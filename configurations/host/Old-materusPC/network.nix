{ config, pkgs, lib, materusArg, ... }:
{
  sops.templates."networkmanager.env".content = ''
    WIREGUARD_PRIVATEKEY="${config.sops.placeholder.wg-key}"
  '';

  networking.useDHCP = lib.mkDefault true;
  networking.hostName = "Old-materusPC";
  networking.wireless.iwd.enable = true;
  networking.networkmanager.enable = true;
  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 24800 5900 5357 4656 
  22000 config.services.syncthing.relay.statusPort config.services.syncthing.relay.port # Syncthing
  ];
  networking.firewall.allowedUDPPorts = [ 24800 5900 3702 4656 
  22000 21027 # Syncthing
  ];
  # Or disable the firewall altogether.
  networking.firewall.enable = true;
  networking.networkmanager.settings = {
    connectivity = {
      uri = "http://nmcheck.gnome.org/check_network_status.txt";
    };
  };

  networking.networkmanager.ensureProfiles.environmentFiles = [
    config.sops.templates."networkmanager.env".path
  ];
  networking.networkmanager.ensureProfiles.profiles = {
    wg0 = {
      connection = {
        id = "wg0";
        type = "wireguard";
        interface-name = "wg0";
      };
      wireguard = {
        private-key = "$WIREGUARD_PRIVATEKEY";
      };
      "wireguard-peer.${materusArg.wireguard.pubKeys.valkyrie}" = {
        endpoint = "${materusArg.ips.valkyrie}:${materusArg.wireguard.port}";
        allowed-ips = "${materusArg.ip-masks.wireguard.main};${materusArg.ip-masks.wireguard.guest};${materusArg.ip-masks.wireguard.asia};${materusArg.ips.wireguard.valkyrie}/32;";
        persistent-keepalive = "20";
      };
      ipv4 = {
        address1 = "${materusArg.ips.wireguard.Old-materusPC}/32";
        dns = "${materusArg.ips.wireguard.valkyrie};";
        method = "manual";
        never-default = "true";
      };
      ipv6 = {
        addr-gen-mode = "stable-privacy";
        method = "disabled";
      };
      proxy = { };
    };
  };
}