{ config, pkgs, lib, materusArg, ... }:
{
  sops.templates."networkmanager.env".content = ''
    WIREGUARD_PRIVATEKEY="${config.sops.placeholder.wireguard}"
  '';

  networking.useDHCP = lib.mkDefault true;
  networking.hostName = "materusPC";
  networking.wireless.iwd.enable = true;
  networking.networkmanager.enable = true;
  #networking.networkmanager.wifi.backend = "iwd";
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 24800 5900 5357 4656 8080 9943 9944 ];
  networking.firewall.allowedUDPPorts = [ 24800 5900 3702 4656 6000 9943 9944 ];
  #Fix warning
  networking.networkmanager.extraConfig = lib.mkDefault ''
    [connectivity]
    uri=http://nmcheck.gnome.org/check_network_status.txt
  '';

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
        allowed-ips = "${materusArg.ip-masks.wireguard.general};";
      };
      ipv4 = {
        address1 = "${materusArg.ips.wireguard.materusPC}/23";
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
