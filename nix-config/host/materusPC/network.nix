{
  config,
  pkgs,
  lib,
  mkk,
  ...
}:
{
  sops.templates."networkmanager.env".content = ''
    WIREGUARD_PRIVATEKEY="${config.sops.placeholder.wireguard}"
  '';

  networking.hostName = "materusPC";
  networking.wireless.iwd.enable = true;
  networking.networkmanager.enable = true;
  networking.firewall.enable = false;

  networking.networkmanager.ensureProfiles.environmentFiles = [
    config.sops.templates."networkmanager.env".path
  ];
  networking.networkmanager.ensureProfiles.profiles = {
    wg0 = {
      connection = {
        id = "PodKos";
        type = "wireguard";
        interface-name = "wg-podkos";
      };
      wireguard = {
        private-key = "$WIREGUARD_PRIVATEKEY";
      };
      "wireguard-peer.${mkk.wireguard.peers.valkyrie.pubKey}" = {
        endpoint = "${mkk.network.valkyrie.ip}:${mkk.wireguard.peers.valkyrie.port}";
        allowed-ips = "${mkk.wireguard.ip-masks.main};${mkk.wireguard.ip-masks.guest};${mkk.wireguard.ip-masks.asia};${mkk.wireguard.peers.valkyrie.ip}/32;";
        persistent-keepalive = "20";
      };
      ipv4 = {
        address1 = "${mkk.wireguard.peers.materusPC.ip}/32";
        dns = "${mkk.wireguard.peers.valkyrie.ip};";
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
