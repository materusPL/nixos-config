{ config, pkgs, lib, materusArg, ... }:
{
  sops.templates."networkmanager.env".content = ''
    WIREGUARD_PRIVATEKEY="${config.sops.placeholder.wireguard}"
  '';

  networking.firewall = {
   logReversePathDrops = false;
   # wireguard trips rpfilter up
   extraCommands = ''
     ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --sport ${materusArg.wireguard.port} -j RETURN
     ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --dport ${materusArg.wireguard.port} -j RETURN
   '';
   extraStopCommands = ''
     ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --sport ${materusArg.wireguard.port} -j RETURN || true
     ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --dport ${materusArg.wireguard.port} -j RETURN || true
   '';
  };

  networking.useDHCP = lib.mkDefault true;
  networking.hostName = "materusPC";
  networking.wireless.iwd.enable = true;
  networking.networkmanager.enable = true;
  #networking.networkmanager.wifi.backend = "iwd";
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 
    24800 5900 5357 4656 8080 9943 9944 
    22000 config.services.syncthing.relay.statusPort config.services.syncthing.relay.port # Syncthing
    25565 8100 # Minecraft + BlueMap 
  ];
  networking.firewall.allowedUDPPorts = [ (lib.strings.toInt materusArg.wireguard.port) 
    24800 5900 3702 4656 6000 9943 9944 
    22000 21027 # Syncthing
    17000 17001 # zomboid
    24454 # Minecraft Voice Chat
  ];
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
        address1 = "${materusArg.ips.wireguard.materusPC}/32";
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
      services = {
        syncthing = {
            enable = true;
            user = "materus";
            dataDir = "/home/materus";
        };
      };
}
