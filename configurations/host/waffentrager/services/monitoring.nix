{ materusArg, config, lib, ... }:
{
  options.waffentragerService.monitoring.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable monitoring";
  config =
    let
      cfg = config.waffentragerService.monitoring;
    in
    lib.mkIf cfg.enable {
      services.grafana = {
        dataDir = "${config.waffentragerService.elements.path}/services/grafana";
        enable = true;
        settings = {
          server = {
            # Listening Address
            http_addr = "127.0.0.1";
            # and Port
            http_port = 3232;
            # Grafana needs to know on which domain and URL it's running
            domain = "watchman.materus.pl";
            serve_from_sub_path = true;
          };
        };
      };
      services.prometheus = {
        enable = true;
        port = 3233;
        globalConfig.scrape_interval = "30s";
        stateDir = "elements/services/prometheus";
        scrapeConfigs = [
          {
            job_name = "node";
            static_configs = [{
              targets = [ "localhost:${toString config.services.prometheus.exporters.node.port}" ];
            }];
          }
        ];
      };
      services.prometheus.exporters.node = {
        enable = true;
        port = 3234;
        enabledCollectors = [ "systemd" ];
        extraFlags = [ "--collector.ethtool" "--collector.softirqs" "--collector.tcpstat" "--collector.wifi" ];
        
      };
      services.nginx.virtualHosts."watchman.materus.pl" = {
        addSSL = true;
        sslTrustedCertificate = "/var/lib/mnt_acme/materus.pl/chain.pem";
        sslCertificateKey = "/var/lib/mnt_acme/materus.pl/key.pem";
        sslCertificate = "/var/lib/mnt_acme/materus.pl/fullchain.pem";
        http2 = false;
        http3 = true;
        locations."/" = {
          proxyPass = "http://${toString config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
          proxyWebsockets = true;
          recommendedProxySettings = true;
        };
      };
    };
}
