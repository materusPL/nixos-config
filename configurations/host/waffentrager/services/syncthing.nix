{ lib, pkgs, materusArg, config, ... }:
{
  options.waffentragerService.syncthing.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable syncthing";

  config =
    let
      cfg = config.waffentragerService.syncthing;
    in
    lib.mkIf cfg.enable {
      networking.firewall.allowedTCPPorts = [ 22000 config.services.syncthing.relay.statusPort config.services.syncthing.relay.port];
      networking.firewall.allowedUDPPorts = [ 22000 21027 ];
      services = {
        syncthing = {
            enable = true;
            user = "materus";
            dataDir = "${config.waffentragerService.elements.path}/storage/materus";
            configDir = "${config.waffentragerService.elements.path}/storage/materus/Inne/Config/Syncthing/waffentrager/";  
        };
      };
    };
}
