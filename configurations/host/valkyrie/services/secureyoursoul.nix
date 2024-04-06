{ config, pkgs, lib, materusArg, ... }:
{
  options.valkyrieService.secureyoursoul.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable secureyoursoul, web archive";




  config =
    let
      cfg = config.valkyrieService.secureyoursoul;
    in
    lib.mkIf cfg.enable {
      systemd.timers.secureyoursoul-steam = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-1,7,14,21 3:00:00";
          Persistent = true; 
          Unit = "secureyoursoul-steam.service";
        };
      };
      systemd.timers.secureyoursoul-p1 = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-3,9,16,23 3:00:00";
          Persistent = true; 
          Unit = "secureyoursoul-p1.service";
        };
      };
      systemd.timers.secureyoursoul-p2 = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "*-*-5,11,18,25 3:00:00";
          Persistent = true; 
          Unit = "secureyoursoul-p2.service";
        };
      };

      systemd.services.secureyoursoul-steam = {
        description = "Make curl requests to archive steam related things";
        path = [ pkgs.coreutils pkgs.util-linux pkgs.curl ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = false;
        script = ''
          STEAM_IDS=( ${ builtins.foldl' (x: y:  x +"\""+ y + "\" ") "" materusArg.to_save.steamids })
          EXTRA_LINKS=( ${ builtins.foldl' (x: y:  x +"\""+ y + "\" ") "" materusArg.to_save.extraLinks-steam  })

          steamladder() {
                  for id in ''${STEAM_IDS[@]}; do
                          curl -X POST -H "Authorization: Token ''$(cat ${config.sops.secrets.steamladder-api.path})" \
                          "https://steamladder.com/api/v1/profile/$id/"
                  done;
          }

          webarchive(){
          for id in ''${STEAM_IDS[@]}; do
                  curl -X POST -H "Accept: application/json" \
                          -H "Authorization: LOW ''$(cat ${config.sops.secrets.webarchive-accesskey.path}):''$(cat ${config.sops.secrets.webarchive-secretkey.path})" \
                          -d"url=https://steamcommunity.com/profiles/$id" \
                          -d"capture_outlinks=1" \
                          -d"capture_screenshot=on" \
                          -d"capture_all=on" \
                          "https://web.archive.org/save";
                  sleep 180;
          done;


          for link in ''${EXTRA_LINKS[@]}; do
                  curl -X POST -H "Accept: application/json" \
                          -H "Authorization: LOW ''$(cat ${config.sops.secrets.webarchive-accesskey.path}):''$(cat ${config.sops.secrets.webarchive-secretkey.path})" \
                          -d"url=$link" \
                          -d"capture_outlinks=1" \
                          -d"capture_screenshot=on" \
                          -d"capture_all=on" \
                          "https://web.archive.org/save";
                  sleep 180;
          done;

          }



          steamladder &
          webarchive
          wait
        '';
      };


       systemd.services.secureyoursoul-p1 = {
        description = "Make curl requests to archive related things";
        path = [ pkgs.coreutils pkgs.util-linux pkgs.curl ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = false;
        script = ''
          EXTRA_LINKS=( ${ builtins.foldl' (x: y:  x +"\""+ y + "\" ") "" materusArg.to_save.extraLinks1  })
          webarchive(){
          for link in ''${EXTRA_LINKS[@]}; do
                  curl -X POST -H "Accept: application/json" \
                          -H "Authorization: LOW ''$(cat ${config.sops.secrets.webarchive-accesskey.path}):''$(cat ${config.sops.secrets.webarchive-secretkey.path})" \
                          -d"url=$link" \
                          -d"capture_outlinks=1" \
                          -d"capture_screenshot=on" \
                          -d"capture_all=on" \
                          "https://web.archive.org/save";
                  sleep 180;
          done;

          }
          webarchive
        '';
      };

      systemd.services.secureyoursoul-p2 = {
        description = "Make curl requests to archive related things - part 2";
        path = [ pkgs.coreutils pkgs.util-linux pkgs.curl ];
        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = false;
        script = ''
          EXTRA_LINKS=( ${ builtins.foldl' (x: y:  x +"\""+ y + "\" ") "" materusArg.to_save.extraLinks2  })
          webarchive(){
          for link in ''${EXTRA_LINKS[@]}; do
                  curl -X POST -H "Accept: application/json" \
                          -H "Authorization: LOW ''$(cat ${config.sops.secrets.webarchive-accesskey.path}):''$(cat ${config.sops.secrets.webarchive-secretkey.path})" \
                          -d"url=$link" \
                          -d"capture_outlinks=1" \
                          -d"capture_screenshot=on" \
                          -d"capture_all=on" \
                          "https://web.archive.org/save";
                  sleep 180;
          done;

          }
          webarchive
        '';
      };


    };


}
