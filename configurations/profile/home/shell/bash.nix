{ config, pkgs, lib, materusArg, ... }:
let
  cfg = config.materus.profile.bash;
in
{
  options.materus.profile.bash.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableTerminal "Enable materus bash config";


  config = lib.mkIf cfg.enable {

    programs.bash = {
      
      enable = true;
      enableCompletion = lib.mkDefault true;
      enableVteIntegration = lib.mkDefault true;
      historyControl = lib.mkDefault [ "erasedups" "ignorespace" ];
      shellOptions = lib.mkDefault [ "autocd" "checkwinsize" "cmdhist" "expand_aliases" "extglob" "globstar" "checkjobs" "nocaseglob" ];
      initExtra = ''
      if [ -n "$EAT_SHELL_INTEGRATION_DIR" ]; then
          source "$EAT_SHELL_INTEGRATION_DIR/bash";
      fi
      '';
    };
  };

}
