{materusArg, config, pkgs, lib, ...}:
let
cfg = config.materus.profile.zsh;
in
{
  options.materus.profile.zsh.enable = materusArg.pkgs.lib.mkBoolOpt true "Enable materus system zsh config";
  config =  lib.mkIf cfg.enable {
    programs.zsh = {
      enable = true;
      enableGlobalCompInit=false;
      interactiveShellInit = ''
        if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
          source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
        fi
      '';
      promptInit = ''
      '';
    };
  };
}
