{materusArg, config, pkgs, lib, ...}:
let
cfg = config.materus.profile.zsh;
in
{
  options.materus.profile.zsh.enable = materusArg.pkgs.lib.mkBoolOpt true "Enable materus system zsh config";
  config =  lib.mkIf cfg.enable {
    users.defaultUserShell = pkgs.zsh;
    environment.shells = [ pkgs.zsh ];
    programs.zsh = {
      enable = true;
      enableGlobalCompInit=false;
      interactiveShellInit = ''
        if [[ $(${pkgs.coreutils}/bin/echo $TTY | ${pkgs.gnugrep}/bin/grep "/dev/tty") ]] && [[ "$TERM" == "linux"  ]]; then
           setfont lat2-16 -m 8859-2
        fi

        if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
          source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
        fi
      '';
      promptInit = ''
      '';
    };
  };
}
