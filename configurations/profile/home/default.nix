{ config, lib, pkgs, materusArg, ... }:
let
  packages = cfg.packages;
  cfg = config.materus.profile;
in
{
  imports = [
    ./fonts.nix
    ./browser.nix
    ./xdg.nix

    ./shell
    ./editor
    ./terminal

  ];
  options.materus.profile.enableDesktop = materusArg.pkgs.lib.mkBoolOpt false "Enable settings for desktop";
  options.materus.profile.enableTerminal = materusArg.pkgs.lib.mkBoolOpt true "Enable settings for terminal";
  options.materus.profile.enableTerminalExtra = materusArg.pkgs.lib.mkBoolOpt false "Enable extra settings for terminal";
  options.materus.profile.enableNixDevel = materusArg.pkgs.lib.mkBoolOpt false "Enable settings for nix devel";

  config =
    {

      home.packages = (if cfg.enableDesktop then packages.list.desktopApps else [ ]) ++
        (if cfg.enableNixDevel then packages.list.nixRelated else [ ]) ++
        (if cfg.enableTerminal then packages.list.terminalApps else [ ]);
      #Desktop
      programs.feh.enable = lib.mkDefault cfg.enableDesktop;
      programs.mpv = lib.mkIf cfg.enableDesktop {
        enable = true;
        config = {
          ytdl-format = "bestvideo+bestaudio";
          slang="pl,pol,Polish,Polski,en,eng,English";
          alang="ja,jp,jpn,Japanese,pl,pol,Polski,en,eng,English";
          demuxer-max-bytes="500MiB";
          demuxer-max-back-bytes="150MiB";
          cache="yes";
          cache-pause-wait="10";
          cache-pause-initial="yes";
          keep-open="yes";
        };
      };

      #Terminal
      programs.git = {
        enable = lib.mkDefault cfg.enableTerminal;
        package = lib.mkDefault pkgs.gitFull;
        delta.enable = lib.mkDefault cfg.enableTerminal;
        lfs.enable = lib.mkDefault cfg.enableTerminal;
      };
      programs.gitui.enable = cfg.enableTerminalExtra;

      programs.nix-index = {
        enable = lib.mkDefault cfg.enableTerminal;
        enableBashIntegration = lib.mkDefault config.programs.bash.enable;
        enableFishIntegration = lib.mkDefault config.programs.fish.enable;
        enableZshIntegration = lib.mkDefault config.programs.zsh.enable;
      };

      programs.direnv = {
        enable = lib.mkDefault (cfg.enableTerminalExtra || cfg.enableNixDevel);
        nix-direnv.enable = lib.mkDefault (cfg.enableNixDevel && (config.programs.direnv.enable == true));
        enableBashIntegration = lib.mkDefault config.programs.bash.enable;
        #enableFishIntegration = lib.mkDefault config.programs.fish.enable;
        enableZshIntegration = lib.mkDefault config.programs.zsh.enable;
      };



      programs.fzf = {
        enable = lib.mkDefault cfg.enableTerminalExtra;
        enableBashIntegration = lib.mkDefault config.programs.bash.enable;
        enableFishIntegration = lib.mkDefault config.programs.fish.enable;
        enableZshIntegration = lib.mkDefault config.programs.zsh.enable;
      };

      programs.eza.enable = lib.mkDefault cfg.enableTerminalExtra;

      programs.yt-dlp.enable = lib.mkDefault cfg.enableTerminalExtra;

    };


}
