{ config, pkgs, lib, materusArg, ... }:
let

  relToDotDir = file: (lib.optionalString (config.programs.zsh.dotDir != null) (config.programs.zsh.dotDir + "/")) + file;
  pluginsDir =
    if config.programs.zsh.dotDir != null then
      relToDotDir "plugins" else "${config.home.homeDirectory}/.zsh/plugins";



  p10kcfg = "${zshcfg}/p10kcfg";
  zshcfg = "${materusArg.cfg.path}" + "/extraFiles/config/zsh";
  cfg = config.materus.profile.zsh;
  #enableStarship = config.materus.starship.enable;

  makeEnv = name: val: ''${name}=''${${name}:-"${val}"}'';
  makeIfVar = var: val: ret: ''
    if [ ''$${var} = "${val}" ]; then 
    ${ret}
    fi'';






  makePlugin = nameArg: fileArg: srcArg: rec {
    name = nameArg;
    src = srcArg;
    path = pluginsDir + "/" + name;
    file = fileArg;
    fullPath = path + "/" + file;
  };

  extraPlugins = {
    powerlevel10k = makePlugin "powerlevel10k" "powerlevel10k.zsh-theme" (pkgs.fetchFromGitHub {
      owner = "romkatv";
      repo = "powerlevel10k";
      rev = "v1.20.0";
      sha256 = "sha256-ES5vJXHjAKw/VHjWs8Au/3R+/aotSbY7PWnWAMzCR8E=";
    });

  };
in
{
  options.materus.profile.zsh.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableTerminalExtra "Enable materus zsh config";
  options.materus.profile.zsh.prompt = lib.mkOption {
    type = lib.types.enum [ "p10k" "starship" ];
    example = "p10k";
    default = "p10k";
  };


  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.ripgrep
    ];

    home.file = builtins.foldl' (a: b: a // b) { } (builtins.map (plugin: { ${plugin.path}.source = plugin.src; }) (builtins.attrValues extraPlugins));

    programs.zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      enableVteIntegration = true;
      historySubstringSearch.enable = true;
      historySubstringSearch.searchUpKey = ";5A";
      historySubstringSearch.searchDownKey = ";5B";


      envExtra = ''
        ${makeEnv "__MATERUS_HM_ZSH" "1"}
        ${makeEnv "__MATERUS_HM_ZSH_PROMPT" cfg.prompt}
      '';


      initExtraFirst = ''
        ${makeIfVar "__MATERUS_HM_ZSH_PROMPT" "p10k" ''
            if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
              source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
            fi
            if [[ -f "${extraPlugins.powerlevel10k.fullPath}" ]]; then
              source "${extraPlugins.powerlevel10k.fullPath}"
            fi
            ''
        }'';



      plugins = [
      ];

      history = {
        extended = true;
        save = 100000;
        size = 100000;
        share = false;
        ignoreDups = true;
        ignoreSpace = true;
      };


      initExtra = ''
        . ${zshcfg}/zinputrc
        source ${zshcfg}/zshcompletion.zsh
        
        bindkey -r "^["
        bindkey ";5C" forward-word
        bindkey ";5D" backward-word
      '' +
      makeIfVar "__MATERUS_HM_ZSH_PROMPT" "p10k" ''
        if zmodload zsh/terminfo && (( terminfo[colors] >= 256 )); then
          [[ ! -f ${p10kcfg}/fullcolor.zsh ]] || source ${p10kcfg}/fullcolor.zsh
        else
          [[ ! -f ${p10kcfg}/compatibility.zsh ]] || source ${p10kcfg}/compatibility.zsh
        fi
      '';

    };

    programs.starship.enableZshIntegration = lib.mkDefault false;
  };


}
