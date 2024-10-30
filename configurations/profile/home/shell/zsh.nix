{ config, pkgs, lib, materusArg, ... }:
let

  relToDotDir = file: (lib.optionalString (config.programs.zsh.dotDir != null) (config.programs.zsh.dotDir + "/")) + file;
  pluginsDir =
    if config.programs.zsh.dotDir != null then
      relToDotDir "plugins" else "${config.home.homeDirectory}/.zsh/plugins";



  p10kcfg = "${zshcfg}/p10kcfg";
  zshcfg = "${materusArg.cfg.path}" + "/extraFiles/config/zsh";
  cfg = config.materus.profile.zsh;

  makeEnv = name: val: ''${name}=''${${name}:-"${val}"}'';
  makeIfVar = var: val: ret: ''
    if [[ "''$${var}" = "${val}" ]]; then 
      ${ret}
    fi
  '';






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
    sudo = makePlugin "sudo" "sudo.plugin.zsh" "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/sudo";
    extract = makePlugin "extract" "extract.plugin.zsh" "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/extract";


  };
in
{
  options.materus.profile.zsh.enable = materusArg.pkgs.lib.mkBoolOpt config.materus.profile.enableTerminalExtra "Enable materus zsh config";
  options.materus.profile.zsh.prompt = lib.mkOption {
    type = lib.types.enum [ "p10k" ];
    example = "p10k";
    default = "p10k";
  };
  options.materus.profile.zsh.endConfig = lib.mkOption {
    default = "";
    description = "Zsh config after all of config";
    type = lib.types.lines;
  };


  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.ripgrep
    ];

    home.file = lib.mkMerge [
      (builtins.foldl' (a: b: a // b) { } (builtins.map (plugin: { ${plugin.path}.source = plugin.src; }) (builtins.attrValues extraPlugins)))
      { "${relToDotDir ".zshrc"}".text = lib.mkAfter cfg.endConfig; }
    ];

    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      enableVteIntegration = true;
      historySubstringSearch.enable = true;
      historySubstringSearch.searchUpKey = "$key[Up]";
      historySubstringSearch.searchDownKey = "$key[Down]";


      envExtra = ''
        ${makeEnv "__MATERUS_HM_ZSH" "1"}
        ${makeEnv "__MATERUS_HM_ZSH_PROMPT" cfg.prompt}
        ${makeEnv "__MATERUS_HM_ZSH_PRIVATE" "0"}
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
        }
        if zmodload zsh/terminfo && (( "$terminfo[colors]" >= "256" )); then 
          __MATERUS_HM_ZSH_256COLORS="''${__MATERUS_HM_ZSH_256COLORS:-1}"; else
          __MATERUS_HM_ZSH_256COLORS="''${__MATERUS_HM_ZSH_256COLORS:-0}"; 
        fi

        if [[ -f "${extraPlugins.sudo.fullPath}" ]]; then
          source "${extraPlugins.sudo.fullPath}"
        fi
        if [[ -f "${extraPlugins.extract.fullPath}" ]]; then
          source "${extraPlugins.extract.fullPath}"
          path+="${extraPlugins.extract.path}"
          fpath+="${extraPlugins.extract.path}"
        fi
        '';



      plugins = [
      ];

      history = {
        extended = true;
        save = 100000;
        size = 100000;
        share = true;
        ignoreDups = true;
        ignoreAllDups = true;
        ignoreSpace = true;
      };

      completionInit = ''
      ZSH_COMPDUMP="''${ZSH_COMPDUMP:-''${XDG_CACHE_HOME:-${config.home.homeDirectory}/.cache}/.zcompdump-''${HOST}-''${ZSH_VERSION}}"
      autoload -U compinit && compinit -d $ZSH_COMPDUMP
      '';
      initExtra = ''
        if [[ "$__ETC_ZSHRC_SOURCED" != "1" ]]; then
          . ${zshcfg}/zinputrc
        fi
        source ${zshcfg}/zshcompletion.zsh

        history-substring-search-up-prefixed(){
          HISTORY_SUBSTRING_SEARCH_PREFIXED=1 history-substring-search-up
        }
        history-substring-search-down-prefixed(){
          HISTORY_SUBSTRING_SEARCH_PREFIXED=1 history-substring-search-down
        }


        zle -N history-substring-search-up-prefixed
        zle -N history-substring-search-down-prefixed


        bindkey -r "^["
        bindkey "^[[1;5C" forward-word
        bindkey "^[[1;5D" backward-word
        bindkey "^[[1;5A" history-substring-search-up-prefixed
        bindkey "^[[1;5B" history-substring-search-down-prefixed

        zsh-private() {
          __MATERUS_HM_ZSH_PRIVATE=1 ${lib.getExe config.programs.zsh.package}
        }

        myip() {
          ${lib.getExe pkgs.wget} -qO- https://wtfismyip.com/text
        }

        speedtest() {
          ${lib.getExe pkgs.curl} -s https://raw.githubusercontent.com/sivel/speedtest-cli/master/speedtest.py | ${lib.getExe pkgs.python3} 
        }
        ${lib.optionalString config.materus.profile.editor.emacs.enable ''alias "e"="emacsclient -nw -c --alternate-editor= "''}
        


      '' +
      makeIfVar "__MATERUS_HM_ZSH_PROMPT" "p10k" ''
        if [[ "$__MATERUS_HM_ZSH_256COLORS" = "1" ]] ; then
          [[ ! -f ${p10kcfg}/fullcolor.zsh ]] || source ${p10kcfg}/fullcolor.zsh
        else
          [[ ! -f ${p10kcfg}/compatibility.zsh ]] || source ${p10kcfg}/compatibility.zsh
        fi
      '' + makeIfVar "__MATERUS_HM_ZSH_PRIVATE" "1" ''
        unset HISTFILE
        ${lib.optionalString config.programs.zsh.history.share "unsetopt SHARE_HISTORY"}
        alias -- 'zsh'="__MATERUS_HM_ZSH_PRIVATE=0 zsh "
      ''

      ;

    };

    programs.starship.enableZshIntegration = lib.mkDefault false;
  };


}
