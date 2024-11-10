{config, pkgs, lib, ...}:
{

  imports = [
    ./zsh.nix
    ./bash.nix
    ./fish.nix
    ./starship.nix
  ];
  home.sessionVariables = {
    EDITOR = lib.mkDefault (if (config.materus.profile.editor.emacs.enable) then 
    lib.getBin (pkgs.writeShellScript "editor" ''${config.programs.emacs.finalPackage}/bin/emacsclient --alternate-editor= -c  -nw $@'') 
    else "${pkgs.micro}/bin/micro");
    VISUAL = lib.mkDefault (if (config.materus.profile.editor.emacs.enable) then 
    lib.getBin (pkgs.writeShellScript "editor-visual" ''exec env COLORTERM=truecolor ${config.programs.emacs.finalPackage}/bin/emacsclient --alternate-editor= -c $@'')
    else "${pkgs.micro}/bin/micro");
  };
}
