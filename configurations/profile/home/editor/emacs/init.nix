{path, pkgs}:
{

  earlyInitText = ''
  ${builtins.readFile (path + "early-init.el")}
  '';
  initText = ''
      (defvar materus/init-from-home nil)
      (when materus/init-from-home
        (setq-default inhibit-defaul-init 1)
      )
    (setq-default materus/nix-packages (require 'doom-themes nil 'noerror))
      ${builtins.readFile (path + "packages.el")}
      ${builtins.readFile (path + "init.el")}
  '';
  
}
