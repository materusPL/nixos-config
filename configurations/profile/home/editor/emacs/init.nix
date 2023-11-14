{path, pkgs}:
{

  earlyInitText = ''
  ${builtins.readFile (path + "early-init.el")}
  '';
  initText = ''
      (defvar materus/init-early nil)
      (unless materus/init-early
        (tool-bar-mode -1)
        (setq initial-major-mode 'fundamental-mode)
        (setq-default package-quickstart t)
        (setq native-comp-speed 3)
      )
      (defvar materus/init-from-home nil)
      (when materus/init-from-home
        (setq-default inhibit-defaul-init 1)
      )
    (setq-default materus/nix-packages (require 'doom-themes nil 'noerror))
      ${builtins.readFile (path + "packages.el")}
      ${builtins.readFile (path + "init.el")}
  '';
  
}
