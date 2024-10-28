{ path, pkgs }:
{

  earlyInitText = ''
    ${builtins.readFile (path + "early-init.el")}
  '';
  initText = ''
    (require 'doom-themes nil 'noerror)
    ${builtins.readFile (path + "init.el")}
  '';

}
