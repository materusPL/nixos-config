{ path, pkgs }:
{

  earlyInitText = ''
    ${builtins.readFile (path + "early-init.el")}
  '';
  initText = ''
    ${builtins.readFile (path + "init.el")}
    

  '';

}
