{ config, pkgs, ... }:
{
  imports = [
    ./nix.nix
    ./fonts.nix
    
    ./shell
    ./games
    ./desktop
  ];
  
}
 