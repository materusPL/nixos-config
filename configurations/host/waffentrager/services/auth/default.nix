{ config, materusArg, lib, pkgs, ... }:
{
  imports =
    [
      ./lldap.nix
    ];
  config = 
    {
      waffentragerService.auth.lldap.enable = true;
    };
}
