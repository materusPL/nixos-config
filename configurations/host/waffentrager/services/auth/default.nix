{ config, materusArg, lib, pkgs, ... }:
{
  imports =
    [
      ./lldap.nix
      ./authelia.nix
    ];
  config = 
    {
      waffentragerService.auth.lldap.enable = true;
      waffentragerService.auth.authelia.enable = true;
    };
}
