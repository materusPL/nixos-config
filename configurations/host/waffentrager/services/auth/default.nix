{ config, materusArg, lib, pkgs, ... }:
let
  cfg = config.waffentragerService.auth;
in
{
  imports =
    [
      ./samba.nix
    ];
  config = 
    {

    };
}
