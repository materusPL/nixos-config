{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./secrets
      ./configuration.nix
      ./nvidia.nix
      ./plasma.nix
    ];



}
