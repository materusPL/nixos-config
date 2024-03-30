{ config, pkgs, lib, ... }:
{
  imports =
    [
      ./configuration.nix
      ./nvidia.nix
      ./plasma.nix
    ];



}
