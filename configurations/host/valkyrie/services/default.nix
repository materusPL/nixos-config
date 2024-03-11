{ config, pkgs, ... }:
{
  imports =
    [
      ./pleroma.nix
      ./pihole.nix
    ];
  services.adguardhome.enable = true;

  valkyrieService.pihole.enable = false;
  valkyrieService.pleroma.enable = true;



}
