{ config, pkgs, materusFlake, ... }:
{
      imports =
    [
      ./pleroma.nix
      ./pihole.nix
    ];
    services.adguardhome.enable = false;

    valkyrieService.pihole.enable = true;
    valkyrieService.pleroma.enable = true;



}