{ config, pkgs, ... }:
{
  imports =
    [
      ./pleroma.nix
      ./pihole.nix
      ./dcbot.nix
      ./saveyoursoul.nix
    ];
  services.adguardhome.enable = true;

  valkyrieService.pihole.enable = false;
  valkyrieService.pleroma.enable = true;
  valkyrieService.dcbot.enable = true;
  valkyrieService.saveyoursoul.enable = true;

}
