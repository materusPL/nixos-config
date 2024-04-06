{ config, pkgs, ... }:
{
  imports =
    [
      ./pleroma.nix
      ./pihole.nix
      ./dcbot.nix
      ./secureyoursoul.nix
    ];
  services.adguardhome.enable = true;

  valkyrieService.pihole.enable = false;
  valkyrieService.pleroma.enable = true;
  valkyrieService.dcbot.enable = true;
  valkyrieService.secureyoursoul.enable = true;

}
