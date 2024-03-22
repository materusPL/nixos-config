{ config, pkgs, ... }:
{
  imports =
    [
      ./pleroma.nix
      ./pihole.nix
      ./muse.nix
    ];
  services.adguardhome.enable = true;

  valkyrieService.pihole.enable = false;
  valkyrieService.pleroma.enable = true;
  valkyrieService.muse.enable = true;

}
