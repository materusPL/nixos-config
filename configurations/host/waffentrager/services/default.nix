{ ... }:
{
  imports =
    [
      ./storage/elements.nix
      ./storage/mount-acme.nix
      ./storage/gitea.nix
      ./storage/nextcloud.nix
      ./storage/samba.nix
      ./storage/syncthing.nix
      ./multimedia/jellyfin.nix
      ./multimedia/scrobbling.nix
      ./monitoring.nix
      ./nginx.nix
      ./postgresql.nix
      ./auth
    ];
  waffentragerService.elements.enable = true;
  waffentragerService.postgresql.enable = true;
  waffentragerService.mount-acme.enable = true;
  waffentragerService.gitea.enable = true;
  waffentragerService.nginx.enable = true;
  waffentragerService.nextcloud.enable = true;
  waffentragerService.samba.enable = true;
  waffentragerService.jellyfin.enable = true;
  waffentragerService.scrobbling.enable = true;

  waffentragerService.syncthing.enable = true;
  waffentragerService.monitoring.enable = true;
}