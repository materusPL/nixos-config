{ ... }:
{
  imports =
    [
      ./elements.nix
      ./postgresql.nix
      ./mount-acme.nix
      ./gitea.nix
      ./nginx.nix
      ./nextcloud.nix
    ];
  waffentragerService.elements.enable = true;
  waffentragerService.postgresql.enable = true;
  waffentragerService.mount-acme.enable = true;
  waffentragerService.gitea.enable = true;
  waffentragerService.nginx.enable = true;
  waffentragerService.nextcloud.enable = true;
}