{ ... }:
{
  imports =
    [
      ./elements.nix
      ./postgresql.nix
      ./mount-acme.nix
    ];
  waffentragerService.elements.enable = true;
  waffentragerService.postgresql.enable = true;
  waffentragerService.mount-acme.enable = true;
}