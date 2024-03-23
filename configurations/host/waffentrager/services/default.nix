{ ... }:
{
  imports =
    [
      ./elements.nix
      ./postgresql.nix
    ];
  waffentragerService.elements.enable = true;
  waffentragerService.postgresql.enable = true;
}