{ config, pkgs, lib, materusFlake, inputs, ... }:
{
  imports = [
    ./nixpkgs.nix
    ./packages
  ];
  config._module.args.materusPkgs = (import inputs.nixerus { inherit pkgs; }) //
  (if pkgs.system == "x86_64-linux" then { i686Linux = import inputs.nixerus { pkgs = pkgs.pkgsi686Linux; }; } else { });
}
