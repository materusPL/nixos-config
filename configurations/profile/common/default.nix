{ config, pkgs, lib, materusCfg, ... }:
let
materusArg = {
  pkgs = (import materusCfg.nixerus { inherit pkgs; }) //
  (if pkgs.system == "x86_64-linux" then { i686Linux = import materusCfg.nixerus { pkgs = pkgs.pkgsi686Linux; }; } else { });
  cfg = materusCfg;
};
in
{
  imports = [
    ./nixpkgs.nix
    ./packages
  ];
  config._module.args.materusArg = materusArg;
}
