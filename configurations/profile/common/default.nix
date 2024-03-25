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
    ./private
  ];
  options.materus.materusArg = lib.mkOption { default = { }; };
  config._module.args.materusArg = config.materus.materusArg // materusArg;
  config.assertions = [
    {
      assertion = materusCfg.materusFlake.decrypted;
      message = "Repository not decrypted, use crypt.sh to decrypt";
    }
  ];



}
