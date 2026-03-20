isHm:
{ lib, materusArgs, ... }:
{

  options.mkk.dir = lib.mkOption {
    default = "${materusArgs.flake-path}";
    type = lib.types.path;
  };

  imports = [
    (import ./nvim.nix isHm)
  ];

  config._module.args.mkk = import ./private/variables.nix;

}
