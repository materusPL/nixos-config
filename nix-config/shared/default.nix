isHm:
{
  lib,
  materusArgs,
  config,
  ...
}:
{

  options.mkk.dir = lib.mkOption {
    default = "${materusArgs.flake-path}";
    type = lib.types.path;
  };

  options.mkk.var = lib.mkOption {
    default = { };
    type = lib.types.attrs;
  };

  imports = [
    ./nix.nix
  ]
  ++ lib.optionals isHm [ ./hm ]
  ++ lib.optionals (!isHm) [ ./os ];

  config.mkk.var = import ./private/variables.nix {};
  config._module.args.mkk = rec {
    nixerus.pkgs = materusArgs.inputs.nixerus.packages."${config.nixpkgs.hostPlatform.system}";
    lib = nixerus.pkgs.lib;
    files = materusArgs.files;
    args = materusArgs;
  } // config.mkk.var;
}
