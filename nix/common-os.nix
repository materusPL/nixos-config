# * Common OS
{
  mkkArg,
  config,
  ...
}:
{

  imports = [
    mkkArg.current.sops-nix.nixosModules.sops
# * Config
# ** Assertions
    {
      assertions = [
        {
          assertion = builtins.pathExists (config.konfig.vars.path.mkk + "/host/keys/ssh_host_ed25519_key");
          message = "Not found host ed25519 key";
        }
        {
          assertion = builtins.pathExists (config.konfig.vars.path.mkk + "/host/keys//ssh_host_rsa_key");
          message = "Not found host RSA key";
        }
      ];
    }
# ** Variables 
    {
      mkk.commonVariables = {
        path = {
          mkk = "/mkk";
        };
      };
    }
# * Common OS END
  ];
}
