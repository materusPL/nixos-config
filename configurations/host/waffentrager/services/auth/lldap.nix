{ config, pkgs, lib, materusArg, ... }:
{
  options.waffentragerService.auth.lldap.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable lldap";
  config =
    let
      cfg = config.waffentragerService.auth.lldap;
    in
    lib.mkIf cfg.enable {
      waffentragerService.elements.enable = true;
      systemd.services.lldap = {
        partOf = [ "elements-mount.service" ];
        requires = [ "elements-mount.service" ];
        after = [ "elements-mount.service" ];
        serviceConfig = {
          DynamicUser = lib.mkForce false;
          WorkingDirectory = lib.mkForce config.waffentragerService.elements.lldapDir;
        };
      };
      users.groups.lldap = { };
      users.users.lldap = {
        group = "lldap";
        isSystemUser = true;
      };
      sops.secrets.jwt = { owner = "lldap"; group = "lldap";};
      services.lldap.enable = true;
      services.lldap.environment = {
        LLDAP_JWT_SECRET_FILE = config.sops.secrets.jwt.path;
      };
      services.lldap.settings = {
        ldap_base_dn = "dc=podkos,dc=pl";
        database_url = "sqlite://${config.waffentragerService.elements.lldapDir}/users.db?mode=rwc";
        http_url = "http://mamba.podkos.pl";
        ldap_user_dn = "master";
        ldap_user_email = "materus@podkos.pl";
        key_seed = materusArg.waffentrager.lldap.seed;
      };
    };
}
