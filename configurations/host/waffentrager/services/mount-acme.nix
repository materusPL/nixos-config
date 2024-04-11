{ materusArg, config, lib, pkgs, ... }:
{
  options.waffentragerService.mount-acme.enable = materusArg.pkgs.lib.mkBoolOpt false "Enable mount-acme";

  config =
    let
      cfg = config.waffentragerService.mount-acme;
    in
    lib.mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ sshfs ];
      systemd.mounts = [{
        description = "Mount remote acme dir from valkyrie";
        what = "acme@valkyrie:/var/lib/acme";
        where = "/var/lib/mnt_acme";
        type = "fuse.sshfs";
        options = "reconnect,gid=${builtins.toString config.ids.gids.nginx},_netdev,rw,nosuid,allow_other,default_permissions,follow_symlinks,idmap=user,compression=yes,identityfile=/materus/root/ssh_host_ed25519_key";
        wantedBy = [ "multi-user.target" ];
      }];
    };
}
