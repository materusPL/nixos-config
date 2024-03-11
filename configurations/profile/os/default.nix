{ config, pkgs, ... }:
{
  imports = [
    ./nix.nix
    ./fonts.nix

    ./games
  ];
  /*
    users.users.nixos-rebuild = {
    #isSystemUser = true;
    isNormalUser = true;
    group = "nixos-rebuild";
    openssh.authorizedKeys.keys = [
     "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPEDY+H8Hc/RSLE064AAh8IojvqxPd8BE5gec2aOfYMh materus@podkos.pl"
    ];
    home = "/tmp/nixos-rebuild";
    };
    users.groups.nixos-rebuild = { };
    security.sudo.extraRules = [
    {
      users = [ "nixos-rebuild" ];
      commands = let path = "/run/current-system/sw/bin/"; in
        [
          {
            command =
              "${path}nixos-rebuild";
            options = [ "NOPASSWD" ];
          }
          {
            command =
              "${path}nix-store";
            options = [ "NOPASSWD" ];
          }
          {
            command =
              "${path}nix-env";
            options = [ "NOPASSWD" ];
          }
          {
            command =
              "${path}nix-build";
            options = [ "NOPASSWD" ];
          }
          {
            command =
              "${path}nix-instantiate";
            options = [ "NOPASSWD" ];
          }
          {
            command =
              "${path}nix-channel";
            options = [ "NOPASSWD" ];
          }
        ];


    }
    ];
  */
}
 