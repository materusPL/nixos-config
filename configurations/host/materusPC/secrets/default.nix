{ config, pkgs, lib, ... }:
{
  imports =
    [
    ];
    sops.age.keyFile = "/materus/root/age.key";
    sops.age.generateKey = false;
    sops.gnupg.home = null;
    sops.gnupg.sshKeyPaths = [];
    sops.secrets.users.materus = {
      format = "json";
      sopsFile = ./users.json;
    };

    services.openssh.hostKeys = [
      {
        bits = 4096;
        path = "/materus/root/ssh_host_rsa_key";
        type = "rsa";
      }
      {
        path = "/materus/root/ssh_host_ed25519_key";
        type = "ed25519";
      }
    ];

}
