{ config, pkgs, lib, materusCfg, ... }:
{
  imports =
    [
      (if (materusCfg.materusFlake.decrypted == true) then ./private else "")
    ];

    sops.age.generateKey = false;
    sops.gnupg.home = null;
    sops.gnupg.sshKeyPaths = [];
    sops.secrets."users/materus" = {
      format = "yaml";
      sopsFile = ./users.yaml;
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
