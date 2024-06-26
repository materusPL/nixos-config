{ materusCfg, ... }:
{
  imports =
    [

    ] ++ (if (materusCfg.materusFlake.decrypted) then [ ./private ] else [ ]);

  sops.age.generateKey = false;
  sops.gnupg.home = null;
  sops.gnupg.sshKeyPaths = [ ];
  sops.defaultSopsFile = materusCfg.hostPath + "/secrets/secrets.yaml";

  sops.secrets.wireguard = { };
  sops.secrets.discord-token = {};
  sops.secrets.spotify-client-id = {};
  sops.secrets.spotify-client-secret = {};
  sops.secrets.youtube-api = {};
  sops.secrets.certs = {};
  sops.secrets.steamladder-api = {};
  sops.secrets.webarchive-accesskey = {};
  sops.secrets.webarchive-secretkey = {};
  
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
