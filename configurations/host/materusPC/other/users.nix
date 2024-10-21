{ pkgs, materusArg, config, ... }:
{

users.users.materus = {
    isNormalUser = true;
    extraGroups = [
      "audio"
      "video"
      "render"
      "pipewire"
      "wheel"
      "networkmanager"
      "input"
      "kvm"
      "libvirt-qemu"
      "libvirt"
      "libvirtd"
      "podman"
      "scanner"
      "lp"
    ];
    shell = pkgs.zsh;
    description = "Mateusz Słodkowicz";
    openssh.authorizedKeys.keyFiles = [ ("${materusArg.cfg.path}" + "/extraFiles/keys/ssh/materus.pub") ];
    hashedPasswordFile = config.sops.secrets."users/materus".path;
  };
  users.users.tester = {
    isNormalUser = true;
    home = "/tmp/home/tester";
    hashedPasswordFile = config.sops.secrets."users/materus".path;
  };
}