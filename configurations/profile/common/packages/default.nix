{
  config,
  pkgs,
  lib,
  materusArg,
  ...
}:
with materusArg.pkgs.lib;
{
  imports = [ ./fonts.nix ];

  #Single Packages
  options.materus.profile.packages.home-manager =
    mkPrivateVar
      materusArg.cfg.configInputs.home-manager.packages.${pkgs.system}.home-manager;
  options.materus.profile.packages.firefox = mkPrivateVar (
    pkgs.firefox.override { nativeMessagingHosts = [ pkgs.plasma-browser-integration ]; }
  );

  #Package Lists
  options.materus.profile.packages.list.nixRelated = mkPrivateVar (
    with pkgs;
    [
      nix-prefetch
      nix-prefetch-scripts
      nix-prefetch-github
      nix-prefetch-docker
      nixfmt-rfc-style
      nix-top
      nix-tree
      nix-diff
      nix-ld
      nixpkgs-fmt
      nixpkgs-review
    ]
  );

  options.materus.profile.packages.list.desktopApps = mkPrivateVar (
    with pkgs;
    [
      (discord.override {
        nss = nss_latest;
        withOpenASAR = true;
        withTTS = true;
      })
      tdesktop
      syncplay
      ani-cli
      nextcloud-client
      spotify
      thunderbird
      keepassxc
      (aspellWithDicts (
        ds: with ds; [
          en
          en-computers
          en-science
          pl
        ]
      ))
      onlyoffice-bin
      qalculate-qt
    ]
  );

  options.materus.profile.packages.list.terminalApps = mkPrivateVar (
    with pkgs;
    [
      neofetch
      ripgrep
      fd
      micro
    ]
  );

}
