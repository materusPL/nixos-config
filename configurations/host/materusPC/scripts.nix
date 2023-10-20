{ config, pkgs, lib, inputs, materusFlake, ... }:
let
valkyrie-sync = pkgs.writeShellScriptBin "valkyrie-sync" ''
${pkgs.rsync}/bin/rsync -avzrh --delete --exclude ".git*" --exclude "flake.lock" /materus/config/nixos-config materus@valkyrie:/materus/config/ && \
  ${pkgs.rsync}/bin/rsync -avzrh --delete --exclude ".git*" /materus/config/private/valkyrie materus@valkyrie:/materus/config/private
'';

valkyrie-flakelock = pkgs.writeShellScriptBin "valkyrie-flakelock" ''
${pkgs.openssh}/bin/ssh materus@valkyrie "nix flake update /materus/config/nixos-config --override-input nixpkgs github:NixOS/nixpkgs/23.05 \
  --override-input home-manager github:nix-community/home-manager/release-23.05 \
  --override-input private /materus/config/private/valkyrie/flake"
'';

valkyrie-rebuild-boot = pkgs.writeShellScriptBin "valkyrie-rebuild-boot" ''
${pkgs.openssh}/bin/ssh -t materus@valkyrie "sudo nixos-rebuild boot --flake /materus/config/nixos-config#valkyrie \
  --override-input private /materus/config/private/valkyrie/flake"
'';
valkyrie-rebuild-switch = pkgs.writeShellScriptBin "valkyrie-rebuild-switch" ''
${pkgs.openssh}/bin/ssh -t materus@valkyrie "sudo nixos-rebuild switch --flake /materus/config/nixos-config#valkyrie \
  --override-input private /materus/config/private/valkyrie/flake"
'';



flamaster-sync = pkgs.writeShellScriptBin "flamaster-sync" ''
${pkgs.rsync}/bin/rsync -avzrh --delete --exclude ".git*" --exclude "flake.lock" /materus/config/nixos-config materus@flamaster:/materus/config/ && \
  ${pkgs.rsync}/bin/rsync -avzrh --delete --exclude ".git*" /materus/config/private/flamaster materus@flamaster:/materus/config/private
'';

flamaster-flakelock = pkgs.writeShellScriptBin "flamaster-flakelock" ''
${pkgs.openssh}/bin/ssh materus@flamaster "nix flake update /materus/config/nixos-config --override-input nixpkgs github:NixOS/nixpkgs/23.05 \
  --override-input home-manager github:nix-community/home-manager/release-23.05 \
  --override-input private /materus/config/private/flamaster/flake"
'';

flamaster-rebuild-boot = pkgs.writeShellScriptBin "flamaster-rebuild-boot" ''
${pkgs.openssh}/bin/ssh -t materus@flamaster "sudo nixos-rebuild boot --flake /materus/config/nixos-config#flamaster \
  --override-input private /materus/config/private/flamaster/flake"
'';
flamaster-rebuild-switch = pkgs.writeShellScriptBin "flamaster-rebuild-switch" ''
${pkgs.openssh}/bin/ssh -t materus@flamaster "sudo nixos-rebuild switch --flake /materus/config/nixos-config#flamaster \
  --override-input private /materus/config/private/flamaster/flake"
'';


in
{ 
  environment.systemPackages = [
    valkyrie-rebuild-boot
    valkyrie-rebuild-switch
    valkyrie-sync
    valkyrie-flakelock

    flamaster-rebuild-boot
    flamaster-rebuild-switch
    flamaster-sync
    flamaster-flakelock
    ];
}
