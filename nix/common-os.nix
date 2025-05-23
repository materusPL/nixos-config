# * Common OS
{
  lib,
  pkgs,
  mkkArg,
  config,
  konfig,
  ...
}:
{

  imports = [
    mkkArg.current.sops-nix.nixosModules.sops
# * Config
# ** System
# *** Fonts
    {
      options.mkk.os.fonts.enable = konfig.nixerusPkgs.lib.mkBoolOpt false "Enable MKK font settings for OS";

      config = lib.mkIf config.mkk.os.fonts.enable {

        fonts.packages = konfig.vars.packageLists.fonts;
        fonts.enableDefaultPackages = lib.mkDefault true;

        fonts.fontconfig.enable = lib.mkDefault true;
        fonts.fontconfig.cache32Bit = lib.mkDefault true;

        fonts.fontconfig.defaultFonts.sansSerif = [ "Noto Sans" "DejaVu Sans" "WenQuanYi Zen Hei" "Noto Color Emoji" ];
        fonts.fontconfig.defaultFonts.serif = [ "Noto Serif" "DejaVu Serif" "WenQuanYi Zen Hei" "Noto Color Emoji" ];
        fonts.fontconfig.defaultFonts.emoji = [ "Noto Color Emoji" "OpenMoji Color" ];
        fonts.fontconfig.defaultFonts.monospace = [ "Hack Nerd Font" "Noto Sans Mono" "WenQuanYi Zen Hei Mono" ];

        fonts.fontDir.enable = lib.mkDefault true;
  };
    }
# ** Shells
# *** Zsh
    {
      options.mkk.os.zsh.enable =
        konfig.nixerusPkgs.lib.mkBoolOpt true "Enable MKK system zsh config";
      config = lib.mkIf config.mkk.os.zsh.enable {
        users.defaultUserShell = pkgs.zsh;
        environment.shells = [ pkgs.zsh ];
        programs.zsh = {
          enable = true;
          enableGlobalCompInit = false;
          interactiveShellInit = ''
            if [[ ''${__MATERUS_HM_ZSH:-0} == 0 ]]; then
              source ${pkgs.grml-zsh-config}/etc/zsh/zshrc
            fi
          '';
          promptInit = '''';
        };
      };
    }
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
