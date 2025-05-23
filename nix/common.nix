# * common.nix
{
  pkgs,
  lib,
  mkkArg,
  config,
  ...
}:
{
  imports = [
# * Config
# ** NIX & NIXPKGS
    {
      nixpkgs.config = {
        allowUnfree = lib.mkDefault true;
        joypixels.acceptLicense = lib.mkDefault true;
        nvidia.acceptLicense = lib.mkDefault true;
      };

      nix.package = lib.mkDefault pkgs.nixVersions.latest;
      nix.settings = {
        experimental-features = [
          "nix-command"
          "flakes"
          "no-url-literals"
        ];

        auto-optimise-store = true;
        trusted-users = [
          "root"
          "@wheel"
        ];

        substituters = [
          "https://nix-community.cachix.org"
          "https://cache.nixos.org/"
          "https://nixerus.cachix.org/"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "nixerus.cachix.org-1:2x7sIG7y1vAoxc8BNRJwsfapZsiX4hIl4aTi9V5ZDdE="
        ];
      };

      nix.registry = {
        nixpkgs-stable = {
          from = {
            type = "indirect";
            id = "nixpkgs-stable";
          };
          flake = mkkArg.stable.nixpkgs;
        };
        nixpkgs-unstable = {
          from = {
            type = "indirect";
            id = "nixpkgs-unstable";
          };
          flake = mkkArg.unstable.nixpkgs;
        };

        nixpkgs = {
          from = {
            type = "indirect";
            id = "nixpkgs";
          };
          flake = mkkArg.current.nixpkgs;
        };

        emacs-overlay = {
          from = {
            type = "indirect";
            id = "emacs-overlay";
          };
          flake = mkkArg.current.emacs-overlay;
        };

        flake-utils = {
          from = {
            type = "indirect";
            id = "flake-utils";
          };
          flake = mkkArg.current.flake-utils;
        };

        nixos-hardware = {
          from = {
            type = "indirect";
            id = "nixos-hardware";
          };
          flake = mkkArg.current.nixos-hardware;
        };

        nixerus = {
          from = {
            type = "indirect";
            id = "nixerus";
          };
          flake = mkkArg.current.nixerus;
        };

        devshell = {
          from = {
            type = "indirect";
            id = "devshell";
          };
          flake = mkkArg.current.devshell;
        };

        home-manager = {
          from = {
            type = "indirect";
            id = "home-manager";
          };
          flake = mkkArg.current.home-manager;
        };

        sops-nix = {
          from = {
            type = "indirect";
            id = "sops-nix";
          };
          flake = mkkArg.current.sops-nix;
        };

        base16 = {
          from = {
            type = "indirect";
            id = "base16";
          };
          flake = mkkArg.current.base16;
        };

        git-agecrypt = {
          from = {
            type = "indirect";
            id = "git-agecrypt";
          };
          flake = mkkArg.current.git-agecrypt;
        };

        plasma-manager = {
          from = {
            type = "indirect";
            id = "plasma-manager";
          };
          flake = mkkArg.current.plasma-manager;
        };

        nur = {
          from = {
            type = "indirect";
            id = "nur";
          };
          flake = mkkArg.current.nur;
        };

        nix-vscode-extensions = {
          from = {
            type = "indirect";
            id = "nix-vscode-extensions";
          };
          flake = mkkArg.current.nix-vscode-extensions;
        };

        impermanence = {
          from = {
            type = "indirect";
            id = "impermanence";
          };
          flake = mkkArg.current.impermanence;
        };

      };
    }
# ** Variables
    {
      mkk.commonVariables = {
        packageLists = rec {
          defaultFonts = [
            pkgs.dejavu_fonts
            pkgs.freefont_ttf
            pkgs.gyre-fonts
            pkgs.liberation_ttf
            pkgs.unifont
          ];
          fonts = [
            pkgs.noto-fonts
            pkgs.noto-fonts-extra
            pkgs.noto-fonts-emoji
            pkgs.noto-fonts-cjk-sans
            pkgs.noto-fonts-cjk-serif
            
            pkgs.wqy_zenhei
            
            pkgs.corefonts
            pkgs.hack-font
            pkgs.ubuntu_font_family
            pkgs.monocraft

            pkgs.nerd-fonts.hack
            pkgs.nerd-fonts.meslo-lg
            pkgs.nerd-fonts.droid-sans-mono
            pkgs.nerd-fonts.profont
            pkgs.nerd-fonts.fira-code
            
          ] ++ defaultFonts;
          
        };
      };
    }
# ** Assertions
    {
      config.assertions = [
        {
          assertion = builtins ? currentSystem;
          message = "MKK must be build with --impure";
        }

        {
          assertion = mkkArg.isDecrypted;
          message = "Need to decrypt MKK repo to build";
        }
      ];
    }
# ** Args
    {
      imports = [
        (if mkkArg.isDecrypted then ./variables-private.nix else {})
        (if mkkArg.isOs then ./common-os.nix else {})
      ];
      options.konfig = lib.mkOption { default = { }; };
      options.mkk.commonVariables = lib.mkOption { default = { }; };
      
      config = {
        konfig = {
          unstable = mkkArg.unstable;
          stable = mkkArg.stable;
          current = mkkArg.current;
          nixerusPkgs =
            (import mkkArg.current.nixerus { inherit pkgs; })
            // (
              if (pkgs.system == "x86_64-linux") then
                {
                  i686Linux = import mkkArg.current.nixerus { pkgs = pkgs.pkgsi686Linux; };
                }
              else
                { }
            );

          arg = mkkArg;
          rootFlake = (builtins.getFlake mkkArg.configRootPath);
          vars = config.mkk.commonVariables;
        };
        _module.args.konfig = config.konfig;
      };
    }

# * common.nix END
  ];

}
