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
# * NIX & NIXPKGS
    {
      nixpkgs.config = {
        allowUnfree = lib.mkDefault true;
        joypixels.acceptLicense = lib.mkDefault true;
        nvidia.acceptLicense = lib.mkDefault true;
      };

      nix.package = lib.mkDefault pkgs.nixVersions.nix_2_28;
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
# * Assertions
    {
      config.assertions = [
        {
          assertion = builtins ? currentSystem;
          message = "MKK must be build with --impure";
        }

        {
          assertion = config.konfig.decrypted;
          message = "Need to decrypt MKK repo to build";
        }
      ];
    }
# * Args

    (
      let
        decryptedBool =
          let
            file = ../private/check-encryption;
            resultFile = pkgs.runCommandLocal "check-encryption" { src = file; } ''
              if [[ "$(< $src)" != "DECRYPTED" ]]; then
                 echo -n "no" >> $out;
              else
                 echo -n "yes" >> $out;
              fi 
            '';
          in
          (builtins.readFile resultFile == "yes");

      in
      {
        options.konfig = lib.mkOption { default = { }; };
        config = {
          konfig = {
            decrypted = decryptedBool;
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
            vars = lib.mkDefault { };
          };
          _module.args.konfig = config.konfig;
        };
      }
    )
# * common.nix END
  ];

}
