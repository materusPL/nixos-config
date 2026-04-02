{
  description = "Materus hosts and user config";
  #region Inputs
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-25.11";
    };

    nixpkgs-unstable = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-unstable";
    };

    materus-inputs = {
      url = "path:./nix-config/subflake";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.home-manager.url = "github:nix-community/home-manager/release-25.11";
    };
    materus-inputs-unstable = {
      url = "path:./nix-config/subflake";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

  };
  #endregion
  outputs =
    inputs@{
      self,
      nixpkgs,
      nixpkgs-unstable,
      materus-inputs,
      materus-inputs-unstable,
    }:
    let
      #region Variables
      path = builtins.toString ./.;
      files = {
        ssh-keys = {
          materus = ./extra-files/ssh/materus.pub;
        };
        patches = {
          bwrap = ./extra-files/patches/bubblewrap.patch;
        };
      };
      #endregion
      #region System make helper
      makeSystem =
        {
          host,
          arch ? "x86_64-linux",
          stable ? true,
          extraArgs ? { },
        }:
        let
          inp = if stable then materus-inputs else materus-inputs-unstable;
          sys-nixpkgs = if stable then nixpkgs else nixpkgs-unstable;
          materusArgs = {
            inherit files;
            inherit arch;
            inputs = inp;
            flake-path = path;
            host-path = path + "/nix-config/host/${host}";
          };
        in
        (sys-nixpkgs.lib.nixosSystem {
          system = arch;
          specialArgs = extraArgs // {
            inherit materusArgs;
          };
          modules = [
            ./nix-config/host/${host}
            (import ./nix-config/shared false)
          ];

        });
      #endregion
      #region Home make helper
      makeHome =
        {
          user,
          arch ? "x86_64-linux",
          stable ? true,
          extraArgs ? { },
          host ? null,
        }:
        let
          inp = if stable then materus-inputs else materus-inputs-unstable;
          home-nixpkgs = if stable then nixpkgs else nixpkgs-unstable;
          pkgs = import home-nixpkgs {
            system = arch;
            config.allowUnfree = true;
            overlays = [
              inp.nix-vscode-extensions.overlays.default 
            ];
          };
          materusArgs = {
            inherit files;
            inherit arch;
            inputs = inp;
            flake-path = path;
            home-path = path + "/nix-config/home/${user}";
          };
        in
        inp.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules =
            let
              host-path = ./. + "/nix-config/host/${host}/home-manager/${user}.nix";
            in
            [
              ./nix-config/home/${user}
              (import ./nix-config/shared true)
            ]
            ++ (if (host != null && builtins.pathExists host-path) then [ host-path ] else [ ]);
          extraSpecialArgs = extraArgs // {
            inherit materusArgs;
          };
        };
      #endregion
    in
    rec {
      nixosConfigurations = {
        materusPC = makeSystem {
          host = "materusPC";
          stable = true;
        };
      };

      homeConfigurations = {
        "materus@materusPC" = makeHome {
          user = "materus";
          host = "materusPC";
          stable = true;
        };
      };

    };

}
