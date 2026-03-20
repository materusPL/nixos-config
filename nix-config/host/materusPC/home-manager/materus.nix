{ pkgs, materusArgs, config, ... }:
{
  mkk.neovim.enable = true;

  mkk.dir = config.lib.file.mkOutOfStoreSymlink "/mkk/config";
  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user.email = "materus@podkos.pl";
      user.name = "materus";
      commit.gpgsign = true;
      gpg.format = "ssh";
    };

    signing.signByDefault = true;
    signing.key = "/mkk/config/extra-files/ssh/materus.pub";
  };
  home.packages = with pkgs; [
    materusArgs.inputs.nixerus.packages.x86_64-linux.polymc
    neovide

    curl
    wget
    python3

    packwiz
    ani-cli
    mpv

    libreoffice-qt6-fresh

  ];
  xdg.dataFile."java-runtimes/graalvm-oracle-17".source = pkgs.graalvmPackages.graalvm-oracle_17;
  xdg.dataFile."java-runtimes/graalvm-oracle-latest".source = pkgs.graalvmPackages.graalvm-oracle;
  xdg.dataFile."java-runtimes/openjdk21".source = pkgs.jdk21;
}
