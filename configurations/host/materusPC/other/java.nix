{ pkgs, ... }:
{
  programs = {
    java.enable = true;
    java.package = pkgs.jdk;
    java.binfmt = true;
  };

  environment.variables = { 
    JAVA_8_HOME = "${pkgs.jdk8}/lib/openjdk/";
    JAVA_17_HOME = "${pkgs.jdk17}/lib/openjdk/";
    JAVA_21_HOME = "${pkgs.jdk21}/lib/openjdk/";
  };
}
