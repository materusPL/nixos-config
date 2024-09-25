{ pkgs, ... }:
{
  programs = {
    java.enable = true;
    java.package = pkgs.jdk;
    java.binfmt = true;
  };

  environment.variables = { 
    JAVA_8_HOME = pkgs.jdk8;
    JAVA_17_HOME = pkgs.jdk17;
    JAVA_21_HOME = pkgs.jdk21;
  };
}
