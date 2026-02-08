{ pkgs, lib, ... }:
{
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    audio.enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    systemWide = true;
    jack.enable = true;
  };

  environment.sessionVariables =
    let
      makePluginPath =
        format:
        "$HOME/.${format}:"
        + (lib.makeSearchPath format [
          "$HOME/.nix-profile/lib"
          "/run/current-system/sw/lib"
          "/etc/profiles/per-user/$USER/lib"
        ]);
    in
    {
      ALSOFT_DRIVERS = "pulse";

      DSSI_PATH = makePluginPath "dssi";
      LADSPA_PATH = makePluginPath "ladspa";
      LV2_PATH = makePluginPath "lv2";
      LXVST_PATH = makePluginPath "lxvst";
      VST_PATH = makePluginPath "vst";
      VST3_PATH = makePluginPath "vst3";
    };

  environment.systemPackages = with pkgs; [
    openal
    pulseaudio

    reaper

    yabridge
    yabridgectl

    vital
    odin2
    surge
    fire
    decent-sampler
    lsp-plugins
  ];

  services.udev.extraRules = ''
    KERNEL=="rtc0", GROUP="audio"
    KERNEL=="hpet", GROUP="audio"
    DEVPATH=="/devices/virtual/misc/cpu_dma_latency", OWNER="root", GROUP="audio", MODE="0660"
  '';

}
