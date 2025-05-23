{ pkgs, lib, ... }:
{
  #REGION test
  #sound.enable = true;
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
  hardware.pulseaudio.enable = false;
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

  services.udev = let
    script = pkgs.writeShellScript "arch-mknod" ''
      ACTION=$1
      KERNEL=$(basename $2)
      MAJOR=$3
      MINOR=$4


      if (systemctl is-active --quiet systemd-nspawn@archlinux); then
          if [[ $ACTION == "add" || "$ACTION" == "change"  ]]; then
              machinectl shell root@archlinux /bin/bash -c "
                  if ! [ -f /dev/$KERNEL ]; then
                    mknod /dev/$KERNEL c $MAJOR $MINOR
                    chmod 660 /dev/$KERNEL
                    chown root:input /dev/$KERNEL
                  fi
              "
          elif [[ $ACTION == "remove" ]]; then
              machinectl shell root@archlinux /bin/rm /dev/$KERNEL
          fi
      fi

    '';
  in {
    extraRules = ''
      KERNEL=="rtc0", GROUP="audio"
      KERNEL=="hpet", GROUP="audio"
      DEVPATH=="/devices/virtual/misc/cpu_dma_latency", OWNER="root", GROUP="audio", MODE="0660"

      
      SUBSYSTEM=="hidraw", KERNEL=="hidraw*", RUN+="${script} ''$env{ACTION} ''$env{DEVNAME} ''$env{MAJOR} ''$env{MINOR}"
    '';
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

}
