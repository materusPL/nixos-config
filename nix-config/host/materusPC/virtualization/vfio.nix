{ config, pkgs, ... }:
let
  bar0_guest = "15";
  bar2_guest = "8";
  bar0_host = "15";
  bar2_host = "8";

  allCores = "0-31";
  allCoresMask = "ffffffff";
  hostCores = "0-7,16-23";
  hostCoresMask = "00ff00ff";
  vmCores = "8-15,24-31";
  vmCoresMask = "ff00ff00";

  VM_UUID = "ad2632db-0da0-4204-98b3-0592a185ebd0";

  startedHook = ''
    # Renice QEMU process and threads

    QEMU_PID=$(ps aux | grep qemu-system-x86_64 | grep "${VM_UUID}" | tr -s ' ' | cut -d " " -f 2)
    for pid in $(ls /proc/$QEMU_PID/task); do 
      renice -n "-15" -p "$pid";
    done
    renice -n "-10" -p "$QEMU_PID";
  '';
  startHook =
    /*
      ''

      # Debugging
        exec 19>/home/materus/startlogfile
        BASH_XTRACEFD=19
        set -x

        exec 3>&1 4>&2
        trap 'exec 2>&4 1>&3' 0 1 2 3
        exec 1>/home/materus/startlogfile.out 2>&1
      ''
      +
    */
    ''
      # Service for my shared qcow2 drive, it's mounted to host when VM not running
      systemctl stop windows-share-mount.service

      # Stop arch container, script doesnt kill things in container so gpu will be in broken state without it
      if [ $(systemctl is-active systemd-nspawn@archlinux) = "active" ]; then
        systemctl stop systemd-nspawn@archlinux; 
        sleep 5s;
        while [ $(systemctl is-active systemd-nspawn@archlinux) = "active" ];do sleep 2s; done;
      fi

      # Remember non symlink path to card and render, symlink might get deleted
      DRI_RENDER=$(readlink -f /dev/dri/by-path/pci-$VIRSH_GPU_VIDEO-render)
      DRI_CARD=$(readlink -f /dev/dri/by-path/pci-$VIRSH_GPU_VIDEO-card)

      # Send "remove" event so wayland compositors can release gpu, sleep because it doesnt work instantly
      echo remove > /sys/bus/pci/devices/$VIRSH_GPU_VIDEO/drm/card*/uevent
      sleep 3s

      # Remove all permissions from DRI nodes so no new processes will attach to it, kill all processes currently using it 
      chmod 0 $DRI_RENDER 
      chmod 0 $DRI_CARD
      fuser -k $DRI_RENDER
      fuser -k $DRI_CARD

      # Seems to fix reset bug for 7900 XTX
      echo "0" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/d3cold_allowed"

      # Unbind GPU from drivers
      echo ''$VIRSH_GPU_VIDEO > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/driver/unbind"
      echo ''$VIRSH_GPU_AUDIO > "/sys/bus/pci/devices/''${VIRSH_GPU_AUDIO}/driver/unbind"

      # Optionally resize bars, it's pointless for me since it's full size here but keeping just in case
      echo "${bar0_guest}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource0_resize"
      echo "${bar2_guest}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource2_resize"

      # Compact memory if possible to make continuous space for transparent huge pages
      sync
      echo "3" > /proc/sys/vm/drop_caches
      sync
      echo "1" > /proc/sys/vm/compact_memory



      # Set host cgroups and workqueue to use defined cpu cores (I'm using first half of cpu on host, second half on guest)
      systemctl set-property --runtime -- user.slice AllowedCPUs=${hostCores}
      systemctl set-property --runtime -- system.slice AllowedCPUs=${hostCores}
      systemctl set-property --runtime -- init.scope AllowedCPUs=${hostCores}
      echo "${hostCoresMask}" > /sys/bus/workqueue/devices/writeback/cpumask

      # Set performance governor if not set
      echo performance | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

      # Reduce interval of memory statistics to 120s from default 1s
      sysctl vm.stat_interval=120





    '';
  stopHook = ''

    # Debugging
    #  exec 19>/home/materus/stoplogfile
    #  BASH_XTRACEFD=19
    #  set -x

    #  exec 3>&1 4>&2
    #  trap 'exec 2>&4 1>&3' 0 1 2 3
    #  exec 1>/home/materus/stoplogfile.out 2>&1


    #  echo performance | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

    # Stop arch container, sometimes gpu doesnt return properly if it's active
    if [ $(systemctl is-active systemd-nspawn@archlinux) = "active" ]; then
        systemctl stop systemd-nspawn@archlinux; 
        sleep 5s;
        while [ $(systemctl is-active systemd-nspawn@archlinux) = "active" ]; do sleep 2s; done;
    fi

    sysctl vm.stat_interval=1
    
        
    sleep 1s
    echo ''$VIRSH_GPU_VIDEO > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/driver/unbind"
    echo ''$VIRSH_GPU_AUDIO > "/sys/bus/pci/devices/''${VIRSH_GPU_AUDIO}/driver/unbind"

        


    echo "${bar0_host}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource0_resize"
    echo "${bar2_host}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource2_resize"

    echo "1" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/d3cold_allowed"


    echo ''$VIRSH_GPU_VIDEO > /sys/bus/pci/drivers/amdgpu/bind
    echo ''$VIRSH_GPU_AUDIO > /sys/bus/pci/drivers/snd_hda_intel/bind


    systemctl start windows-share-mount.service

    systemctl set-property --runtime -- user.slice AllowedCPUs=${allCores}
    systemctl set-property --runtime -- system.slice AllowedCPUs=${allCores}
    systemctl set-property --runtime -- init.scope AllowedCPUs=${allCores}
    echo "${allCoresMask}" > /sys/bus/workqueue/devices/writeback/cpumask

  '';
in
{
  services.udev.extraRules = ''
    SUBSYSTEM=="kvmfr", OWNER="root", GROUP="kvm", MODE="0660"
  '';
  virtualisation.libvirtd.hooks.qemu = {
    "windows-vfio" = pkgs.writeShellScript "windows.sh" ''
      VIRSH_GPU_VIDEO="0000:03:00.0"
      VIRSH_GPU_AUDIO="0000:03:00.1"
      VIRSH_USB1="0000:10:00.0"

      if [ ''$1 = "windows-vfio" ]; then
        if [ ''$2 = "prepare" ] && [ ''$3 = "begin" ]; then
          ${startHook}
        fi

        #if [ ''$2 = "started" ] && [ ''$3 = "begin" ]; then
          ${startedHook}
        #fi

        if [ ''$2 = "release" ] && [ ''$3 = "end" ]; then
          ${stopHook}
        fi

      fi    
    '';
  };
}
