{ config, pkgs, materusArg, ... }:
let
  bar0_guest="15";
  bar2_guest="8";
  bar0_host="15";
  bar2_host="8";

  VM_UUID = "ad2632db-0da0-4204-98b3-0592a185ebd0";

  startedHook = ''
    # Renice QEMU process and threads

    QEMU_PID=$(ps aux | grep qemu-system-x86_64 | grep "${VM_UUID}" | tr -s ' ' | cut -d " " -f 2)
    for pid in $(ls /proc/$QEMU_PID/task); do 
      renice -n "-15" -p "$pid";
    done
    renice -n "-10" -p "$QEMU_PID";
  '';
  startHook = /*''


    # Debugging
      exec 19>/home/materus/startlogfile
      BASH_XTRACEFD=19
      set -x

      exec 3>&1 4>&2
      trap 'exec 2>&4 1>&3' 0 1 2 3
      exec 1>/home/materus/startlogfile.out 2>&1
    ''
    +*/
    ''
      # Service for my shared qcow2 drive, it's mounted to host when VM not running
      systemctl stop windows-share-mount.service
      systemctl stop systemd-nspawn@archlinux
      
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
      systemctl set-property --runtime -- user.slice AllowedCPUs=${materusArg.materusPC.hostCores}
      systemctl set-property --runtime -- system.slice AllowedCPUs=${materusArg.materusPC.hostCores}
      systemctl set-property --runtime -- init.scope AllowedCPUs=${materusArg.materusPC.hostCores}
      echo "${materusArg.materusPC.hostCoresMask}" > /sys/bus/workqueue/devices/writeback/cpumask

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

    systemctl set-property --runtime -- user.slice AllowedCPUs=${materusArg.materusPC.allCores}
    systemctl set-property --runtime -- system.slice AllowedCPUs=${materusArg.materusPC.allCores}
    systemctl set-property --runtime -- init.scope AllowedCPUs=${materusArg.materusPC.allCores}
    echo "${materusArg.materusPC.allCoresMask}" > /sys/bus/workqueue/devices/writeback/cpumask

  '';
in
{







  virtualisation.libvirtd.qemu.verbatimConfig = ''
  cgroup_device_acl = [
    "/dev/null", "/dev/full", "/dev/zero",
    "/dev/random", "/dev/urandom",
    "/dev/ptmx", "/dev/kvm", "/dev/kqemu",
    "/dev/rtc","/dev/hpet", "/dev/vfio/vfio",
    "/dev/kvmfr0"
  ]
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

  systemd.services.windows-share-mount = {
    wantedBy = [ "multi-user.target" ];
    path = [ config.virtualisation.libvirtd.qemu.package pkgs.util-linux pkgs.kmod pkgs.coreutils ];
    serviceConfig.Type = "oneshot";
    serviceConfig.RemainAfterExit = true;
    script = ''
      modprobe nbd max_part=16
      sleep 1
      qemu-nbd -c /dev/nbd10 /materus/data/VM/data.qcow2 --discard=unmap
      sleep 1
      mount /dev/nbd10p1 /materus/data/Windows -o uid=1000,gid=100
    '';
    preStop = ''
      umount -r /dev/nbd10p1
      qemu-nbd -d /dev/nbd10
    '';
  };
}
