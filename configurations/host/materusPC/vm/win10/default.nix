{ config, pkgs, materusArg, ... }:
let
  bar0_guest="15";
  bar2_guest="8";
  bar0_host="15";
  bar2_host="8";

  VM_UUID = "ad2632db-0da0-4204-98b3-0592a185ebd0";

  startedHook = ''
    QEMU_PID=$(ps aux | grep qemu-system-x86_64 | grep "${VM_UUID}" | tr -s ' ' | cut -d " " -f 2)

    for pid in $(cat /sys/fs/cgroup/cpu/machine.slice/machine-qemu*$1.scope/libvirt/vcpu*/tasks); do 
      renice -n "-15" -p "$pid";
    done
    renice -n "-10" -p "$QEMU_PID";

    echo "${materusArg.materusPC.hostCoresMask}" > /proc/irq/default_smp_affinity
    for irq in /proc/irq/[0-9]*/smp_affinity; do 
      if [ $(cat $irq) = "${materusArg.materusPC.allCoresMask}" ]; then
        echo "${materusArg.materusPC.hostCoresMask}" > $irq 2> /dev/null 
      fi;
    done;
    for irq in $(cat /proc/interrupts | grep vfio | cut -d ":" -f 1); do 
      echo "${materusArg.materusPC.vmCoresMask}" > /proc/irq/$irq/smp_affinity; 
    done


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
      systemctl stop windows-share-mount.service

      # Make sure nothing renders on gpu to prevent "sysfs: cannot create duplicate filename" after rebinding to amdgpu
      chmod 0 /dev/dri/renderD128 
      fuser -k /dev/dri/renderD128

      # Seems to fix reset bug for 7900 XTX
      echo "0" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/d3cold_allowed"

      #####################################################################
      # Weird bug on kernel 6.7+, after changing bar sizes and binding to vfio driver, performance after returning to host will be lower than expected
      # binding to amdgpu after changing bar sizes and binding after it to vfio will work as expected.
      # I could skip changing bar sizes since I'm able to use full bar, but keeping it just in case
      echo ''$VIRSH_GPU_VIDEO > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/driver/unbind"
      sleep 1s
      echo "${bar0_host}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource0_resize"
      echo "${bar2_host}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource2_resize"

      echo ''$VIRSH_GPU_VIDEO > /sys/bus/pci/drivers/amdgpu/bind
      
      sleep 1s

      chmod 0 /dev/dri/renderD128 
      fuser -k /dev/dri/renderD128
      #####################################################################
      
      echo ''$VIRSH_GPU_VIDEO > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/driver/unbind"
      echo ''$VIRSH_GPU_AUDIO > "/sys/bus/pci/devices/''${VIRSH_GPU_AUDIO}/driver/unbind"

      echo "${bar0_guest}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource0_resize"
      echo "${bar2_guest}" > "/sys/bus/pci/devices/''${VIRSH_GPU_VIDEO}/resource2_resize"

      sync
      echo "3" > /proc/sys/vm/drop_caches
      sync
      echo "1" > /proc/sys/vm/compact_memory
      

      

      systemctl set-property --runtime -- user.slice AllowedCPUs=${materusArg.materusPC.hostCores}
      systemctl set-property --runtime -- system.slice AllowedCPUs=${materusArg.materusPC.hostCores}
      systemctl set-property --runtime -- init.scope AllowedCPUs=${materusArg.materusPC.hostCores}
      echo "${materusArg.materusPC.hostCoresMask}" > /sys/bus/workqueue/devices/writeback/cpumask
      echo performance | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

      sysctl vm.stat_interval=120
      sysctl -w kernel.watchdog=0


      


    '';
  stopHook = ''

    # Debugging
    #  exec 19>/home/materus/stoplogfile
    #  BASH_XTRACEFD=19
    #  set -x

    #  exec 3>&1 4>&2
    #  trap 'exec 2>&4 1>&3' 0 1 2 3
    #  exec 1>/home/materus/stoplogfile.out 2>&1
    echo performance | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

    sysctl vm.stat_interval=1
    sysctl -w kernel.watchdog=1
    echo "${materusArg.materusPC.allCoresMask}" > /proc/irq/default_smp_affinity
    for irq in /proc/irq/[0-9]*/smp_affinity; do 
      if [ $(cat $irq) = "${materusArg.materusPC.hostCoresMask}" ] || [ $(cat $irq) = "${materusArg.materusPC.vmCoresMask}" ]; then
        echo "${materusArg.materusPC.allCoresMask}" > $irq 2> /dev/null 
      fi;
    done;

        
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








  virtualisation.libvirtd.hooks.qemu = {
    "win10" = pkgs.writeShellScript "win10.sh" ''
      VIRSH_GPU_VIDEO="0000:03:00.0"
      VIRSH_GPU_AUDIO="0000:03:00.1"
      VIRSH_USB1="0000:10:00.0"

      if [ ''$1 = "win10" ] || [ ''$1 = "win11" ]; then
        if [ ''$2 = "prepare" ] && [ ''$3 = "begin" ]; then
          ${startHook}
        fi

        if [ ''$2 = "started" ] && [ ''$3 = "begin" ]; then
          ${startedHook}
        fi

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

      losetup -P /dev/loop6 /materus/data/VM/data.raw
      mount /dev/loop6p1 /materus/data/Windows -o uid=1000,gid=100
    '';
    preStop = ''
      umount -lf /materus/data/Windows
      losetup -d /dev/loop6
    '';
  };
}
