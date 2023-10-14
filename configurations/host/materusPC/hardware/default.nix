{ config, pkgs, lib, inputs, materusFlake, materusPkgs, ... }:
{
  imports =
    [
      ./filesystem.nix
      ./boot.nix

    ];
  hardware.firmware = with pkgs; [
    #materusPkgs.amdgpu-pro-libs.firmware.vcn
    #materusPkgs.amdgpu-pro-libs.firmware
    linux-firmware
    alsa-firmware
    sof-firmware
  ];

  environment.variables = {
    DISABLE_LAYER_AMD_SWITCHABLE_GRAPHICS_1 = "1";
    VK_ICD_FILENAMES = "${pkgs.mesa.drivers}/share/vulkan/icd.d/radeon_icd.x86_64.json:${pkgs.driversi686Linux.mesa.drivers}/share/vulkan/icd.d/radeon_icd.i686.json";
    AMD_VULKAN_ICD = "RADV";
    RADV_PERFTEST = "gpl,rt,sam";
  };
  hardware.cpu.amd.updateMicrocode = lib.mkForce true;

  #extra
  hardware.wooting.enable = true;
  hardware.bluetooth.enable = true;


  #Graphics
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = with pkgs; [
    vaapiVdpau
    libvdpau-va-gl
    amdvlk
    rocmPackages.clr.icd
    rocmPackages.clr
    materusPkgs.amdgpu-pro-libs.vulkan
    materusPkgs.amdgpu-pro-libs.amf
  ];
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [
    vaapiVdpau
    pkgs.driversi686Linux.amdvlk
    materusPkgs.i686Linux.amdgpu-pro-libs.vulkan
    libvdpau-va-gl
  ];
  services.udev.extraRules = ''

    #GPU bar size
    ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x1002", ATTR{device}=="0x744c", ATTR{resource0_resize}="15"
    ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x1002", ATTR{device}=="0x744c", ATTR{resource2_resize}="8"
  '';


  #Trim
  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

}
