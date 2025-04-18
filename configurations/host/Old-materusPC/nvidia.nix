{ config, pkgs, ... }:
{
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    modesetting.enable = true;
    nvidiaSettings = true;
    open = true;

  };
  hardware.opengl.extraPackages = with pkgs; [
    vaapiVdpau
    nvidia-vaapi-driver
    libvdpau-va-gl
  ];

  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [
    vaapiVdpau
    nvidia-vaapi-driver
    libvdpau-va-gl
  ];
  materus.profile.steam.extraEnv = {
    VK_ICD_FILENAMES=''''$VK_ICD_FILENAMES:/run/opengl-driver/share/vulkan/icd.d/nvidia_icd.x86_64.json:/run/opengl-driver-32/share/vulkan/icd.d/nvidia_icd.i686.json'';
    __NV_PRIME_RENDER_OFFLOAD="1";
    __NV_PRIME_RENDER_OFFLOAD_PROVIDER="NVIDIA-G0";
    __GLX_VENDOR_LIBRARY_NAME="nvidia";
    __VK_LAYER_NV_optimus="NVIDIA_only";
  };
}
