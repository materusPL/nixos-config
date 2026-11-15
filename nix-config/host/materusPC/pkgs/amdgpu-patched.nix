{ pkgs
, lib
, kernel ? pkgs.linuxPackages_latest.kernel
}:

pkgs.stdenv.mkDerivation {
  pname = "amdgpu-kernel-module";
  inherit (kernel) src version postPatch nativeBuildInputs;

  kernel_dev = kernel.dev;
  kernelVersion = kernel.modDirVersion;

  patches = [
    (pkgs.fetchpatch {
      name = "cap_sys_nice_begone.patch";
      url = "https://github.com/Frogging-Family/community-patches/raw/master/linux61-tkg/cap_sys_nice_begone.mypatch";
      hash = "sha256-Y3a0+x2xvHsfLax/uwycdJf3xLxvVfkfDVqjkxNaYEo=";
    })
  ];

  modulePath = "drivers/gpu/drm/amd/amdgpu";

  buildPhase = ''
    BUILT_KERNEL=$kernel_dev/lib/modules/$kernelVersion/build

    cp $BUILT_KERNEL/Module.symvers .
    cp $BUILT_KERNEL/.config        .
    cp $kernel_dev/vmlinux          .

    make "-j$NIX_BUILD_CORES" modules_prepare
    make "-j$NIX_BUILD_CORES" M=$modulePath modules
  '';

  installPhase = ''
    make \
      INSTALL_MOD_PATH="$out" \
      XZ="xz -T$NIX_BUILD_CORES" \
      M="$modulePath" \
      modules_install
  '';

  meta = {
    description = "AMD GPU kernel module";
    license = lib.licenses.gpl3;
  };
}