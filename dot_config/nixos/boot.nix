{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    boot.initrd.luks.reusePassphrases = true;
    boot.initrd.availableKernelModules = [ "nvme" "ehci_pci" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" "amdgpu" ];
    boot.initrd.kernelModules = [ "dm-snapshot" "amdgpu" ];
    boot.kernelModules = [ "kvm-amd" "v4l2loopback" ];
    boot.extraModulePackages = with config.boot.kernelPackages
                               ; [ v4l2loopback ];
    boot.extraModprobeConfig = ''
    options v4l2loopback devices=2 video_nr=8,9 card_label="Wayland 8,Wayland 9" exclusive_caps=1
  '';

    boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = "1310720";
      "fs.inotify.max_user_instances" = "8192";
    };
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.systemd-boot.enable = true;
    boot.initrd.luks.forceLuksSupportInInitrd = true;
  };
}
