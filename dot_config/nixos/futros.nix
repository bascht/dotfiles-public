{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    boot.initrd.availableKernelModules = [ "r8169"  "nvme" "ehci_pci" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" "amdgpu" ];
    boot.initrd.kernelModules = [ "dm-snapshot" "r8169"];


  };
}
