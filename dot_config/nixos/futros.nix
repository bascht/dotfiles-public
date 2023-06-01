{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    boot.initrd.availableKernelModules = [ "r8169"  "nvme" "ehci_pci" "xhci_pci" "usbhid" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" "amdgpu" ];
    boot.initrd.kernelModules = [ "dm-snapshot" "r8169"];
    boot.kernelModules = [ "kvm-intel" "r8169" ];

    services.prometheus = {
      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9100;
        };
      };
    };

    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  };
}
