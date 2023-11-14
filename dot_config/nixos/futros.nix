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

    networking.firewall.extraCommands = "iptables -A nixos-fw -p tcp --source 10.11.12.20/32 --dport 9100:9100 -j nixos-fw-accept";
    networking.firewall.extraStopCommands = "iptables -D nixos-fw -p tcp --source 10.11.12.20/32 --dport 9100:9100 -j nixos-fw-accept || true";

    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  };
}
