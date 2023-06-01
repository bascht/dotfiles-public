{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    networking.hostName = "flaki";
    boot.kernelModules = [ "kvm-intel" "r8169" ];

    boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "usb_storage" "usbhid" "sd_mod" ];
    boot.initrd.kernelModules = [ "dm-snapshot" ];
    boot.initrd.luks.devices.crypted.device = "/dev/disk/by-uuid/1786c83d-16de-4a83-bedc-dd2606b6eccc";


    services.scanberry.enable = true;
    hardware.sane.enable = true;
    hardware.sane.drivers.scanSnap.enable = true;

    services.prometheus = {
      exporters = {
        node = {
          enable = true;
          enabledCollectors = [ "systemd" ];
          port = 9100;
        };
      };
    };
    services.nginx = {
      enable = true;
      virtualHosts."scanberry.dorhamm.me" =  {
        locations."/" = {
          proxyPass = "http://127.0.0.1:3000";
          proxyWebsockets = true;
        };
      };
    };
    networking = {
      firewall.allowedTCPPorts = [ 22 80 9100 ];
    };
    fileSystems."/" =
      {
        device = "/dev/mapper/vg-root";
        # device = "/dev/disk/by-uuid/2f6792bd-35d6-497d-83f5-bf315e055ebe";
        fsType = "ext4";
      };

    fileSystems."/boot" =
      { device = "/dev/disk/by-uuid/3874-FE08";
        fsType = "vfat";
      };

    swapDevices =
      [ { device = "/dev/disk/by-uuid/636e053e-e829-4f28-8bb7-f45a4343f76d"; }
      ];

    hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
}
