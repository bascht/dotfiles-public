{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    networking.hostName = "pierogi";

    boot.initrd.luks.devices = {
      root = {
        device = "/dev/disk/by-uuid/00b9f4c9-396a-4078-9d99-c00e3a0070be";
        preLVM = true;
        allowDiscards = true;
      };
    };

    boot.initrd.secrets = {
      "keyfile0.bin" = "/etc/secrets/initrd/keyfile0.bin";
    };

    swapDevices = [ ];

    fileSystems."/" =
      { device = "/dev/disk/by-uuid/ea6426b8-42c7-425b-865c-65b456802859";
        fsType = "ext4";
      };

    fileSystems."/home" =
      { device = "/dev/mapper/vg0-home";
        fsType = "ext4";
      };

    fileSystems."/boot" =
      { device = "/dev/disk/by-uuid/A97D-AB98";
        fsType = "vfat";
      };
  };
}
