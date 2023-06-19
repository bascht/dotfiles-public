{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    networking.hostName = "apfelstrudel";
    virtualisation.libvirtd.enable = true;


#    boot.initrd.luks.devices.crypted.device = "/dev/disk/by-uuid/22d00e62-3409-4f07-a400-8e5de944f56a";
    boot.initrd.luks.devices = {
      root = {
        device = "/dev/disk/by-uuid/22d00e62-3409-4f07-a400-8e5de944f56a";
        preLVM = true;
        keyFile = "/keyfile0.bin";
        allowDiscards = true;
      };

    };
    boot.initrd.secrets = {
      "keyfile0.bin" = "/etc/secrets/initrd/keyfile0.bin";
    };

    swapDevices = [ ];

    fileSystems."/" =
      { device = "/dev/disk/by-uuid/d7be6605-64c6-4089-bae1-3389cf3306b5";
        fsType = "ext4";
      };

    fileSystems."/home" =
      { device = "/dev/disk/by-label/home";
        fsType = "ext4";
      };

    fileSystems."/boot" =
      { device = "/dev/disk/by-uuid/B11D-4A56";
        fsType = "vfat";
      };
  };
}
