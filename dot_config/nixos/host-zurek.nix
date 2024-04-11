{ config, pkgs, lib, ... }:

{
  imports = [];

  config = {
    networking.hostName = "zurek";

    boot.initrd.preLVMCommands = lib.mkOrder 400 "sleep 1";

    systemd.services.systemd-networkd-wait-online.enable = lib.mkForce false;
    virtualisation.podman.defaultNetwork.dnsname.enable = true;

    networking.firewall = {
      allowedTCPPorts = [ 22 53 80 81 443 9100 ];
      allowedUDPPorts = [ 53 ];
    };

    fileSystems."/" =
      {
        device = "/dev/mapper/vg-root";
        fsType = "ext4";
      };

    fileSystems."/var/storage" =
      {
        device = "/dev/mapper/chrupki";
        fsType = "ext4";
      };
    fileSystems."/var/rost" =
      {
        device = "/dev/mapper/rost";
        fsType = "ext4";
      };

    fileSystems."/boot" =
      { device = "/dev/disk/by-uuid/353B-B671";
        fsType = "vfat";
      };

    swapDevices =
      [ { device = "/dev/mapper/vg-swap"; }
      ];

    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  };
}
