{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
    boot.initrd.luks.reusePassphrases = true;
    boot.kernelModules = [ "kvm-amd" "v4l2loopback" ];
    boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = "1310720";
      "fs.inotify.max_user_instances" = "8192";
    };
    boot.loader.efi.canTouchEfiVariables = true;
    boot.loader.systemd-boot.enable = true;
    boot.initrd.luks.forceLuksSupportInInitrd = true;
  };
}
