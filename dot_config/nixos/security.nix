{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {

  security.doas.enable = true;

  security.doas.extraRules = [{
      users = [ "bascht" ];
      keepEnv = true;
      persist = true;
    }{
      users = [ "bascht" ];
      keepEnv = true;
      noPass = true;
      cmd = "/run/current-system/sw/bin/openvpn";
    }{
    }{
      users = [ "bascht" ];
      keepEnv = true;
      noPass = true;
      cmd = "/run/current-system/sw/bin/cryptsetup";
    }{
      users = [ "bascht" ];
      noPass = true;
      cmd = "/run/current-system/sw/bin/systemctl";
      args = ["suspend"];
    }{
      users = [ "bascht" ];
      noPass = true;
      cmd = "/run/current-system/sw/bin/networkctl";
    }{
      users = [ "bascht" ];
      noPass = true;
      cmd = "/run/current-system/sw/bin/cryptsetup";
      args = ["luksOpen" "/dev/disk/by-partlabel/Gulasch" "Gulasch"];
    }{
      users = [ "bascht" ];
      noPass = true;
      cmd = "/run/wrappers/bin/mount";
      args = ["/dev/mapper/Gulasch" "/mnt/Gulasch"];
  }];
  security.pam.loginLimits = [
    { domain = "@users"; item = "nofile"; type = "soft"; value = "8192"; }
  ];

  };
}
