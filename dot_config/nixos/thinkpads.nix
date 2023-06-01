{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
  sound.enable = true;
  hardware.keyboard.zsa.enable = true;
  hardware.logitech.wireless.enable = true;
  hardware.logitech.wireless.enableGraphical = true;
  hardware.ledger.enable = true;

  programs.sway.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
  };

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
  };
}
