{ config, pkgs, lib, ... }:

{
  imports = [  ];
  config = {
  sound.enable = true;
  hardware.keyboard.zsa.enable = true;
  hardware.logitech.wireless.enable = true;
  hardware.logitech.wireless.enableGraphical = true;
  hardware.ledger.enable = true;

  services.pipewire.enable = true;
  hardware.pulseaudio.enable = true;
  programs.sway.enable = true;
  };
}
