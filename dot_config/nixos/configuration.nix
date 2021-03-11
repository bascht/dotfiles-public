{ config, pkgs, lib, ... }:

{
  imports =
    [
      <nixos-hardware/lenovo/thinkpad/t14s/amd>
      /etc/nixos/hardware-configuration.nix
    ];

  nixpkgs.config.allowUnfree = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.reusePassphrases = true;
  boot.kernelModules = [ "kvm-amd" "v4l2loopback" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.extraModprobeConfig = ''
    options v4l2loopback video_nr=8 card_label="Wayland" exclusive_caps=1
  '';

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "nodev";
    efiSupport = true;
    enableCryptodisk = true;
  };

  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking.useDHCP = false;
  networking.networkmanager.enable = true;
  networking.useNetworkd = true;
  networking.hostName = "apfelstrudel";
  networking.firewall.allowedTCPPorts = [ 22 ];

  systemd.services.NetworkManager-wait-online.enable = false;

  time.timeZone = "Europe/Berlin";

  programs.zsh.enable = true;
  programs.sway.enable = true;
  programs.light.enable = true;

  users.defaultUserShell = pkgs.zsh;
  users.extraUsers.bascht = {
    isNormalUser = true;
    extraGroups = [ "audio" "video" "networkmanager" "storage" "wheel" "disk" ];
  };

  security.doas.enable = true;
  security.doas.extraConfig = "permit nopass bascht as root cmd /run/current-system/sw/bin/openvpn";
  security.doas.extraRules = [{
    users = [ "bascht" ];
    keepEnv = true;
    persist = true;
  }];
  security.pam.loginLimits = [
    { domain = "@users"; item = "nofile"; type = "soft"; value = "4096"; }
  ];

  console = {
    packages = [ pkgs.terminus_font ];
    font = "${pkgs.terminus_font}/share/consolefonts/ter-v12n.psf.gz";
    colors = [
      "3B4252"
      "BF616A"
      "A3BE8C"
      "EBCB8B"
      "81A1C1"
      "B48EAD"
      "88C0D0"
      "E5E9F0"
      "4C566A"
      "BF616A"
      "A3BE8C"
      "EBCB8B"
      "81A1C1"
      "B48EAD"
      "8FBCBB"
      "ECEFF4"
    ];
    earlySetup = true;
    keyMap = "de-latin1-nodeadkeys";
  };

  services.devmon.enable = true;
  services.pipewire.enable = true;
  services.printing.drivers = [ pkgs.brgenml1cupswrapper ];
  services.printing.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization ];
  services.udisks2.enable = true;
  services.xserver.enable = false;
  services.xserver.libinput.enable = true;
  services.pcscd.enable = true;
  services.openssh.enable = true;
  services.upower.enable = true;

  sound.enable = true;

  hardware.pulseaudio.enable = true;
  hardware.enableAllFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  fonts = {
    enableDefaultFonts = true;
    fonts = with pkgs; [ font-awesome twitter-color-emoji jetbrains-mono fira fira-mono fira-code fantasque-sans-mono medio terminus_font ];
    fontconfig.enable = true;
    fontconfig.defaultFonts = {
      emoji = [ "Font Awesome 5 Free" "Noto Color Emoji" ];
      monospace = [ "JetBrains Mono" ];
      serif = [ "Fira Sans" ];
      sansSerif = [ "Fira Sans" ];
    };
  };

  environment.systemPackages = with pkgs; [
    wget
    curl
    vim
    git
    linuxPackages.v4l2loopback
    wireguard
    wireguard-tools
    openvpn
  ];

  virtualisation = {
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };
}
