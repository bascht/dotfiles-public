{ config, pkgs, lib, ... }:

{
  imports =
    [
      <nixos-hardware/lenovo/thinkpad/t14s/amd>
      /etc/nixos/hardware-configuration.nix
      /etc/nixos/machine.nix
      /etc/nixos/wifi.nix
    ];

  system.stateVersion = "22.05";
  nix.autoOptimiseStore = true;
  nixpkgs.config.allowUnfree = true;

  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.reusePassphrases = true;
  boot.kernelModules = [ "kvm-amd" "v4l2loopback" ];
  boot.extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback ];
  boot.extraModprobeConfig = ''
    options v4l2loopback devices=2 video_nr=8,9 card_label="Wayland 8,Wayland 9" exclusive_caps=1
  '';

  boot.kernel.sysctl = {
      "fs.inotify.max_user_watches" = "1310720";
      "fs.inotify.max_user_instances" = "8192";
  };
  boot.loader.systemd-boot.enable = false;
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "nodev";
    efiSupport = true;
    enableCryptodisk = true;
  };

  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  networking = {
    useDHCP = false;
    networkmanager.enable = false;
    useNetworkd = true;
    firewall.allowedTCPPorts = [ 22 ];
    wireless = {
      enable = true;
      userControlled.enable = true;
    };
  };

  systemd.network = {
    enable = true;
    wait-online.anyInterface = true;

    networks = {
      "100-ethernet" = {
        matchConfig.Name = "ethernet";
        DHCP = "yes";
        networkConfig = {
          IPv6AcceptRA = true;
          LinkLocalAddressing = "yes";
        };
        dhcpV4Config = {
          UseDomains = true;
        };
        dhcpConfig = {
          UseDNS = true;
        };
      };

      "100-dorhamm-docking" = {
        matchConfig.Name = "dorhamm-docking";
        DHCP = "yes";
        networkConfig = {
          IPv6AcceptRA = true;
          LinkLocalAddressing = "yes";
        };
        dhcpV4Config = {
          UseDomains = true;
        };
        dhcpConfig = {
          UseDNS = true;
        };
      };

      "100-wifi" = {
        matchConfig.Name = "wifi";
        DHCP = "yes";
        networkConfig = {
          IPv6AcceptRA = true;
          LinkLocalAddressing = "yes";
        };
        dhcpV4Config = {
          UseDomains = true;
          RouteMetric = 1200;
        };
        dhcpConfig = {
          UseDNS = true;
        };
      };
    };
  };

  time.timeZone = "Europe/Berlin";

  programs.zsh.enable = true;
  programs.sway.enable = true;
  programs.light.enable = true;
  programs.fish.enable = true;

  users.defaultUserShell = pkgs.fish;
  users.extraUsers.bascht = {
    isNormalUser = true;
    shell = pkgs.fish;
    extraGroups = [ "audio" "video" "networkmanager" "systemd-network" "storage" "wheel" "disk" "plugdev" "docker" ];
  };

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

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

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
  services.fwupd.enable = true;
  services.fstrim.enable = true;

  sound.enable = true;
  hardware.keyboard.zsa.enable = true;
  hardware.logitech.wireless.enable = true;
  hardware.logitech.wireless.enableGraphical = true;
  hardware.ledger.enable = true;

  hardware.pulseaudio.enable = true;
  hardware.enableAllFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

  services.udev.extraRules = ''
    ACTION=="remove", ATTRS{idVendor}=="3297", RUN+="${pkgs.su}/bin/su bascht --shell ${pkgs.bash}/bin/bash /home/bascht/bin/thinkpad-dock", OWNER="bascht"
    ACTION=="add",    ATTRS{idVendor}=="3297", RUN+="${pkgs.su}/bin/su bascht --shell ${pkgs.bash}/bin/bash /home/bascht/bin/thinkpad-dock", OWNER="bascht"
  '' + builtins.readFile /etc/nixos/machine-udev-network.rules;

  nixpkgs.config.joypixels.acceptLicense = true;

  fonts = {
    enableDefaultFonts = false;
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
      font-awesome
      twitter-color-emoji
      jetbrains-mono
      fira
      mononoki
      fira-mono
      fira-code
      fantasque-sans-mono
      medio
      terminus_font
      ibm-plex
      iosevka
      roboto
      roboto-mono
      joypixels
      (iosevka-bin.override { variant = "aile"; })
      (iosevka-bin.override { variant = "etoile"; })
      (nerdfonts.override { fonts = [ "JetBrainsMono" "IBMPlexMono" "Iosevka" ]; }) ];
    fontconfig.enable = true;
    fontconfig.defaultFonts = {
      emoji = lib.mkBefore [ "Joypixels" "Noto Color Emoji" ];
      monospace = lib.mkBefore [ "IBM Plex Mono" "JetBrains Mono" ];
      serif = lib.mkBefore [ "Iosevka Etoile" ];
      sansSerif = lib.mkBefore [ "Iosevka Aile" ];
    };
  };

  nix.trustedUsers = [ "root" "bascht" ];

  environment.systemPackages = with pkgs; [
    home-manager
    wget
    curl
    vim
    git
    cachix
    linuxPackages.v4l2loopback
    wireguard-tools
    openvpn
    openssl
    ledger-udev-rules
  ];

  virtualisation = {
    docker.enable = true;
    podman = {
      enable = true;
      dockerCompat = false;
    };
  };
}
