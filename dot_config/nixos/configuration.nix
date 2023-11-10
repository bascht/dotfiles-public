{ config, pkgs, lib, inputs, ... }:

{
  imports = [];

  system.stateVersion = "23.05";
  nix.settings.auto-optimise-store = true;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  nix.settings.trusted-users = [ "root" "bascht" ];

  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };

  nixpkgs.config.allowUnfree = true;

  networking = {
    useDHCP = true;
    networkmanager.enable = false;
    useNetworkd = true;
    firewall.allowedTCPPorts = [ 22 ];
    wireless = {
      enable = true;
      userControlled.enable = true;
    };
  };

  time.timeZone = "Europe/Berlin";

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = {
      LC_TIME     = "en_GB.UTF-8";
      LC_MONETARY = "de_DE.UTF-8";
      LC_PAPER    = "de_DE.UTF-8";
      LC_NUMERIC  = "de_DE.UTF-8";
    };
  };

  programs.light.enable = true;
  programs.fish.enable = true;

  users.defaultUserShell = pkgs.bash;
  users.extraUsers.bascht = {
    isNormalUser = true;
    shell = pkgs.bash;
    extraGroups = [ "audio" "video" "networkmanager" "systemd-network" "storage" "wheel" "disk" "plugdev" "docker"  "scanner" "lp" ];
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
  services.printing.drivers = [ pkgs.brgenml1cupswrapper ];
  services.printing.enable = true;
  services.udev.packages = [ pkgs.yubikey-personalization pkgs.platformio ];
  services.udisks2.enable = true;
  services.xserver.enable = false;
  services.xserver.libinput.enable = true;
  services.pcscd.enable = true;
  services.openssh.enable = true;
  services.upower.enable = true;
  services.fwupd.enable = true;
  services.fstrim.enable = true;

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  hardware.nitrokey.enable = true;
  hardware.enableAllFirmware = true;
  hardware.opengl = {
    enable = true;
    driSupport = true;
  };

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
      overpass
      ibm-plex
      iosevka
      martian-mono
      roboto
      roboto-mono
      joypixels
      (iosevka-bin.override { variant = "aile"; })
      (iosevka-bin.override { variant = "etoile"; })
      (nerdfonts.override { fonts = [ "JetBrainsMono" "IBMPlexMono" "Iosevka"  "NerdFontsSymbolsOnly" "VictorMono" ]; }) ];
    fontconfig.enable = true;
    fontconfig.defaultFonts = {
      emoji = lib.mkBefore [ "Joypixels" "Noto Color Emoji" ];
      monospace = lib.mkBefore [ "IBM Plex Mono" "JetBrains Mono" ];
      serif = lib.mkBefore [ "Iosevka Etoile" ];
      sansSerif = lib.mkBefore [ "Iosevka Aile" ];
    };
  };

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
    docker.daemon.settings = {
      dns = ["1.1.1.1" "8.8.4.4"];
    };
    podman = {
      enable = true;
      dockerCompat = false;
    };
  };
}
