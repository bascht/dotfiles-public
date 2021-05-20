
{ config, lib, pkgs, systemd, inputs, services, ... }:
let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

in
{
  programs.home-manager.enable = true;

  home.username = "bascht";
  home.homeDirectory = "/home/bascht";

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/90d2409.tar.gz;
    }))
  ];

  home.packages = [
      unstable.borgmatic
      pkgs.toilet
      pkgs.ydotool
      pkgs.somafm-cli
      pkgs.tree
      pkgs.imv
      pkgs.cloc
      pkgs.httpie
      pkgs.yaml-language-server
      pkgs.k9s
      pkgs.openssl
      pkgs.unzip
      pkgs.crun
      pkgs.go
      pkgs.libreoffice
      pkgs.aws
      pkgs.kristall
      pkgs.gitAndTools.tig
      pkgs.gimp
      pkgs.wally-cli
      pkgs.poppler_utils
      pkgs.python38Packages.pdftotext
      pkgs.w3m
      pkgs.xfce.tumbler
      pkgs.ffmpegthumbnailer
      pkgs.tridactyl-native
      pkgs.adwaita-qt
      pkgs.adementary-theme
      pkgs.alacritty
      pkgs.appimage-run
      pkgs.neovim
      pkgs.aspell
      pkgs.aspellDicts.de
      pkgs.aspellDicts.de
      pkgs.aspellDicts.en
      pkgs.aspellDicts.en-computers
      pkgs.bat
      pkgs.bemenu
      pkgs.obs-studio
      pkgs.obs-v4l2sink
      pkgs.bibata-cursors
      pkgs.binutils
      pkgs.borgbackup
      pkgs.brightnessctl
      pkgs.capitaine-cursors
      pkgs.chezmoi
      pkgs.chromium
      pkgs.clipman
      pkgs.cmake
      pkgs.dmenu
      pkgs.dnsutils
      pkgs.editorconfig-core-c
      pkgs.exa
      pkgs.fd
      pkgs.feh
      pkgs.firefox-wayland
      pkgs.font-awesome-ttf
      pkgs.fzf
      pkgs.gammastep
      pkgs.gcc
      pkgs.glib
      pkgs.gnome-icon-theme
      pkgs.gnome-themes-extra
      pkgs.gnome-themes-standard
      pkgs.gnome-themes-standard
      pkgs.gnome3.adwaita-icon-theme
      pkgs.gnome3.defaultIconTheme
      pkgs.gnome3.defaultIconTheme
      pkgs.gnome3.file-roller
      pkgs.gnome3.gcr
      pkgs.gnome3.gnome-keyring
      pkgs.gnome3.gnome-themes-standard
      pkgs.gnome3.gnome-themes-standard
      pkgs.gnome3.libgnome-keyring
      pkgs.gnome3.seahorse
      pkgs.gnome3.zenity
      pkgs.gnumake
      pkgs.libreoffice
      pkgs.hledger-web
      pkgs.hledger
      pkgs.gnupg
      pkgs.gnupg-pkcs11-scd
      pkgs.go
      pkgs.gopass
      pkgs.gore
      pkgs.gotests
      pkgs.gomodifytags
      pkgs.grim
      pkgs.gsettings-desktop-schemas
      pkgs.gst_all_1.gst-plugins-bad
      pkgs.gst_all_1.gst-plugins-good
      pkgs.gst_all_1.gst-plugins-ugly
      pkgs.gthumb
      pkgs.gtk-engine-murrine
      pkgs.gtk_engines
      pkgs.hicolor-icon-theme
      pkgs.hicolor-icon-theme
      pkgs.hunspell
      pkgs.hunspellDicts.de_DE
      pkgs.hunspellDicts.de_DE
      pkgs.hunspellDicts.en_GB-large
      pkgs.i3status-rust
      pkgs.imagemagick
      pkgs.ispell
      pkgs.isync
      pkgs.j4-dmenu-desktop
      pkgs.jq
      pkgs.kanshi
      pkgs.ledger
      pkgs.libnotify
      pkgs.libqalculate
      pkgs.libtool
      pkgs.libvterm
      pkgs.lsof
      pkgs.lxappearance
      pkgs.lxappearance
      pkgs.mako # notification daemon
      pkgs.material-design-icons
      pkgs.mpc_cli
      pkgs.mpv
      pkgs.ffmpeg-full
      pkgs.msmtp
      pkgs.mtr
      pkgs.mu
      pkgs.ncmpcpp
      pkgs.ncspot
      pkgs.nixfmt
      pkgs.nextcloud-client
      pkgs.patchelf
      pkgs.pavucontrol
      pkgs.pcmanfm
      pkgs.pinentry-gtk2
      pkgs.playerctl
      pkgs.pngquant
      pkgs.podman
      pkgs.pwgen
      pkgs.gammastep
      pkgs.mc
      pkgs.rpm
      pkgs.qalculate-gtk
      pkgs.ripgrep
      pkgs.rocm-opencl-icd
      pkgs.rocm-opencl-runtime
      pkgs.rofi
      pkgs.rofi-calc
      pkgs.rpm
      pkgs.ruby
      pkgs.rubyPackages.net-scp
      pkgs.rubyPackages.net-ssh
      pkgs.rubyPackages.pry
      pkgs.signal-desktop
      pkgs.silver-searcher
      pkgs.slurp
      pkgs.sway
      pkgs.starship
      pkgs.swayidle
      pkgs.swaylock
      pkgs.wayvnc
      pkgs.tdns-cli
      pkgs.tmux
      pkgs.tmuxinator
      pkgs.upower
      pkgs.weather-icons
      pkgs.wf-recorder
      pkgs.wl-clipboard
      pkgs.python38Packages.youtube-dl
      pkgs.python38Packages.pip
      pkgs.python38Packages.setuptools
      pkgs.wofi
      pkgs.wol
      pkgs.xfce.thunar
      pkgs.xfce.thunar
      pkgs.xfce.thunar-volman
      pkgs.xwayland
      pkgs.yubikey-manager
      pkgs.yubikey-personalization
      pkgs.zathura
      pkgs.spotifyd
      pkgs.spotify-tui
  ];

 programs.vscode = {
     enable = true;
     package = pkgs.vscode;
     extensions = with pkgs.vscode-extensions; [
       vscodevim.vim
       redhat.vscode-yaml
       bbenoist.Nix
     ];
     userSettings = {
         "workbench.colorTheme" = "Visual Studio Light";
         "terminal.integrated.fontFamily" = "JetBrains Mono";
         "editor.renderControlCharacters" = true;
     };
 };
 systemd.user.services.mako = {
       Unit = {
             Description = "Mako notifications";
             After = "graphical-session-pre.target";
             PartOf = "graphical-session.target";
       };
       Service = {
             ExecStart = "${pkgs.mako}/bin/mako";
             Restart = "on-failure";
             RestartSec = 5;
       };
       Install = {
             WantedBy = [ "graphical-session.target" ];
       };
 };

 systemd.user.services.clipboard-manager = {
       Unit = {
             Description = "Clipboard manager";
             After = "graphical-session-pre.target";
             PartOf = "graphical-session.target";
       };
       Service = {
             ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste -t text --watch clipman store --max-items=200 --histpath=~/.local/share/clipman-primary.json";
             Restart = "on-failure";
             RestartSec = 5;
       };
       Install = {
             WantedBy = [ "graphical-session.target" ];
       };
 };

  services.gammastep = {
        enable = true;
        latitude = "48.15";
        longitude = "11.64";
        temperature = {
              day= 6500;
              night = 3000;
        };
  };

  services.nextcloud-client.enable = true;
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
