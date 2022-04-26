
{ config, lib, pkgs, systemd, inputs, services, ... }:
let
  unstable = import <nixos-unstable> { config = { allowUnfree = true; }; };

in
{
  programs.home-manager.enable = true;

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    enableExtraSocket = true;
    extraConfig = ''
      allow-emacs-pinentry
      keep-display
      extra-socket /run/user/1000/gnupg/S.gpg-agent.extra
      no-grab
      allow-preset-passphrase
    '';
  };

  home.username = "bascht";
  home.homeDirectory = "/home/bascht";

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/759b1eb18a5b4afd9b286305032ba73b5bbc8103.tar.gz;
    }))
  ];

  home.sessionPath = [ "$HOME/.local/bin" "$HOME/bin" "$HOME/bin/go/bin" "$HOME/.cargo/bin" "$HOME/.emacs.d/bin" ];
  home.sessionVariables = {
    LC_TIME="en_GB.UTF-8";
    IBUS_SOCK="$XDG_RUNTIME_DIR/ibus.socket";
    XDG_CURRENT_DESKTOP="sway";
    XDG_SESSION_TYPE="wayland";
    MOZ_ENABLE_WAYLAND=1;
    MOZ_USE_XINPUT2=1;
    EDITOR="vim";
    XKB_DEFAULT_LAYOUT="de";
    XKB_DEFAULT_OPTIONS="caps:escape";
    QT_SCALE_FACTOR=1;
    QT_AUTO_SCREEN_SCALE_FACTOR=1;
    QT_WAYLAND_DISABLE_WINDOWDECORATION=1;
    _JAVA_AWT_WM_NONREPARENTING=1;
    GNOME_KEYRING_CONTROL="/run/user/1000/keyring";
    VAGRANT_DEFAULT_PROVIDER="libvirt";
    VAGRANT_DOTFILE_PATH=".vagrant-\${HOSTNAME}";
    QT_QPA_PLATFORMTHEME="qt5ct";
    SWAYSOCK="/run/user/1000/sway-ipc.sock";
    SDL_VIDEODRIVER="wayland";
    QT_QPA_PLATFORM="wayland-egl";
    #SSH_AUTH_SOCK="/run/user/1000/gnupg/S.gpg-agent.ssh";
    LF_ICONS="tw=:st=:ow=:dt=:di=:fi=:ln=:or=:ex=:*.c=:*.cc=:*.clj=:*.coffee=:*.cpp=:*.css=:*.go=:*.h=:*.hh=:*.hpp=:*.html=:*.js=:*.json=:*.md=:*.php=:*.py=:*.rb=:*.rs=:*.ts=:*.vim=:*.cmd=:*.ps1=:*.sh=:*.bash=:*.zsh=:*.fish=:*.tar=:*.tgz=:*.arc=:*.arj=:*.taz=:*.lha=:*.lz4=:*.lzh=:*.lzma=:*.tlz=:*.txz=:*.tzo=:*.t7z=:*.zip=:*.z=:*.dz=:*.gz=:*.lrz=:*.lz=:*.lzo=:*.xz=:*.zst=:*.tzst=:*.bz2=:*.bz=:*.tbz=:*.tbz2=:*.tz=:*.deb=:*.rpm=:*.jar=:*.war=:*.ear=:*.sar=:*.rar=:*.alz=:*.ace=:*.zoo=:*.cpio=:*.7z=:*.rz=:*.cab=:*.wim=:*.swm=:*.dwm=:*.esd=:*.jpg=:*.jpeg=:*.mjpg=:*.mjpeg=:*.gif=:*.bmp=:*.pbm=:*.pgm=:*.ppm=:*.tga=:*.xbm=:*.xpm=:*.tif=:*.tiff=:*.png=:*.svg=:*.svgz=:*.mng=:*.pcx=:*.mov=:*.mpg=:*.mpeg=:*.m2v=:*.mkv=:*.webm=:*.ogm=:*.mp4=:*.m4v=:*.mp4v=:*.vob=:*.qt=:*.nuv=:*.wmv=:*.asf=:*.rm=:*.rmvb=:*.flc=:*.avi=:*.fli=:*.flv=:*.gl=:*.dl=:*.xcf=:*.xwd=:*.yuv=:*.cgm=:*.emf=:*.ogv=:*.ogx=:*.aac=:*.au=:*.flac=:*.m4a=:*.mid=:*.midi=:*.mka=:*.mp3=:*.mpc=:*.ogg=:*.ra=:*.wav=:*.oga=:*.opus=:*.spx=:*.xspf=:*.pdf=:*.nix=:";
    FZF_DEFAULT_OPTS=" --prompt=' ' --border --height='40%' --layout=reverse --color=bg+:#f0f0f1,bg:#fafafa,spinner:#0184bc,hl:#4078f2 --color=fg:#696c77,header:#4078f2,info:#c18401,pointer:#0184bc --color=marker:#0184bc,fg+:#202227,prompt:#c18401,hl+:#4078f2";
  };

  home.packages = [
      # unstable.pkgs.logseq
      # unstable.pkgs.obsidian
      unstable.pkgs.darktable
      unstable.pkgs.foot
      unstable.pkgs.qutebrowser
      pkgs.any-nix-shell
      pkgs.socat
      pkgs.age
      pkgs.gnuplot
      pkgs.sqlite
      pkgs.inotify-tools
      pkgs.graphviz
      pkgs.liboping
      pkgs.libqrencode
      pkgs.pandoc
      pkgs.ghostscript
      pkgs.ansi2html
      pkgs.kind
      pkgs.liboping
      pkgs.tmux-cssh
      pkgs.dict
      pkgs.wordnet
      pkgs.dragon-drop
      pkgs.borgmatic
      pkgs.weechat
      pkgs.docker-compose
      pkgs.toilet
      pkgs.ydotool
      pkgs.viu
      pkgs.somafm-cli
      pkgs.tree
      pkgs.scrot
      unstable.pkgs.imv
      pkgs.cloc
      pkgs.httpie
      pkgs.yaml-language-server
      pkgs.k9s
      pkgs.multimarkdown
      pkgs.unzip
      pkgs.crun
      pkgs.go
      pkgs.gopls
      pkgs.libreoffice
      pkgs.awscli2
      pkgs.kristall
      pkgs.gitAndTools.tig
      pkgs.gitAndTools.lab
      pkgs.gimp
      pkgs.wally-cli
      pkgs.poppler_utils
      pkgs.python3Minimal
      pkgs.python38Packages.pdftotext
      pkgs.w3m
      pkgs.ruby
      pkgs.rubyPackages.net-scp
      pkgs.rubyPackages.net-ssh
      pkgs.rubyPackages.pry
      pkgs.xfce.tumbler
      pkgs.ffmpegthumbnailer
      pkgs.adwaita-qt
      pkgs.adementary-theme
      pkgs.alacritty
      pkgs.appimage-run
      pkgs.neovim
      pkgs.enchant
      pkgs.entr
      (unstable.pkgs.aspellWithDicts (dicts: with dicts; [de en en-computers en-science]))
      pkgs.bat
      pkgs.bemenu
      pkgs.obs-studio
      pkgs.binutils
      pkgs.borgbackup
      pkgs.brightnessctl
      pkgs.capitaine-cursors
      pkgs.chezmoi
      unstable.pkgs.chromium
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
      pkgs.glab
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
      pkgs.hunspellDicts.en_US
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
      pkgs.mako # notification daemon
      pkgs.material-design-icons
      pkgs.mpc_cli
      pkgs.mpv
      pkgs.ffmpeg-full
      pkgs.msmtp
      pkgs.mtr
      unstable.mu
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
      pkgs.mopidy
      pkgs.mopidy-iris
      pkgs.mopidy-local
      pkgs.mopidy-mpd
      pkgs.mopidy-mpris
      pkgs.mopidy-somafm
      pkgs.mopidy-spotify
      pkgs.rpm
      pkgs.qalculate-gtk
      pkgs.ripgrep
      pkgs.rocm-opencl-icd
      pkgs.rocm-opencl-runtime
      pkgs.rofi
      pkgs.rofi-calc
      pkgs.rpm
      unstable.pkgs.signal-desktop
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

 programs.direnv.enable = true;
 programs.direnv.nix-direnv.enable = true;
 programs.direnv.enableFishIntegration = true;
 programs.fish.package = unstable.pkgs.fish;
 programs.fish.enable = true;
 programs.fish.loginShellInit = ''
   if [ (/run/current-system/sw/bin/tty) = "/dev/tty1" ]
     exec sway-run
   end
 '';
 programs.fish.shellInit = ''
   set -U fish_greeting
   fzf_configure_bindings
   starship init fish | source
   any-nix-shell fish --info-right | source
 '';
   programs.fish.plugins = [
   {
    name = "z";
    src = pkgs.fetchFromGitHub {
      owner = "jethrokuan";
      repo = "z";
      rev = "45a9ff6d0932b0e9835cbeb60b9794ba706eef10";
      sha256 = "1kjyl4gx26q8175wcizvsm0jwhppd00rixdcr1p7gifw6s308sd5";
    };
  }
   {
    name = "fzf.fish";
    src = pkgs.fetchFromGitHub {
      owner = "PatrickF1";
      repo = "fzf.fish";
      rev = "17fcc74029bbd88445712752a5a71bc64aa3994c";
      sha256 = "12fyg3ycj3fqqms9b5ncnyyjs0gl54yc5qcbp5yp4p5fy5vwy6jr";
    };
  }
   {
    name = "autopair.fish";
    src = pkgs.fetchFromGitHub {
      owner = "jorgebucaran";
      repo = "autopair.fish";
      rev = "1222311994a0730e53d8e922a759eeda815fcb62";
      sha256 = "0lxfy17r087q1lhaz5rivnklb74ky448llniagkz8fy393d8k9cp";
    };
  }
  ];
 programs.vscode = {
     enable = true;
     package = pkgs.vscode;
     extensions = with pkgs.vscode-extensions; [
       vscodevim.vim
       redhat.vscode-yaml
       #bbenoist.Nix
     ];
     userSettings = {
         "workbench.colorTheme" = "Visual Studio Light";
         "terminal.integrated.fontFamily" = "JetBrains Mono";
         "editor.fontFamily" = "JetBrains Mono";
         "editor.renderControlCharacters" = true;
         "yaml.format.printWidth" = 250;
         "spellright.language" = [
           "de_DE"
           "en_GB"
           "en_US"
         ];
     };
 };

 systemd.user.services.comacs = {
       Unit = {
             Description = "Emacs Coding daemon";
	     X-RestartIfChanged = "false";
       };
       Service = {
             ExecStart = "${pkgs.emacsPgtkGcc}/bin/emacs --fg-daemon=comacs";
             Restart = "on-failure";
             SuccessExitStatus = 15;
             Type = "notify";
       };
       Install = {
             WantedBy = [ "graphical-session.target" ];
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
             ExecStart = "${pkgs.wl-clipboard}/bin/wl-paste -t text --watch clipman store --no-persist --max-items=200 --histpath=~/.local/share/clipman-primary.json";
             Restart = "on-failure";
             RestartSec = 5;
       };
       Install = {
             WantedBy = [ "graphical-session.target" ];
       };
 };

 systemd.user.services.swayidle = {
       Unit = {
             Description = "Idle Manager for Wayland";
             After = "graphical-session-pre.target";
             PartOf = "graphical-session.target";
       };
       Service = {
             ExecStart = '' ${pkgs.swayidle}/bin/swayidle -w -d \
               before-sleep '${config.home.homeDirectory}/bin/blur-lock' \
               timeout 300 '${config.home.homeDirectory}/bin/blur-lock' \
               timeout 600 '${pkgs.sway}/bin/swaymsg "output * dpms off"' \
               resume '${pkgs.sway}/bin/swaymsg "output * dpms on"'
             '';
             Restart = "on-failure";
             RestartSec = 5;
       };
       Install = {
             WantedBy = [ "graphical-session-pre.target" ];
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

  services.mbsync.enable = true;
  services.gnome-keyring.enable = true;
  services.nextcloud-client.enable = true;
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkGcc;
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
