{ config, lib, pkgs, systemd, inputs, services, ... }:
let
  # Via https://nixos.wiki/wiki/Sway
  dbus-sway-environment = pkgs.writeTextFile {
    name = "dbus-sway-environment";
    destination = "/bin/dbus-sway-environment";
    executable = true;

    text = ''
      dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway
      systemctl --user stop pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
      systemctl --user start pipewire pipewire-media-session xdg-desktop-portal xdg-desktop-portal-wlr
    '';
  };
  #
  # currently, there is some friction between sway and gtk:
  # https://github.com/swaywm/sway/wiki/GTK-3-settings-on-Wayland
  # the suggested way to set gtk settings is with gsettings
  # for gsettings to work, we need to tell it where the schemas are
  # using the XDG_DATA_DIR environment variable
  # run at the end of sway config
  configure-gtk = pkgs.writeTextFile {
    name = "configure-gtk";
    destination = "/bin/configure-gtk";
    executable = true;
    text = let
      schema = pkgs.gsettings-desktop-schemas;
      datadir = "${schema}/share/gsettings-schemas/${schema.name}";
    in ''
      export XDG_DATA_DIRS=${datadir}:$XDG_DATA_DIRS
      gnome_schema=org.gnome.desktop.interface
      gsettings set $gnome_schema gtk-theme 'Dracula'
    '';
  };
in {
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

  home.sessionPath = [
    "$HOME/.local/bin"
    "$HOME/bin"
    "$HOME/bin/go/bin"
    "$HOME/.cargo/bin"
    "$HOME/.emacs.d/bin"
  ];
  home.sessionVariables = {
    LC_TIME = "en_GB.UTF-8";
    IBUS_SOCK = "$XDG_RUNTIME_DIR/ibus.socket";
    XDG_CURRENT_DESKTOP = "sway";
    XDG_SESSION_TYPE = "wayland";
    MOZ_ENABLE_WAYLAND = 1;
    MOZ_USE_XINPUT2 = 1;
    EDITOR = "vim";
    XKB_DEFAULT_LAYOUT = "de";
    XKB_DEFAULT_OPTIONS = "caps:escape";
    QT_SCALE_FACTOR = 1;
    QT_AUTO_SCREEN_SCALE_FACTOR = 1;
    QT_WAYLAND_DISABLE_WINDOWDECORATION = 1;
    _JAVA_AWT_WM_NONREPARENTING = 1;
    GNOME_KEYRING_CONTROL = "/run/user/1000/keyring";
    VAGRANT_DEFAULT_PROVIDER = "libvirt";
    VAGRANT_DOTFILE_PATH = ".vagrant-\${HOSTNAME}";
    QT_QPA_PLATFORMTHEME = "qt5ct";
    SWAYSOCK = "/run/user/1000/sway-ipc.sock";
    SDL_VIDEODRIVER = "wayland";
    QT_QPA_PLATFORM = "wayland-egl";
    #SSH_AUTH_SOCK="/run/user/1000/gnupg/S.gpg-agent.ssh";
    LF_ICONS =
      "tw=:st=:ow=:dt=:di=:fi=:ln=:or=:ex=:*.c=:*.cc=:*.clj=:*.coffee=:*.cpp=:*.css=:*.go=:*.h=:*.hh=:*.hpp=:*.html=:*.js=:*.json=:*.md=:*.php=:*.py=:*.rb=:*.rs=:*.ts=:*.vim=:*.cmd=:*.ps1=:*.sh=:*.bash=:*.zsh=:*.fish=:*.tar=:*.tgz=:*.arc=:*.arj=:*.taz=:*.lha=:*.lz4=:*.lzh=:*.lzma=:*.tlz=:*.txz=:*.tzo=:*.t7z=:*.zip=:*.z=:*.dz=:*.gz=:*.lrz=:*.lz=:*.lzo=:*.xz=:*.zst=:*.tzst=:*.bz2=:*.bz=:*.tbz=:*.tbz2=:*.tz=:*.deb=:*.rpm=:*.jar=:*.war=:*.ear=:*.sar=:*.rar=:*.alz=:*.ace=:*.zoo=:*.cpio=:*.7z=:*.rz=:*.cab=:*.wim=:*.swm=:*.dwm=:*.esd=:*.jpg=:*.jpeg=:*.mjpg=:*.mjpeg=:*.gif=:*.bmp=:*.pbm=:*.pgm=:*.ppm=:*.tga=:*.xbm=:*.xpm=:*.tif=:*.tiff=:*.png=:*.svg=:*.svgz=:*.mng=:*.pcx=:*.mov=:*.mpg=:*.mpeg=:*.m2v=:*.mkv=:*.webm=:*.ogm=:*.mp4=:*.m4v=:*.mp4v=:*.vob=:*.qt=:*.nuv=:*.wmv=:*.asf=:*.rm=:*.rmvb=:*.flc=:*.avi=:*.fli=:*.flv=:*.gl=:*.dl=:*.xcf=:*.xwd=:*.yuv=:*.cgm=:*.emf=:*.ogv=:*.ogx=:*.aac=:*.au=:*.flac=:*.m4a=:*.mid=:*.midi=:*.mka=:*.mp3=:*.mpc=:*.ogg=:*.ra=:*.wav=:*.oga=:*.opus=:*.spx=:*.xspf=:*.pdf=:*.nix=:";
    FZF_DEFAULT_OPTS =
      " --prompt=' ' --border --height='40%' --layout=reverse --color=bg+:#f0f0f1,bg:#fafafa,spinner:#0184bc,hl:#4078f2 --color=fg:#696c77,header:#4078f2,info:#c18401,pointer:#0184bc --color=marker:#0184bc,fg+:#202227,prompt:#c18401,hl+:#4078f2";
  };

  home.packages = [
    dbus-sway-environment
    configure-gtk
    (pkgs.aspellWithDicts
      (dicts: with dicts; [ de en en-computers en-science ]))
    pkgs.adementary-theme
    pkgs.adwaita-qt
    pkgs.age
    pkgs.ansi2html
    pkgs.any-nix-shell
    pkgs.appimage-run
    pkgs.awscli2
    pkgs.bat
    pkgs.bfs
    pkgs.binutils
    pkgs.borgbackup
    pkgs.borgmatic
    pkgs.brightnessctl
    pkgs.btop
    pkgs.calcurse
    pkgs.capitaine-cursors
    pkgs.chezmoi
    pkgs.chromium
    pkgs.clipman
    pkgs.cloc
    pkgs.cmake
    pkgs.delta
    pkgs.dict
    pkgs.dive
    pkgs.dmenu
    pkgs.dnsutils
    pkgs.docker-compose
    pkgs.dockfmt
    pkgs.cljfmt
    pkgs.clj-kondo
    pkgs.editorconfig-core-c
    pkgs.enchant
    pkgs.entr
    pkgs.envsubst
    pkgs.eza
    pkgs.fd
    pkgs.feh
    pkgs.ffmpeg-full
    pkgs.ffmpegthumbnailer
    pkgs.firefox-wayland
    pkgs.font-awesome
    pkgs.foot
    pkgs.freecad
    pkgs.fzf
    pkgs.gcc
    pkgs.ghostscript
    pkgs.gimp
    pkgs.gitAndTools.lab
    pkgs.gitAndTools.tig
    pkgs.glab
    pkgs.glib
    pkgs.gnome-icon-theme
    pkgs.gnome-themes-extra
    pkgs.gnome3.adwaita-icon-theme
    pkgs.gnome3.file-roller
    pkgs.gnome3.gnome-keyring
    pkgs.gnome3.libgnome-keyring
    pkgs.gnome3.seahorse
    pkgs.gnome3.zenity
    pkgs.gnumake
    pkgs.gnupg
    pkgs.gnupg-pkcs11-scd
    pkgs.gnuplot
    pkgs.go
    pkgs.godef
    pkgs.gomodifytags
    pkgs.gopass
    pkgs.gopls
    pkgs.gore
    pkgs.gotests
    pkgs.graphviz
    pkgs.grim
    pkgs.grpcurl
    pkgs.gsettings-desktop-schemas
    pkgs.gst_all_1.gst-plugins-bad
    pkgs.gst_all_1.gst-plugins-good
    pkgs.gst_all_1.gst-plugins-ugly
    pkgs.gst_all_1.gst-vaapi
    pkgs.gthumb
    pkgs.gtk-engine-murrine
    pkgs.gtk_engines
    pkgs.hicolor-icon-theme
    pkgs.hledger
    pkgs.hledger-web
    pkgs.httpie
    pkgs.hunspell
    pkgs.hunspellDicts.de_DE
    pkgs.hunspellDicts.en_GB-large
    pkgs.hunspellDicts.en_US
    pkgs.i3status-rust
    pkgs.imagemagick
    pkgs.imv
    pkgs.inotify-tools
    pkgs.ispell
    pkgs.isync
    pkgs.j4-dmenu-desktop
    pkgs.jless
    pkgs.jq
    pkgs.k9s
    pkgs.kanshi
    pkgs.khal
    pkgs.kind
    pkgs.ledger
    pkgs.libnotify
    pkgs.liboping
    pkgs.libqalculate
    pkgs.libreoffice
    pkgs.libtool
    pkgs.libvterm
    pkgs.lsof
    pkgs.lxappearance
    pkgs.mako
    pkgs.material-design-icons
    pkgs.mediainfo
    pkgs.mesa
    pkgs.mesa_drivers
    pkgs.mpc_cli
    pkgs.mpv
    pkgs.msmtp
    pkgs.mtr
    pkgs.mu
    pkgs.multimarkdown
    pkgs.ncmpcpp
    pkgs.ncspot
    pkgs.nextcloud-client
    pkgs.nixfmt
    pkgs.ntfy-sh
    pkgs.obs-studio
    pkgs.obsidian
    pkgs.pandoc
    pkgs.patchelf
    pkgs.pavucontrol
    pkgs.pcmanfm
    pkgs.pinentry-gtk2
    pkgs.playerctl
    pkgs.pngquant
    pkgs.poppler
    pkgs.pwgen
    pkgs.python311Packages.pip
    pkgs.python311Packages.setuptools
    pkgs.python38Packages.pdftotext
    pkgs.python3Minimal
    pkgs.qalculate-gtk
    pkgs.qrencode
    pkgs.qutebrowser
    pkgs.ripgrep
    pkgs.rocm-opencl-icd
    pkgs.rocm-opencl-runtime
    pkgs.rpm
    pkgs.ruby
    pkgs.rubyPackages.net-scp
    pkgs.rubyPackages.net-ssh
    pkgs.rubyPackages.pry
    pkgs.scrot
    pkgs.shfmt
    pkgs.bitwarden
    pkgs.bitwarden-cli
    pkgs.rofi-rbw-wayland
    pkgs.signal-desktop
    pkgs.slurp
    pkgs.socat
    pkgs.somafm-cli
    pkgs.spotify-player
    pkgs.sqlite
    pkgs.starship
    pkgs.super-slicer-latest
    pkgs.swappy
    pkgs.sway
    pkgs.swayidle
    pkgs.swaylock
    pkgs.tdns-cli
    pkgs.tmux
    pkgs.tmux-cssh
    pkgs.tmuxinator
    pkgs.todoman
    pkgs.toilet
    pkgs.tree
    pkgs.unzip
    pkgs.upower
    pkgs.vale
    pkgs.vdirsyncer
    pkgs.viu
    pkgs.vulkan-loader
    pkgs.vulkan-validation-layers
    pkgs.w3m
    pkgs.wally-cli
    pkgs.wayvnc
    pkgs.weather-icons
    pkgs.wf-recorder
    pkgs.wl-clipboard
    pkgs.wlroots
    pkgs.wofi
    pkgs.wol
    pkgs.wordnet
    pkgs.wpa_supplicant_gui
    pkgs.wtype
    pkgs.xdragon
    pkgs.xplr
    pkgs.xwayland
    pkgs.nodePackages.bash-language-server
    pkgs.nixd
    pkgs.terraform-ls
    pkgs.terraform-lsp
    pkgs.yaml-language-server
    pkgs.ydotool
    pkgs.yq-go
    pkgs.yt-dlp
    pkgs.yubikey-manager
    pkgs.yubikey-personalization
    pkgs.zathura
  ];

  home.pointerCursor = {
    gtk.enable = true;
    package = pkgs.capitaine-cursors;
    name = "capitaine-cursors";
    size = 22;
  };

  programs.starship.enable = true;
  programs.starship.enableBashIntegration = true;
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs.atuin = {
    enable = true;
    package = pkgs.atuin;
    enableBashIntegration = true;
    flags = [ "--disable-up-arrow" ];
    settings = {
      style = "compact";
      word_jump_mode = "emacs";
      search_mode = "fuzzy";
      inline_height = 30;
    };
  };
  programs.bash = {
    enable = true;
    enableCompletion = true;
    historyFileSize = 80000;
    historyControl = [ "ignoredups" "ignorespace" ];
    sessionVariables = { HISTTIMEFORMAT = "%Y-%m-%d-%H%M%S "; };
    profileExtra = ''
      if [[ $(/run/current-system/sw/bin/tty) == "/dev/tty1" ]]; then
        exec sway-run
      fi;
    '';
    initExtra = ''
      for f in ~/.config/bash/*.sh; do source $f; done
    '';
  };

  programs.readline = {
    enable = true;
    extraConfig = ''
      set completion-ignore-case On
      set colored-stats On
      set mark-symlinked-directories On
      set colored-completion-prefix On
      set menu-complete-display-prefix On
    '';
  };
  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.fish.package = pkgs.fish;

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
  programs.broot = {
    enable = true;
    enableBashIntegration = true;
    settings = {
      verbs = [
        {
          key = "ctrl-j";
          execution = ":line_down";
        }
        {
          key = "ctrl-k";
          execution = ":line_up";
        }
        {
          key = "ctrl-l";
          execution = ":panel_right";
        }
        {
          key = "ctrl-h";
          execution = ":back";
        }
        {
          key = "ctrl-d";
          execution = ":page_down";
        }
        {
          key = "ctrl-u";
          execution = ":page_up";
        }
        {
          invocation = "drop";
          external = "drop {file}";
          apply_to = "file";
          leave_broot = false;
          switch_terminal = false;
        }
        {
          invocation = "drag";
          external = "drag {file}";
          apply_to = "file";
          leave_broot = false;
          switch_terminal = false;
        }
      ];
      skin = {
        default = "gray(1) None";
        tree = "gray(7) None / gray(18) None";
        file = "gray(3) None / gray(8) None";
        directory = "ansi(25) None Bold / ansi(25) None";
        exe = "ansi(130) None";
        link = "Magenta None";
        pruning = "gray(12) None Italic";
        perm__ = "gray(5) None";
        perm_r = "ansi(94) None";
        perm_w = "ansi(132) None";
        perm_x = "ansi(65) None";
        owner = "ansi(138) None";
        group = "ansi(131) None";
        dates = "ansi(66) None";
        sparse = "ansi(214) None";
        git_branch = "ansi(229) None";
        git_insertions = "ansi(28) None";
        git_deletions = "ansi(160) None";
        git_status_current = "gray(5) None";
        git_status_modified = "ansi(28) None";
        git_status_new = "ansi(94) None Bold";
        git_status_ignored = "gray(17) None";
        git_status_conflicted = "ansi(88) None";
        git_status_other = "ansi(88) None";
        selected_line = "None gray(19) / None gray(21)";
        char_match = "ansi(22) None";
        file_error = "Red None";
        flag_label = "gray(9) None";
        flag_value = "ansi(166) None Bold";
        input = "gray(1) None / gray(4) gray(20)";
        status_error = "gray(22) ansi(124)";
        status_normal = "gray(2) gray(20)";
        status_job = "ansi(220) gray(5)";
        status_italic = "ansi(166) gray(20)";
        status_bold = "ansi(166) gray(20)";
        status_code = "ansi(17) gray(20)";
        status_ellipsis = "gray(19) gray(15)";
        purpose_normal = "gray(20) gray(2)";
        purpose_italic = "ansi(178) gray(2)";
        purpose_bold = "ansi(178) gray(2) Bold";
        purpose_ellipsis = "gray(20) gray(2)";
        scrollbar_track = "gray(20) none";
        scrollbar_thumb = "ansi(238) none";
        help_paragraph = "gray(2) none";
        help_bold = "ansi(202) none bold";
        help_italic = "ansi(202) none italic";
        help_code = "gray(5) gray(22)";
        help_headers = "ansi(202) none";
        help_table_border = "ansi(239) None";
        preview_title = "gray(3) None / gray(5) None";
        preview = "gray(5) gray(23) / gray(7) gray(23)";
        preview_line_number = "gray(6) gray(20)";
        preview_match = "None ansi(29) Underlined";
        hex_null = "gray(15) None";
        hex_ascii_graphic = "gray(2) None";
        hex_ascii_whitespace = "ansi(143) None";
        hex_ascii_other = "ansi(215) None";
        hex_non_ascii = "ansi(167) None";
        staging_area_title = "gray(8) None / gray(13) None";
        mode_command_mark = "gray(15) ansi(204) Bold ";
      };
    };
  };
  programs.zoxide = {
    enable = true;
    enableFishIntegration = true;
  };
  #nixpkgs.config.allowUnfree = true;
  #nixpkgs.config.chromium.commandLineArgs = "--enable-features=UseOzonePlatform --ozone-platform=wayland";

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
      "spellright.language" = [ "de_DE" "en_GB" "en_US" ];
    };
  };

  systemd.user.targets.sway-session = {
    Unit = {
      Description = "Sway graphical session target";
      BindsTo = "graphical-session.target";
      Wants = "graphical-session-pre.target";
      After = "graphical-session-pre.target";
    };
  };

  systemd.user.services.comacs = {
    Unit = {
      Description = "Emacs Coding daemon";
      X-RestartIfChanged = "false";
    };
    Service = {
      ExecStart =
        "${pkgs.bash}/bin/bash -l -c '${config.services.emacs.package}/bin/emacs --profile comacs --fg-daemon=comacs'";
      Restart = "on-failure";
      SuccessExitStatus = 15;
      Type = "notify";
      Environment = "EMACS_SERVER_NAME=comacs";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  systemd.user.services.workmacs = {
    Unit = {
      Description = "Emacs Worklog daemon";
      X-RestartIfChanged = "false";
    };
    Service = {
      ExecStart =
        "${pkgs.bash}/bin/bash -l -c '${config.services.emacs.package}/bin/emacs  --profile workmacs --name Worklog --fg-daemon=workmacs'";
      Restart = "on-failure";
      SuccessExitStatus = 15;
      Type = "notify";
      Environment = "EMACS_SERVER_NAME=workmacs";
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
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
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  systemd.user.services.clipboard-manager = {
    Unit = {
      Description = "Clipboard manager";
      After = "graphical-session-pre.target";
      PartOf = "graphical-session.target";
    };
    Service = {
      ExecStart =
        "${pkgs.wl-clipboard}/bin/wl-paste -t text --watch clipman store --no-persist --max-items=1000 --histpath=~/.local/share/clipman-primary.json";
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  systemd.user.sockets.dbus = {
    Unit = { Description = "D-Bus User Message Bus Socket"; };
    Socket = {
      ListenStream = "%t/bus";
      ExecStartPost =
        "${pkgs.systemd}/bin/systemctl --user set-environment DBUS_SESSION_BUS_ADDRESS=unix:path=%t/bus";
    };
    Install = {
      WantedBy = [ "sockets.target" ];
      Also = [ "dbus.service" ];
    };
  };

  systemd.user.services.dbus = {
    Unit = {
      Description = "D-Bus User Message Bus";
      Requires = [ "dbus.socket" ];
    };
    Service = {
      ExecStart =
        "${pkgs.dbus}/bin/dbus-daemon --session --address=systemd: --nofork --nopidfile --systemd-activation";
      ExecReload =
        "${pkgs.dbus}/bin/dbus-send --print-reply --session --type=method_call --dest=org.freedesktop.DBus / org.freedesktop.DBus.ReloadConfig";
    };
    Install = { Also = [ "dbus.socket" ]; };
  };

  systemd.user.services.swayidle = {
    Unit = {
      Description = "Idle Manager for Wayland";
      After = "graphical-session-pre.target";
      PartOf = "graphical-session.target";
    };
    Service = {
      ExecStart = ''
        ${pkgs.swayidle}/bin/swayidle -w -d \
                      before-sleep '${config.home.homeDirectory}/bin/blur-lock' \
                      unlock '${config.home.homeDirectory}/bin/thinkpad-dock' \
                      timeout 300 '${config.home.homeDirectory}/bin/blur-lock' \
                      timeout 600 '${pkgs.sway}/bin/swaymsg "output * dpms off"' resume '${pkgs.sway}/bin/swaymsg "output * dpms on" && /home/bascht/bin/thinkpad-dock' \
                      timeout 900 '${config.home.homeDirectory}/bin/suspend-if-docked' resume '${pkgs.sway}/bin/swaymsg "output * dpms on" && /home/bascht/bin/thinkpad-dock'
      '';
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  systemd.user.services.alfaview-sway-event-bus = {
    Unit = {
      Description = "alfaview sway event bus";
      X-RestartIfChanged = "false";
    };
    Service = {
      ExecStart = "/home/bascht/bin/alfaview-sway-event-bus";
      Restart = "on-failure";
      RestartSec = 5;
    };
    Install = { WantedBy = [ "graphical-session.target" ]; };
  };

  services.wlsunset = {
    enable = true;
    latitude = "48.15";
    longitude = "11.64";
    temperature = {
      day = 6500;
      night = 3000;
    };
  };

  services.mopidy = {
    enable = true;
    extensionPackages = with pkgs; [
      mopidy-iris
      mopidy-local
      mopidy-mpd
      mopidy-somafm
      mopidy-tunein
      mopidy-youtube
      mopidy-mpris
      mopidy-bandcamp
      mopidy-jellyfin
      yt-dlp
    ];

    settings = {
      file = { media_dirs = [ "~/Musik" ]; };
      youtube = { youtube_dl_package = "yt_dlp"; };
    };
  };

  services.syncthing = { enable = true; };

  services.mbsync.enable = true;
  services.mbsync.verbose = false;
  services.gnome-keyring.enable = true;

  services.nextcloud-client.enable = true;
  services.emacs.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = epkgs: [
      epkgs.apropospriate-theme
      epkgs.zoxide
      epkgs.vterm
      epkgs.pdf-tools
      epkgs.vscode-icon
      epkgs.mingus
      epkgs.benchmark-init
      epkgs.mu4e
      epkgs.hass
      epkgs.spacious-padding
      epkgs.org-pdftools
      epkgs.org-super-agenda
      epkgs.org-mru-clock
      epkgs.org-journal
      epkgs.org-alert
      epkgs.org-caldav
      epkgs.csv-mode
      epkgs.ef-themes
      epkgs.literate-calc-mode
      epkgs.dwim-shell-command
      epkgs.obsidian
      epkgs.nerd-icons-dired
      epkgs.dirvish
      epkgs.zoxide
      epkgs.dictcc
      epkgs.toml-mode
    ];
  };

  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    extraConfig = ''
      set number
    '';
    plugins = with pkgs.vimPlugins; [
      vim-which-key
      vim-nix
      vim-fish
      vim-surround
      vim-plug
      nvim-ale-diagnostic
      ale
      neorg
      vim-go
      deoplete-nvim
      deoplete-go
      #cmp-nvim-lsp
      nvim-lspconfig

    ];
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.11";
}
