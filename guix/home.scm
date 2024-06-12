(add-to-load-path "/var/home/bascht/Code/bascht/dotfiles/guix/modules/")
(use-modules (bascht binary-packages)
             (bascht home bash)
             (bascht home emacs-service)
             (bascht home packages)
             (bascht home systemd-units)
             (gnu home services desktop)
             (gnu home services fontutils)
             (gnu home services gnupg)
             (gnu home services guix)
             (gnu home services shells)
             (gnu home services shepherd)
             (gnu home services sound)
             (gnu home services)
             (gnu packages admin)
             (gnu packages emacs)
             (gnu packages gnupg)
             (gnu packages shells)
             (gnu packages wm)
             (gnu packages)
             (gnu services security-token)
             (gnu services shepherd)
             (gnu services)
             (guix build-system copy)
             (guix channels)
             (guix download)
             (guix gexp)
             (guix licenses )
             (guix packages)
             (uh packages fonts)
             (gnu home))

(define bascht-home-variant-packages-service
  (simple-service 'variant-packages-service
                  home-channels-service-type
                  (list
                   (channel
                    (name 'uh)
                    (url "https://git.sr.ht/~uh/guix-channel")
                    (branch "trunk")
                    (introduction
                     (make-channel-introduction "66e59621e272b69d155b498cc0d949e58b8568c0"
                      (openpgp-fingerprint "9E26 D2DC 55BF 9256 4743  4768 C516 6D9E D689 7893")))))))

(define bascht-packages (append (map specification->package+output builtin-packages)
                                 (list gopass
                                       git-credential-gopass
                                       starship
                                       atuin
                                       cliphist
                                       font-victormono-nerd-fonts
                                       font-nerdfontssymbolsonly-nerd-fonts
                                       font-monaspace-nerd-fonts
                                       font-jetbrainsmono-nerd-fonts)))

(define bascht-home-variable-service (simple-service 'bascht-home-variable-service
                 home-environment-variables-service-type
                 `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                   ("PATH" . "${HOME}/bin:${HOME}/.config/emacs/bin:${HOME}/.config/guix/current/bin:${PATH}")
                   ("GUIX_LOCPATH" . "${HOME}/.guix-home/profile/lib/locale")
                   ("IBUS_SOCK" . "$XDG_RUNTIME_DIR/ibus.socket")
                   ("XDG_CURRENT_DESKTOP" . "sway")
                   ("XDG_SESSION_TYPE" . "wayland")
                   ("MOZ_ENABLE_WAYLAND" . "1")
                   ("MOZ_USE_XINPUT2" . "1")
                   ("EDITOR" . "vim")
                   ("XKB_DEFAULT_LAYOUT" . "de")
                   ("XKB_DEFAULT_OPTIONS" . "caps:escape")
                   ("QT_SCALE_FACTOR" . "1")
                   ("QT_AUTO_SCREEN_SCALE_FACTOR" . "1")
                   ("QT_WAYLAND_DISABLE_WINDOWDECORATION" . "1")
                   ("_JAVA_AWT_WM_NONREPARENTING" . "1")
                   ("GNOME_KEYRING_CONTROL" . "/run/user/$(id -u)/keyring")
                   ("VAGRANT_DEFAULT_PROVIDER" . "libvirt")
                   ("VAGRANT_DOTFILE_PATH" . ".vagrant-$(hostname)")
                   ("QT_QPA_PLATFORMTHEME" . "qt5ct")
                   ("SWAYSOCK" . "/run/user/$(id -u)/sway-ipc.sock")
                   ("SDL_VIDEODRIVER" . "wayland")
                   ("QT_QPA_PLATFORM" . "wayland-egl")
                   ("FZF_DEFAULT_OPTS" . ""))))

(define bascht-home-gpg-agent-service
  (service home-gpg-agent-service-type
           (home-gpg-agent-configuration
            (pinentry-program
             (file-append pinentry-rofi "/bin/pinentry-rofi"))
            (ssh-support? #t))))

(define bascht-home-xdg-config
  (simple-service 'bascht-home-xdg-config
                 home-xdg-configuration-files-service-type
                 (list
                  (generic-emacs-systemd-unit "emacs")
                  (generic-emacs-systemd-unit "comacs")
                  (generic-emacs-systemd-unit "workmacs")
                  (generic-graphical-systemd-unit "wlsunset"
                                                  "wlsunset -l 50.5 -L 7.4 -d 10"
                                                  "Dim Screen at sunset" )
                  (generic-graphical-systemd-unit "cliphist"
                                                  "wl-paste --watch cliphist store"
                                                  "Clipboard manager" )
                  (generic-graphical-systemd-unit "syncthing"
                                                  "/var/home/bascht/.guix-home/profile/bin/syncthing -no-browser -no-restart -logflags=0"
                                                  "File sync" )
                  (generic-graphical-systemd-unit "swayidle"
                                                  "swayidle -w -d"
                                                  "Idle Manager for Wayland" )
                  )))

(home-environment
 (packages bascht-packages)

 (services
  (list (service home-channels-service-type)
        bascht-home-variant-packages-service
        bascht-home-bash-configuration
        bascht-home-variable-service
        bascht-home-gpg-agent-service
        bascht-home-xdg-config)))
