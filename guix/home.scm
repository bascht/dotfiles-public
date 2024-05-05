;; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules (gnu home)
             (gnu packages)
             (gnu packages shells)
             (gnu packages gnupg)
             (gnu services)
             (guix gexp)
             (gnu home services)
             (gnu home services shells)
             (gnu home services gnupg)
             (gnu services security-token)
             )

(home-environment
  ;; Below is the list of packages that will show up in your
  ;; Home profile, under ~/.guix-home/profile.
  (packages (specifications->packages (list "tmux"
                                            "emacs-pdf-tools"
                                            "node"
                                            "bat"
                                            "fd"
                                            "wl-clipboard"
                                            "font-victormono-nerd-fonts"
                                            "font-nerdfontssymbolsonly-nerd-fonts"
                                            "eza"
                                            "fzf"
                                            "direnv"
                                            "zoxide"
                                            "mu"
                                            "font-awesome"
                                            "distrobox"
                                            "qtwayland"
                                            "socat"
                                            "dmenu"
                                            "i3status-rust"
                                            "font-jetbrains-mono"
                                            "font-ibm-plex"
                                            "bemenu"
                                            "pinentry-bemenu"
                                            "foot"
                                            "sway"
                                            "curl"
                                            "ripgrep"
                                            "gnupg"
                                            "nss-certs"
                                            "git"
                                            "glibc-locales"
                                            "chezmoi"
                                            "qutebrowser"
                                            "emacs")))

  ;; Below is the list of Home services.  To search for available
  ;; services, run 'guix home search KEYWORD' in a terminal.
  (services
   (list (service home-bash-service-type
                  (home-bash-configuration
                   (aliases '(("ag" . "rg")
                              ("alfaview-logfile" . "/usr/bin/env ls -d -- ~/.local/share/alfaview-staging/logs/*.log|tail -n 1")
                              ("baml" . "|bat -l yaml")
                              ("be" . "bundle exec")
                              ("ber" . "bundle exec rake")
                              ("bi" . "bundle install")
                              ("cm" . "chezmoi")
                              ("cm-private" . "chezmoi -S ~/.local/share/chezmoi-private")
                              ("dco" . "docker-compose")
                              ("dcorrm" . "docker-compose run --rm")
                              ("drri" . "docker run --rm -ti")
                              ("dry" . "drri -v /var/run/docker.sock:/var/run/docker.sock skanehira/docui")
                              ("emacseval" . "emacs -batch -l ~/.emacs.d/init.el -eval '\\''(org-batch-agenda a)'\\''")
                              ("g" . "git")
                              ("gcm" . "git checkout $(git branch --format='\\''%(refname:short)'\\'' | grep -E '\\''^main|^master'\\'')")
                              ("gf" . "git fetch -a && git rebase && git --no-pager today-all;  commandline --insert '\\'''\\''")
                              ("git-cleanup-merged-branches" . "git fetch -va && git branch --merged | egrep -v '\\''(^\\*|master)'\\'' | xargs git branch -d")
                              ("gp" . "gopass")
                              ("hl" . "hledger")
                              ("k" . "kubectl")
                              ("kgp" . "kubectl get pods")
                              ("kn" . "kubens")
                              ("krs" . "kubectl rollout status")
                              ("ls" . "eza --icons")
                              ("nps" . "nix search nixpkgs ")
                              ("osslb64" . "base64 -d | openssl x509 -noout -text")
                              ("osslc" . "openssl s_client -connect")
                              ("osslx" . "openssl x509 -noout -text")
                              ("oyaml" . "-o yaml")
                              ("scp-yop" . "scp -q -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no")
                              ("ssh-yop" . "ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no")
                              ("tdir" . "take $(mktemp -d --suffix=.$(basename $(pwd)))")
                              ("termbin" . "nc termbin.com 9999")
                              ("va" . "vagrant")
                              ("vim" . "em")
                              ("wlposslx" . "wl-paste|openssl x509 -noout -text")
                              ("wttr" . "curl '\\''wttr.in/Neuwied?1pQF&lang=de'\\''")))
                   (bashrc (list (local-file
                                  ".bashrc"
                                  "bashrc")))
                   (bash-logout (list (local-file
                                       ".bash_logout"
                                       "bash_logout")))))
         (simple-service 'some-useful-env-vars-service
                         home-environment-variables-service-type
                         `(("LESSHISTFILE" . "$XDG_CACHE_HOME/.lesshst")
                           ("PATH" . "~/bin:${PATH}")
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
                           ("FZF_DEFAULT_OPTS" . " --prompt=' ' --border --height='40%' --layout=reverse --color=bg+:#f0f0f1,bg:#fafafa,spinner:#0184bc,hl:#4078f2 --color=fg:#696c77,header:#4078f2,info:#c18401,pointer:#0184bc --color=marker:#0184bc,fg+:#202227,prompt:#c18401,hl+:#4078f2")

                            ))
         ;; (service pcscd-service-type
         ;;          )
         (service home-gpg-agent-service-type
                  (home-gpg-agent-configuration
                   (pinentry-program
                    (file-append pinentry-bemenu "/bin/pinentry-bemenu"))
                   (ssh-support? #t)))
         )))
