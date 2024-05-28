(define-module (bascht home emacs-service)
  #:use-module (guix gexp))

(define-public (generic-emacs-systemd-unit name)
  `(,(format #f "systemd/user/~a.service" name)
    ,(plain-file (format #f "~a.service" name)
                 (format #f "[Install]
WantedBy=graphical-session.target
[Service]
Type=simple
ExecStart=/var/home/bascht/.guix-home/profile/bin/emacs --fg-daemon=~a
ExecStop=/var/home/bascht/.guix-home/profile/bin/emacsclient -s ~a --no-wait --eval '(progn (setq kill-emacs-hook nil) (kill emacs))'
Restart=on-failure
Environment=COLORTERM=truecolor
Environment=DOOMPROFILE=~a
Environment=EMACSLOADPATH=/var/home/bascht/.guix-home/profile/share/emacs/site-lisp:/var/home/bascht/.guix-home/profile/share/emacs/site-lisp
Environment=EMACSNATIVELOADPATH=/var/home/bascht/.guix-home/profile/lib/emacs/native-site-lisp:/var/home/bascht/.guix-home/profile/lib/emacs/native-site-lisp

[Unit]
Description=Emacs ~a daemon
After=graphical-session-pre.target
PartOf=graphical-session.target
X-RestartIfChanged=false"
                         name name name name))))
;; TODO: There should be a guix-y way to find outh the path to the executable
;; TODO: There must a better way to format this
