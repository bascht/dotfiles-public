(define-module (bascht home emacs-service)
  #:use-module (guix gexp))

(define-public (generic-emacs-systemd-unit name)
  `(,(format #f "systemd/user/~a.service" name)
    ,(plain-file (format #f "~a.service" name)
                 (format #f "[Install]
WantedBy=graphical-session.target
[Service]
Environment=EMACS_SERVER_NAME=comacs
ExecStart=~a --profile comacs --fg-daemon=comacs
Restart=on-failure
SuccessExitStatus=15
Type=notify
[Unit]
Description=Emacs Coding daemon
X-RestartIfChanged=false"
                         "/var/home/bascht/.guix-home/profile/bin/emacs"))))
;; TODO: Fix the absolute path
