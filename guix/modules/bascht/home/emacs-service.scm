(define-module (bascht home emacs-service)
  #:use-module (guix gexp))

(define-public (generic-emacs-systemd-unit name)
  `(,(format #f "systemd/user/~a.service" name)
    ,(plain-file (format #f "~a.service" name)
                 (format #f "[Install]
WantedBy=graphical-session.target
[Service]
ExecStart=/var/home/bascht/.guix-home/profile/bin/emacs --profile ~a --fg-daemon=~a
Restart=on-failure
SuccessExitStatus=15
Type=notify
[Unit]
Description=Emacs ~a daemon
After=graphical-session-pre.target
PartOf=graphical-session.target
X-RestartIfChanged=false"
                         name name name))))
;; TODO: There should be a guix-y way to find outh the path to the executable
;; TODO: There must a better way to format this
