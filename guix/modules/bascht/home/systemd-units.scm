(define-module (bascht home systemd-units)
  #:use-module (guix gexp))

(define generic-graphical-systemd-unit-template
"[Install]
WantedBy=graphical-session.target
[Service]
ExecStart=~a
Restart=on-failure
RestartSec=5
[Unit]
After=graphical-session-pre.target
Description=~a
PartOf=graphical-session.target")

(define-public (generic-graphical-systemd-unit name command description)
  `(,(format #f "systemd/user/~a.service" name)
    ,(plain-file (string-append name ".service")
                 (format #f generic-graphical-systemd-unit-template command description)
                 )))

(define swayidle-systemd-unit
`("systemd/user/swayidle.service"
    ,(plain-file "swayidle.service"
                 "[Install]
WantedBy=graphical-session.target
[Service]
ExecStart=swayidle -w -d
Restart=on-failure
RestartSec=5
[Unit]
After=graphical-session-pre.target
Description=Idle Manager for Wayland
PartOf=graphical-session.target"
)))

(define syncthing-systemd-unit
`("systemd/user/syncthing.service"
    ,(plain-file "syncthing.service"
                 "[Install]
WantedBy=graphical-session.target
[Service]
ExecStart=syncthing -no-browser -no-restart -logflags=0
Restart=on-failure
RestartSec=5
[Unit]
After=graphical-session-pre.target
Description=Idle Manager for Wayland
PartOf=graphical-session.target"
)))
