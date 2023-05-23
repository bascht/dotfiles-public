;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! org-roam)
(package! org-roam-ui)
(package! ef-themes)
(package! dirvish)
(package! md-roam
  :recipe (:host github
           :repo "nobiot/md-roam"
           :files ("md-roam.el")))

(package! pdf-tools :built-in 'prefer)
(package! emacsql-sqlite-builtin :built-in 'prefer)
(package! org-modern :built-in 'prefer)

(when (string= (system-name) "apfelstrudel")
  (package! protobuf-mode)
  (package! salt-mode)
  (package! ob-grpc :recipe
    (:host github :repo "shsms/ob-grpc")))
