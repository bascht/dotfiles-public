;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
;(package! pdf-tools :built-in 'prefer)
;; (package! dirvish :built-in 'prefer)

(package! vscode-icon :built-in 'prefer)
(package! nerd-icons-dired :built-in 'prefer)
(package! dictcc :built-in 'prefer)
(package! org-caldav :built-in 'prefer)
(package! khalel :built-in 'prefer)

(package! ef-themes :built-in 'prefer)
(package! mingus :built-in 'prefer)
(package! spacious-padding :built-in 'prefer)

(when (string= (system-name) "apfelstrudel")
  (package! protobuf-mode)
  (package! salt-mode)
  (package! ob-grpc :recipe
    (:host github :repo "shsms/ob-grpc")))


