;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! pdf-tools :built-in 'prefer)
(package! dirvish :built-in 'prefer)
(package! mu4e :built-in 'prefer)
(package! vscode-icon :built-in 'prefer)
(package! nerd-icons-dired :built-in 'prefer)
(package! dictcc :built-in 'prefer)
(package! khalel :built-in 'prefer)
(package! hass :built-in 'prefer)

(package! ace-window :built-in 'prefer)
(package! spacious-padding)
(package! rainbow-mode)

(package! ef-themes :built-in 'prefer)
(package! mingus :built-in 'prefer)
(package! spacious-padding :built-in 'prefer)
(package! apropospriate-theme :built-in 'prefer)
(package! literate-calc-mode :built-in 'prefer)
(package! org-alert :built-in 'prefer)
(package! simple-mpc :built-in 'prefer)
(package! magit-todos :disable t)

(when (string= (system-name) "apfelstrudel")
  (package! protobuf-mode)
  (package! salt-mode)
  (package! cmake-mode)
  (package! ob-grpc :recipe
    (:host github :repo "shsms/ob-grpc")))


