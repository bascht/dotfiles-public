;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! org-roam)
(package! org-roam-ui)
(package! ef-themes)
(package! dirvish)
(package! mingus)
(package! spacious-padding)
(package! md-roam
  :recipe (:host github
           :repo "nobiot/md-roam"
           :files ("md-roam.el")))

(package! pdf-tools :built-in 'prefer)
(package! emacsql-sqlite-builtin :built-in 'prefer)
(package! org-modern :built-in 'prefer)
(package! dictcc :built-in 'prefer)

;; Until https://github.com/doomemacs/doomemacs/issues/7196 is fixed:
(unpin! evil-collection)
(package! evil-collection
  :recipe (:repo "emacs-evil/evil-collection" :branch "master"))

(when (string= (system-name) "apfelstrudel")
  (package! protobuf-mode)
  (package! salt-mode)
  (package! ob-grpc :recipe
    (:host github :repo "shsms/ob-grpc")))

