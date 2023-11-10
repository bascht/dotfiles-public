;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! pdf-tools :built-in 'prefer)
(package! emacsql-sqlite-builtin :built-in 'prefer)
(package! dictcc :built-in 'prefer)
(package! org-caldav :built-in 'prefer)
(package! dirvish :built-in 'prefer)
(package! dired-rifle :built-in 'prefer)
(package! dired-subtree :built-in 'prefer)
(package! dired-sidebar :built-in 'prefer)
(package! dired-preview :built-in 'prefer)
(package! dired-narrow :built-in 'prefer)
(package! dired-quick-sort :built-in 'prefer)
(package! dired-ranger :built-in 'prefer)
(package! nerd-icons-dired :built-in 'prefer)
(package! treemacs-icons-dired :built-in 'prefer)
(package! all-the-icons-dired :built-in 'prefer)


;; Until https://github.com/doomemacs/doomemacs/issues/7196 is fixed:
(unpin! evil-collection)
(package! evil-collection
  :recipe (:repo "emacs-evil/evil-collection" :branch "master"))

(when (string= (system-name) "apfelstrudel")
  (package! protobuf-mode)
  (package! salt-mode)
  (package! ob-grpc :recipe
    (:host github :repo "shsms/ob-grpc")))


(package! khalel)
