;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-super-agenda)
(package! org-mru-clock)
(package! toml-mode)
(package! org-journal)
(package! org-alert)
(package! humanoid-themes)
(package! csv-mode)
(unpin! org-roam)
(package! org-roam-ui)
(package! ef-themes)
(package! literate-calc-mode)
(package! dwim-shell-command)
(package! obsidian)
(package! scad-mode)
(package! dirvish)
(package! zoxide)
(package! flycheck-vale)
(package! dictcc)

(when (string= (system-name) "apfelstrudel")
  (package! protobuf-mode)
  (package! salt-mode)
  (package! ob-grpc :recipe
    (:host github :repo "shsms/ob-grpc")))
