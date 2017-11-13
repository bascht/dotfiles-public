;;; bascht --- meine settings
;;; Commentary:
;;; Hacky as whatnot, but it's mime.
;;; Code:

(use-package package)
(use-package ag)
(use-package notmuch)
(use-package notmuch-address)
(use-package multiple-cursors-core)
(use-package yasnippet)
(use-package rspec-mode)
(use-package ansi-color)

(add-to-list 'load-path "~/.emacs.d/personal/")

(load-library "org-config")
(load-library "appearance-config")
(load-library "coding-config")
(load-library "mail-config")
(load-library "shortcut-config")

;; Archives from which to fetch.
(setq package-archives
      (append '(("melpa" . "http://melpa.org/packages/"))
              package-archives))

;;; Hello, Emacs!
(setq user-full-name "Sebastian Schulze")
(setq user-mail-address "github.com@bascht.com")

(org-bullets-mode 1)

;;; Global modes
(projectile-global-mode)
(global-hl-line-mode 0)
(yas-global-mode 1)

;;; use yankpad in addition to yasnippet

(use-package yankpad
  :ensure t
  :defer 10
  :init
  (setq yankpad-file "~/.emacs.d/personal/yankpad.org")
  :config
  (bind-key "C-x y" 'yankpad-map)
  (bind-key "C-x x" 'yankpad-expand)
  ;; If you want to complete snippets using company-mode
  (add-to-list 'company-backends #'company-yankpad))

(add-hook 'text-mode-hook 'auto-fill-mode)

(setq browse-url-browser-function (quote browse-url-firefox))

;;; Enable Projectile  cache until I know what is going on
(setq projectile-enable-caching t)

;;; Directly jump into Commander.
(setq projectile-switch-project-action
      #'projectile-commander)

;;; Skip to next match group automatically
(setq helm-move-to-line-cycle-in-source nil)
(setq helm-ls-git-fuzzy-match t)

;;; Start with a German  dictionary
;;;(ispell-change-dictionary "deutsch8")

;;; Show OrgMode Aganda on start.
;; (cond
;;  ((string-equal system-name "kandalingo")
;;   (add-hook 'after-init-hook 'org-agenda-list)))



;;; bascht.el ends here
