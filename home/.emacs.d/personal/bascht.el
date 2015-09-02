;;; bascht --- meine settings
;;; Commentary:
;;; Hacky as whatnot, but it's mime.
;;; Code:

(require 'package)
(require 'ag)
(require 'nyan-mode)
(require 'notmuch)
(require 'notmuch-address)
(require 'multiple-cursors)
(require 'yasnippet)
(require 'rspec-mode)
(require 'ansi-color)

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

;;; Global modes
(projectile-global-mode)
(global-company-mode)
(global-hl-line-mode 0)
(yas-global-mode 1)

(setq ispell-dictionary "german")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(setq browse-url-browser-function (quote browse-url-firefox))

;;; bascht.el ends here
