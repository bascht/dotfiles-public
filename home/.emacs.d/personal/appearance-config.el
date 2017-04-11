;;; bascht --- all the looks
;;; Commentary:
;;; All the looks

;;; Code:

(use-package rainbow-mode)
(use-package rainbow-delimiters-mode)

(use-package avk-emacs-themes
  :ensure t
  :config
  (load-theme 'avk-daylight t))


(set-face-attribute 'default nil :font "Mononoki-12" )
(set-frame-font "Mononoki-12" nil t)
(setq default-frame-alist '((font . "Mononoki-12")))

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun on-after-init () "Don't set a background for -nw Emacs."
       (unless (display-graphic-p (selected-frame))
         (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

;;; Nice Org alert boxes
(setq alert-default-style 'libnotify)

;;; Mail niceties
(setq notmuch-search-line-faces
 (quote
  (("unread" :weight bold :foreground "light black" :background "light blue")
   ("flagged" :foreground "red" :background))))

(custom-set-faces
 '(mu4e-flagged-face ((t (:inherit font-lock-constant-face :foreground "deep sky blue" :weight bold))))
 '(mu4e-unread-face ((t (:inherit bold :background "medium violet red" :foreground "white smoke"))))
 '(sml/filename ((t (:foreground "dim gray" :weight bold))))
 '(sml/modes ((t (:foreground "dim gray" :weight bold)))))

(provide 'appearance-config)
;;; appearance-config.el ends here
