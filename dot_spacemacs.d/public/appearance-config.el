;;; bascht --- all the looks
;;; Commentary:
;;; All the looks

;;; Code:

(use-package "rainbow-mode")

(disable-theme 'spacemacs-dark)
(disable-theme 'zenburn)

(use-package "avk-emacs-themes"
  :ensure t
  :config
  (disable-theme 'zenburn)
  (load-theme 'avk-daylight t))

(use-package "all-the-icons")

;(setq my-font "Mononoki-12")
;(setq my-font "InputMono Light-12")
(setq my-font "FiraMono-12:weight=normal")

(set-face-attribute 'default nil :font "FiraMono-12:weight=normal")
(set-frame-font "FiraMono-12:weight=normal" nil t)
(setq default-frame-alist '((font . "FiraMono-12:weight=normal")))

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

;; Poor Mans day mode
(defun bascht-night-mode nil
    (disable-theme 'avk-daylight)
    (load-theme 'avk-darkblue-yellow))

(defun bascht-day-mode nil
  (load-theme 'avk-darkblue-yellow)
  (disable-theme 'avk-daylight))

(provide 'appearance-config)
;;; appearance-config.el ends here
