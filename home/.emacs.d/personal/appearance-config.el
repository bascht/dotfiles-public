;;; bascht --- all the looks
;;; Commentary:
;;; All the looks

;;; Code:

(require 'moe-theme)
(setq moe-theme-highlight-buffer-id t)
(setq moe-theme-resize-org-title '(1.5 1.4 1.3 1.2 1.1 1.0 1.0 1.0 1.0))
(moe-dark)


(defun on-after-init () "Don't set a background for -nw Emacs."
       (unless (display-graphic-p (selected-frame))
         (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'on-after-init)

(if (display-graphic-p)
    '(progn
       '(menu-bar-mode)
       (custom-set-faces
        '(mode-line ((t (:background "#85C" :foreground "#85CEEB" :box (:line-width 1 :color "dodger blue")))))
        )

       (custom-set-faces
        '(mode-line ((t (:background "brightblack" :foreground "#4e4e4e" :box (:line-width 1 :color "dodger blue")))))
        )))

(provide 'appearance-config)
;;; appearance-config.el ends here
