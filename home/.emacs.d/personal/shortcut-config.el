;;; bascht --- Shortcuts
;;; Commentary:
;;; Just keyboard shortcuts

;;; Code:

(require 'notmuch)

(global-unset-key (kbd "C-c p p"))

(global-set-key [f5] 'compile)
(global-set-key [f6] 'comment-or-uncomment-region)

(global-set-key [f9] 'notmuch-unread)
(define-key notmuch-search-mode-map "F"
  (notmuch-search-toggle-flagged))


(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c f") 'helm-recentf)
(global-set-key (kbd "C-c p s s") 'helm-do-ag-in-root)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c H") 'helm-do-ag-in-root)
(global-set-key (kbd "C-c p p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-*") 'helm-swoop)
(global-set-key (kbd "M-p") 'ace-window)

;;; I <3 multiple cursors!
(define-key ctl-x-map "\C-m" #'mc/mark-all-dwim)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-s->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-s-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-s-c s-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;;; OrgMode Shortcuts

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Try to get used to these instead
(global-set-key (kbd "M-3") #'mc/mark-next-like-this)
(global-set-key (kbd "M-4") #'mc/mark-previous-like-this)
(global-set-key (kbd "M-§") #'mc/unmark-next-like-this)
(global-set-key (kbd "M-$") #'mc/unmark-previous-like-this)

(global-set-key (kbd "M-+") 'er/expand-region)

;;; Ace jump is dead! :)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)

;;; Navigate IDO with N + N
(add-hook 'ido-setup-hook '(lambda ()
                             (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
                             (define-key ido-completion-map "\C-n" 'ido-next-match)
                             (define-key ido-completion-map "\C-p" 'ido-prev-match)
                             ))

(defun fd-switch-dictionary() "Change Flyspell Dictionary on the fly."
  (interactive)
  (let* ((dic ispell-current-dictionary)
         (change (if (string= dic "deutsch8") "english" "deutsch8")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))

(global-set-key (kbd "<f8>")   'fd-switch-dictionary)


(provide 'shortcut-config)
;;; shortcut-config.el ends here
