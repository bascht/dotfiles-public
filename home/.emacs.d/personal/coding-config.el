;;; bascht --- Code configs
;;; Commentary:
;;; Coding related settings / libs

;;; Code:

(require 'rspec-mode)

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

(defun colorize-compilation-buffer () "Nice colours there."
  (read-only-mode)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(defun do-in-root (f) "Execute command F in project root."
       (if (projectile-project-p)
           (funcall f (projectile-project-root))
         (error "You're not in project")))

(defun helm-do-ag-in-root () "Open helm ag current project root."
       (interactive)
       (do-in-root 'helm-do-ag))

(defun do-ag-in-root (string) "Search for  ag for STRING in current project root."
       (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))))
       (do-in-root '(lambda (root) (ag/search string root))))


(add-hook 'rspec-mode-hook 'turn-off-flycheck)
(setq rspec-command-options "--format progress")
(setq rspec-spec-command "rspec")
(setq ruby-insert-encoding-magic-comment nil)

(setq flycheck-rust-executable "/usr/local/bin/rustc")
(setq flycheck-rust-library-path (quote ("/usr/local/lib")))

;;; Fix up rust mode
(setenv "LD_LIBRARY_PATH"
        (let ((current (getenv "LD_LIBRARY_PATH"))
              (new "/usr/local/libs"))
          (if current (concat new ":" current) new)))

(provide 'coding-config)
;;; coding-config.el ends here
