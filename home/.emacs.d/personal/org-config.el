;;; bascht --- org stuff
;;; Commentary:
;;; org-mode related settings

;;; Code:

(use-package org)

(setq org-directory "~/Documents/Zettelkasten")
(setq org-agenda-files (quote ("~/Documents/Zettelkasten")))
(setq org-default-notes-file "~/Documents/Zettelkasten/refile.org")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/Zettelkasten/Todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("r" "RubyShift" entry (file+headline "~/Documents/Zettelkasten/Rubyshift.org" "Unsorted")
         "* %?\nEntered on %U\n  %i\n  %a")))


(provide 'org-config)
;;; org-config.el ends here
