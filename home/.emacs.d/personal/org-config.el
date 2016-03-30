;;; bascht --- org stuff
;;; Commentary:
;;; org-mode related settings

;;; Code:

(require 'org)

(setq org-directory "~/ownCloud/clientsync/Zettelkasten")
(setq org-agenda-files (quote ("~/ownCloud/clientsync/Zettelkasten")))
(setq org-default-notes-file "~/ownCloud/clientsync/Zettelkasten/refile.org")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ownCloud/clientsync/Zettelkasten/Todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("r" "RubyShift" entry (file+headline "~/ownCloud/clientsync/Zettelkasten/Rubyshift.org" "Unsorted")
         "* %?\nEntered on %U\n  %i\n  %a")))


(provide 'org-config)
;;; org-config.el ends here
