;;; bascht --- org stuff
;;; Commentary:
;;; org-mode related settings

;;; Code:

(use-package org)
(use-package org-mu4e)

(setq org-directory "~/Documents/Zettelkasten")
(setq org-agenda-files (quote ("~/Documents/Zettelkasten")))
(setq org-default-notes-file "~/Documents/Zettelkasten/refile.org")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/Zettelkasten/Todo.org" "Inbox")
         "* TODO %?\n  %i\n  %a")
        ("m" "MailTodo" entry (file+headline "~/Documents/Zettelkasten/Todo.org" "Inbox")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("r" "RubyShift" entry (file+headline "~/Documents/Zettelkasten/Projects.org" "RubyShift")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-completion-use-ido t)

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

(provide 'org-config)
;;; org-config.el ends here
