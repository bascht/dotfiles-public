;;; bascht --- org stuff
;;; Commentary:
;;; org-mode related settings

;;; Code:

(use-package org)
(use-package org-mu4e)
(use-package org-alert
  :commands (org-alert-enable))

(setq org-directory "~/Documents/Zettelkasten")
(setq org-agenda-files (quote ("~/Documents/Zettelkasten")))
(setq org-default-notes-file "~/Documents/Zettelkasten/refile.org")
(setq org-agenda-hide-tags-regexp "presents")
(setq org-agenda-include-diary t)
(setq org-agenda-show-inherited-tags (quote always))
(setq org-agenda-with-colors t)
(setq org-deadline-warning-days 5)
(setq org-scheduled-delay-days 0)
(setq org-agenda-default-appointment-duration 60)
(setq org-agenda-columns-add-appointments-to-effort-sum t)

(setq org-icalendar-alarm-time 120)
(setq org-icalendar-combined-agenda-file "~/Nextcloud/OrgExport/Org.ics")
(setq org-icalendar-include-todo (quote all))
(setq org-icalendar-store-UID t)
(setq org-icalendar-timezone "UTC+2:00")
(setq org-icalendar-use-deadline (quote (event-if-todo todo-due)))
(setq org-icalendar-use-scheduled (quote (event-if-todo todo-start)))
(setq org-icalendar-with-timestamps nil)
(setq org-journal-dir "~/Documents/Zettelkasten/Journal")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Documents/Zettelkasten/Todo.org" "Inbox")
         "* TODO %?\n  %i")
        ("m" "MailTodo" entry (file+headline "~/Documents/Zettelkasten/Todo.org" "Inbox")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("r" "RubyShift" entry (file+headline "~/Documents/Zettelkasten/Projects.org" "RubyShift")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-completion-use-ido t)

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;;; Fancy!
(setq org-alert-notification-title "OrgMode")
(setq org-plantuml-jar-path "/home/bascht/bin/plantuml.jar")

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t))))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes (quote ("plantuml" . plantuml)))

(provide 'org-config)
;;; org-config.el ends here
