;;; bascht --- org stuff
;;; Commentary:
;;; org-mode related settings

;;; Code:

(use-package org)
(use-package org-journal)
(use-package org-mu4e)
(use-package org-alert
  :commands (org-alert-enable))

(setq org-directory "~/Documents/Zettelkasten")
(setq org-agenda-files '("~/Documents/Zettelkasten"))
(setq org-default-notes-file "~/Documents/Zettelkasten/refile.org")
(setq org-agenda-sticky nil)
(setq org-agenda-ndays 1)
(setq org-agenda-span 1)
(setq org-log-done t)
(setq org-agenda-file-regexp (format-time-string "\\`[^.].*\\.org\\'\\|\\`%Y%m[0-9]+\\'"))
(setq org-agenda-hide-tags-regexp "presents")
(setq org-agenda-include-diary t)
(setq org-agenda-show-inherited-tags (quote always))
(setq org-agenda-with-colors t)
(setq org-agenda-tags-column -105)
(setq org-agenda-default-appointment-duration 30)
(setq org-agenda-columns-add-appointments-to-effort-sum t)
(setq org-agenda-time-leading-zero t)
(setq org-archive-location "%s_archive::")
(setq org-deadline-warning-days 5)
(setq org-scheduled-delay-days 0)
                                        ;(setq org-duration-format (quote (("h" . t) (special . 2))))
(setq org-confirm-elisp-link-function nil)
(setq org-icalendar-alarm-time 120)
(setq org-icalendar-combined-agenda-file "~/Nextcloud/OrgExport/Org.ics")
(setq org-icalendar-include-todo (quote all))
(setq org-icalendar-store-UID t)
(setq org-icalendar-timezone "UTC+2:00")
(setq org-icalendar-use-deadline (quote (event-if-todo todo-due)))
(setq org-icalendar-use-scheduled (quote (event-if-todo todo-start)))
(setq org-icalendar-with-timestamps nil)
(setq org-journal-dir "~/Documents/Worklog/")
(setq org-journal-file-format "%Y%m%d")
(setq org-capture-templates
      '(("t" "Todo" entry (file "~/Documents/Zettelkasten/Todo.org")
         "* TODO %?\n  %i")
        ("m" "MailTodo" entry (file "~/Documents/Zettelkasten/Todo.org")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
        ("p" "Print" entry (file+headline "~/Documents/Zettelkasten/Personal.org" "Drucken")
         "* TODO %a drucken :@home:@print:%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%a\n")
        ("c" "CustomerIssue" entry (file "~/Documents/Zettelkasten/Todo.org")
         "* TODO issue%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")
        ("r" "RubyShift" entry (file+headline "~/Documents/Zettelkasten/Projects.org" "RubyShift")
         "* %?\nEntered on %U\n  %i\n  %a")))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 4))))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-history nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-clock-sound t)
(setq org-clock-out-remove-zero-time-clocks t)
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Do not prompt to resume an active clock, just resume it
(setq org-clock-persist-query-resume nil)

(setq org-agenda-start-with-clockreport-mode t)
(setq org-clock-report-include-clocking-task t)
(setq org-return-follows-link t)

(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-columns-default-format
      "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")

(setq org-agenda-current-time-string "┈	┈	┈	┈	┈	┈	┈ now ┈	┈	┈	┈	┈	┈")
(setq org-agenda-time-grid
  (quote
   ((daily require-timed)
    (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
    "......" "───────────────")))

(setq org-show-notification-handler "notify-send")

(setq org-tag-faces (quote (
                            ("next" . "red")
                            ("waiting" . "blue")
                            )))

                                        ;(setq org-agenda-current-time ((t (:inherit org-time-grid :foreground "orange red"))))

;;store link to message if in header view, not to header query
(setq org-mu4e-link-query-in-headers-mode nil)

;;; Fancy!
(setq org-alert-notification-title "OrgMode")
(setq org-plantuml-jar-path "/home/bascht/bin/plantuml.jar")

(setq org-modules
      (quote
       (org-bbdb
        org-bibtex
        org-docview
        org-gnus
        org-habit
        org-info
        org-irc
        org-mhe
        org-habit
        org-checklist
        org-rmail
        org-mouse
        org-w3m)))


(setq org-feed-alist
      '(("Pinboard"
         "https://rss.bascht.space/feed/makefulltextfeed.php?url=sec%3A%2F%2Ffeeds.pinboard.in%2Frss%2Fsecret%3A05e96c41c91076f3ca0e%2Fu%3Abascht%2Ftoread%2F&max=10&links=remove&exc=1&content=text"
         "~/Documents/Zettelkasten/Feeds.org" "Pinboard Unread")))

(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-src-lang-modes (quote ("plantuml" . plantuml)))

(use-package org-super-agenda
  :config (org-super-agenda-mode))

(setq org-ellipsis "  ")
(setq org-pretty-entities t)

(setq org-habit-today-glyph ?‖)
(setq org-habit-completed-glyph ?🗸)
(setq org-habit-show-habits-only-for-today nil)
(setq org-habit-graph-column 60)
(setq org-habit-following-days 3)


(setq org-agenda-custom-commands
      '(("l" "Open loops" 
         ((agenda ""))
         ((org-agenda-start-day "-1d")
          ;; (org-agenda-span 'week)
          (org-agenda-show-log nil)
          (org-agenda-ndays 2)
          (tags-todo "-imported")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          (org-agenda-start-with-log-mode '(closed clock state) )))
        ("D" "Old and DONE items" 
         ((agenda ""))
         ((org-agenda-start-day "-1d")
          ;; (org-agenda-span 'week)
          (org-agenda-show-log nil)
          (org-agenda-ndays 2)
          (tags-todo "-imported")
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo))
          (org-agenda-start-with-log-mode '(closed clock state) )))
        ("h" "Daily habits"
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
        ("u" "Super view"
         ((agenda "" ((org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t)))))
          (todo "" ((org-agenda-overriding-header "Other")
                    (org-super-agenda-groups
                     '((:auto-category t))                     )))
          (todo "" ((org-agenda-overriding-header "Grouped")
                    (org-super-agenda-groups
                     '((:name none  ; Disable super group header
                              :children todo)
                       (:discard (:anything t))))))))
        ("c" "Mega Agenda" agenda
         (org-super-agenda-mode)
         ((org-super-agenda-groups
           '(
             (:name "Important"
                    :priority "A")
             (:name "Next Items"
                    :tag ("NEXT" "outbox"))
             (:name "Immersive + Deep"
                    :tag ("@immersive" "@deep"))
             (:name "Process + Shallow"
                    :time-grid t
                    :tag ("@process" "@shallow"))
             (:name "Customer Projects"
                    :file-path "Customer"
                    )
             (:name "Quick Picks"
                    :effort< "0:30"
                    )
             (:priority<= "B"
                          :scheduled future
                          :order 1)))
          )
         ("d" "Sort during Daily" agenda
          (org-super-agenda-mode)
          (setq org-agenda-span 1)
          ((org-super-agenda-groups
            '(
              (:name "Quick Picks"
                     :effort< "0:30"
                     )
              (:name "Immersive + Deep"
                     :tag ("@immersive" "@deep"))
              (:name "Immersive + Shallow"
                     :tag ("@immersive" "@shallow"))
              (:name "Process + Deep"
                     :tag ("@process" "@deep"))
              (:name "Process + Shallow"
                     :tag ("@process" "@shallow"))
              (:name "Customer Projects"
                     :file-path "Customer"
                     )
              (:name "Low Prio and future"
               :priority<= "B"
                           :scheduled future
                           :order 1))))
          (org-agenda nil "a"))
         )))

(setq org-tag-alist '(
                      (:startgroup . nil)
                      ("@immersive" . ?i)
                      ("@process" . ?p)
                      (:endgroup . nil)
                      (:startgroup . nil)
                      ("@deep" . ?d)
                      ("@shallow" . ?s)
                      (:endgroup . nil)
                      (:startgroup . nil)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      (:endgroup . nil)
                      ("errand" . ?E)
                      ("phone" . ?P)
                      ("outbox" . ?O)
                      ("money" . ?M)
                      ("paper" . ?P)
                      ("next" . ?N)))

                                        ; Daily Review
(defun my-daily-review ()
  (interactive)
  (persp-switch "@Org")
  (find-file "/home/bascht/Documents/Zettelkasten/Todo.org")
  (goto-char (org-find-exact-headline-in-buffer "Daily Review"))
  (org-narrow-to-subtree)
  (org-cycle-hide-drawers 'all)
  (search-forward "[ ]"))

(defun my-org-agenda ()
  (interactive)
  (persp-switch "@Org")
  (let ((org-agenda-start-with-log-mode 'nil)
        (org-agenda-show-log 'nil)
        (org-agenda-span 'day)
        (org-agenda-use-time-grid t))
    (org-agenda nil "c")
    (org-agenda-redo)
    ))
                                        ; Recent loops callable for daily review
(defun my-org-agenda-recent-open-loops ()
  (interactive)
  (let ((org-agenda-start-with-log-mode t)
        (org-agenda-span 'day)
        (org-agenda-use-time-grid nil))
    (org-agenda nil "l")
    (beginend-org-agenda-mode-goto-beginning)))

(add-to-list 'org-global-properties
             '("Effort_ALL". "0:05 0:10 0:15 0:30 1:00 2:00 3:00 4:00"))

(setq org-agenda-category-icon-alist
      '(("Todo" "~/.icons/emacs/todo-16x16.png" nil nil :ascent center)
        ("Personal.*" "~/.icons/emacs/person-16x16.png" nil nil :ascent center)
        ("Customer.*" "~/.icons/emacs/briefcase-16x16.png" nil nil :ascent center)
        ("Project.*" "~/.icons/emacs/generic-folder-16x16.png" nil nil :ascent center)
        ("\\(ROPrivat\\|ROArbeit\\)" "~/.icons/emacs/calendar-16x16.png" nil nil :ascent center)
        (".*" '(space . (:width (16))))))

; Save file on every state change
(add-hook 'org-trigger-hook 'save-buffer)
(add-hook 'org-clock-in-hook 'save-buffer)
(add-hook 'org-clock-out-hook 'save-buffer)

(provide 'org-config)
;;; org-config.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
