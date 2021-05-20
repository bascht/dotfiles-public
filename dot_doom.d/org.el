;;; org.el -*- lexical-binding: t; -*-

(after! org

  (setq org-directory "~/Documents/Zettelkasten"
        calendar-week-start-day 1
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-default-appointment-duration 30
        org-agenda-file-regexp (format-time-string "\\`[^.].*\\.org\\'\\|\\`%Y%m[0-9]+\\'")
        org-agenda-files '("~/Documents/Zettelkasten")
        org-agenda-hide-tags-regexp "presents"
        org-agenda-include-diary t
        org-agenda-ndays 1
        org-agenda-show-inherited-tags (quote always)
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 1
        org-agenda-start-with-clockreport-mode t
        org-agenda-sticky nil
        org-agenda-tags-column -105
        org-agenda-time-leading-zero t
        org-agenda-with-colors t
        org-alert-notification-title "OrgMode"
        org-archive-location "%s_archive::"
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-report-include-clocking-task t
        org-clock-sound t
        org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
        org-confirm-babel-evaluate nil
        org-duration-format 'h:mm
        org-link-elisp-confirm-function nil
        org-deadline-warning-days 5
        org-default-notes-file "~/Documents/Zettelkasten/refile.org"
        org-ellipsis "  "
        org-habit-completed-glyph ?🗸
        org-habit-following-days 3
        org-habit-graph-column 60
        org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?‖
        org-hide-emphasis-markers t
        org-icalendar-alarm-time 120
        org-icalendar-combined-agenda-file "~/Nextcloud/OrgExport/Org.ics"
        org-icalendar-include-todo (quote all)
        org-icalendar-store-UID t
        org-icalendar-timezone "UTC+2:00"
        org-icalendar-use-deadline (quote (event-if-todo todo-due))
        org-icalendar-use-scheduled (quote (event-if-todo todo-start))
        org-icalendar-with-timestamps nil
        org-journal-dir "~/Documents/Worklog/"
        org-journal-enable-agenda-integration t
        org-journal-file-format "%Y%m%d"
        org-log-done t
        org-mu4e-link-query-in-headers-mode nil
        org-outline-path-complete-in-steps nil
        org-pretty-entities t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-history nil
        org-refile-targets '((org-agenda-files . (:maxlevel . 4)))
        org-refile-use-outline-path 'file
        org-return-follows-link t
        org-scheduled-delay-days 0
        org-show-notification-handler "notify-send"
        org-startup-folded t
        org-tag-faces (quote (("next" . "red") ("waiting" . "blue")))
        counsel-org-goto-all-outline-path-prefix 'file-name-nondirectory
        )

  (setq org-agenda-current-time-string "┈	┈	┈	┈	┈	┈	┈ now ┈	┈	┈	┈	┈	┈")

  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/Documents/Zettelkasten/Todo.org")
           "* TODO %?\n  %i")
          ("m" "MailTodo" entry (file "~/Documents/Zettelkasten/Todo.org")
          "* TODO %:fromname %a%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
          ("p" "Print" entry (file+headline "~/Documents/Zettelkasten/Personal.org" "Drucken")
           "* TODO %a drucken :@home:@print:%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%a\n")
          ("c" "CustomerIssue" entry (file "~/Documents/Zettelkasten/Todo.org")
           "* TODO issue%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")

          ("a" "Alfaview" entry (file+headline "~/Documents/Zettelkasten/CustomerAlfaview.org" "Arbeitszeiten")
           "* %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n" :jump-to-captured t :clock-in t)
          ("r" "RubyShift" entry (file+headline "~/Documents/Zettelkasten/Projects.org" "RubyShift")
           "* %?\nEntered on %U\n  %i\n  %a")))
  (let* (
         (headline-font      `(:font "Fantasque Sans Mono"))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@headline-font))))
     `(org-level-7 ((t (,@headline ,@headline-font))))
     `(org-level-6 ((t (,@headline ,@headline-font))))
     `(org-level-5 ((t (,@headline ,@headline-font))))
     `(org-level-4 ((t (,@headline ,@headline-font))))
     `(org-level-3 ((t (,@headline ,@headline-font :height 1.1))))
     `(org-level-2 ((t (,@headline ,@headline-font :height 1.2))))
     `(org-level-1 ((t (,@headline ,@headline-font :height 1.3 :background "white smoke"))))
     `(org-document-title ((t (,@headline ,@headline-font :height 1.5 :underline nil))))))

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  (setq org-agenda-time-grid
        (quote
         ((daily require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
          "......" "───────────────")))

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



  (use-package org-super-agenda
    :config (org-super-agenda-mode))



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
  (defun bascht/daily-review ()
    (interactive)
    (persp-switch "@Org")
    (find-file "/home/bascht/Documents/Zettelkasten/Todo.org")
    (goto-char (org-find-exact-headline-in-buffer "Daily Review"))
    (org-narrow-to-subtree)
    (org-cycle-hide-drawers 'all)
    (search-forward "[ ]"))

  (defun bascht/org-agenda ()
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
  (defun bascht/org-agenda-recent-open-loops ()
    (interactive)
    (let ((org-agenda-start-with-log-mode t)
          (org-agenda-span 'day)
          (org-agenda-use-time-grid nil))
      (org-agenda nil "l")
      (beginend-org-agenda-mode-goto-beginning)))

  (defun bascht/sway-org-clock-indicator ()
    (if (org-clocking-p)
        (format "%s @ %s"
                org-clock-heading
                (format-seconds "%h:%m" (* (org-clock-get-clocked-time) 60)))
      (format "🕶 chilling")))

  (add-to-list 'org-global-properties
               '("Effort_ALL". "0:05 0:10 0:15 0:30 1:00 2:00 3:00 4:00"))

                                        ; Via https://www.reddit.com/r/emacs/comments/8kz8dv/tip_how_i_use_orgjournal_to_improve_my/
  (defun bascht/goto-yesterdays-journal ()
    (interactive)
    (find-file (expand-file-name (concat org-journal-dir (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1)))))))

  (defun bascht/alfatraining-clock-in ()
    (interactive)
    (persp-switch "@Org")
    (find-file "/home/bascht/Documents/Zettelkasten/CustomerAlfaview.org")
    (goto-char (org-find-exact-headline-in-buffer "Arbeitszeiten"))
    (goto-char (org-find-exact-headline-in-buffer (format-time-string "%Y-%m")))
    (search-forward "Total")
    (org-table-insert-row)
    (org-insert-time-stamp (org-read-date nil t "+0d 09:30") t)
    (execute-kbd-macro (read-kbd-macro "<tab>"))
    (execute-kbd-macro (read-kbd-macro "<backtab>f:"))
    (org-clock-in)
    (save-buffer))

  (defun bascht/alfatraining-hours-a-day (date)
    (cond
     ((string-match " Thu" date) "8:00")
     ((string-match " Wed" date) "4:00")
     ((string-match " Mon" date) "0:00")
     ((string-match " Tue" date) "8:00")
     (t "0:00")))

  (setq org-agenda-category-icon-alist
        '(("Todo" "~/.icons/emacs/todo-16x16.png" nil nil :ascent center)
          ("Personal" "~/.icons/emacs/person-16x16.png" nil nil :ascent center)
          ("Customer.*" "~/.icons/emacs/customer-16x16.png" nil nil :ascent center)
          ("Freelance.*" "~/.icons/emacs/customer-16x16.png" nil nil :ascent center)
          ("Projects" "~/.icons/emacs/generic-folder-16x16.png" nil nil :ascent center)
          ("\\(ROPrivat\\|ROArbeit\\)" "~/.icons/emacs/calendar-16x16.png" nil nil :ascent center)
          (".*" '(space . (:width (16))))))

                                        ; Save file on every state change
  (add-hook 'org-trigger-hook 'save-buffer)
  (add-hook 'org-clock-in-hook 'save-buffer)
  (add-hook 'org-clock-out-hook 'save-buffer)

  )
