;;; org.el -*- lexical-binding: t; -*-

(after! org
  (setq org-directory "~/Documents/Zettelkasten"
        ;; org-agenda-include-diary t
        ;; org-agenda-inhibit-startup t
        org-agenda-span 1
        org-agenda-start-day nil
        ;; org-agenda-start-with-clockreport-mode t
        ;; org-agenda-sticky nil
        calendar-week-start-day 1
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-default-appointment-duration 30
        org-agenda-dim-blocked-tasks nil
        org-agenda-file-regexp (format-time-string "\\`[^.].*\\.org\\'\\|\\`%Y%m[0-9]+\\'")
        org-agenda-files '("~/Documents/Zettelkasten")
        org-agenda-hide-tags-regexp "presents"
        org-agenda-show-inherited-tags (quote always)
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-tags-column -105
        org-agenda-time-leading-zero t
        org-agenda-use-tag-inheritance nil
        org-agenda-with-colors t
        org-archive-location "%s_archive::"
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t
        org-clock-persist t
        org-clock-persist-query-resume nil
        org-clock-report-include-clocking-task t
        org-clock-sound t
        org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
        org-confirm-babel-evaluate nil
        org-deadline-warning-days 5
        org-default-notes-file "~/Documents/Zettelkasten/refile.org"
        org-ellipsis " ▼ "
        org-habit-completed-glyph ?✰
        org-habit-following-days 3
        org-habit-graph-column 60
        org-habit-show-all-today t
        org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?‖
        org-hide-emphasis-markers t
        org-hide-leading-stars nil
        org-icalendar-alarm-time 120
        org-icalendar-combined-agenda-file "~/Nextcloud/OrgExport/Org.ics"
        org-icalendar-include-todo (quote all)
        org-icalendar-store-UID t
        org-icalendar-timezone "UTC+2:00"
        org-icalendar-use-deadline (quote (event-if-todo todo-due))
        org-icalendar-use-scheduled (quote (event-if-todo todo-start))
        org-icalendar-with-timestamps nil
        org-indent-mode-turns-on-hiding-stars nil
        org-journal-dir "~/Documents/Worklog"
        org-link-elisp-confirm-function nil
        org-log-done t
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
        )

  ;; (setq org-agenda-current-time-string "┈	┈	┈	┈	┈	┈	┈ now ┈	┈	┈	┈	┈	┈")

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

  (custom-theme-set-faces
   'user
   `(org-level-3        ((t (:height 1.1 :weight light))))
   `(org-level-2        ((t (:height 1.2 :weight light))))
   `(org-level-1        ((t (:height 1.3 :weight normal))))
   `(org-document-title ((t (:height 1.5 :underline nil)))))

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  ;; (setq org-agenda-time-grid
  ;;       (quote
  ;;        ((daily require-timed)
  ;;         (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
  ;;         "......" "───────────────")))

  (setq org-modules
        (quote (org-habit
                org-checklist
                org-mouse)))


  (use-package! org-super-agenda

    :init
    (setq! org-super-agenda-header-map (make-sparse-keymap))

    :config
    (org-super-agenda-mode)
    (setq! org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:name "Today"  ; Optionally specify section name
                :time-grid t  ; Items that appear on the time grid
                :todo "TODAY")  ; Items that have this TODO keyword
         (:name "Personal"
                ;; Single arguments given alone
                :category "Personal")
         (:name "Alfatraining"
          ;; Multiple args given in list with implicit OR
          :category "CustomerAlfaview")

         (:name "Projects"
          ;; Multiple args given in list with implicit OR
          :todo "PROJ")
         ;; Set order of multiple groups at once
         (:order-multi (2 (:name "Shopping in town"
                                 ;; Boolean AND group matches items that match all subgroups
                                 :and (:tag "shopping" :tag "@town"))
                          (:name "Alfatraining"
                                 ;; Multiple args given in list with implicit OR
                                 :category "CustomerAlfatraining")
                          (:name "Food-related"
                                 ;; Multiple args given in list with implicit OR
                                 :tag ("food" "dinner"))
                          (:name "Personal"
                                 :habit t
                                 :tag "personal")
                          (:name "Space-related (non-moon-or-planet-related)"
                                 ;; Regexps match case-insensitively on the entire entry
                                 :and (:regexp ("space" "NASA")
                                               ;; Boolean NOT also has implicit OR between selectors
                                               :not (:regexp "moon" :tag "planet")))))
         ;; Groups supply their own section names when none are given
         (:todo "WAITING" :order 8)  ; Set order of this section
         (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                ;; Show this group at the end of the agenda (since it has the
                ;; highest number). If you specified this group last, items
                ;; with these todo keywords that e.g. have priority A would be
                ;; displayed in that group instead, because items are grouped
                ;; out in the order the groups are listed.
                :order 9)
         (:priority<= "B"
                      ;; Show this section after "Today" and "Important", because
                      ;; their order is unspecified, defaulting to 0. Sections
                      ;; are displayed lowest-number-first.
                      :order 1)
         ;; After the last group, the agenda will display items that didn't
         ;; match any of these groups, with the default order position of 99
         ))

    )

    (add-hook! 'org-roam-mode-hook
               (setq company-idle-delay 0.2))

  (setq org-agenda-custom-commands
        '(("l" "Open loops"
           ((agenda ""))
           ((org-agenda-start-day "-1d")
            ;; (org-agenda-span 'week)
            (org-super-agenda-groups nil)
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
                         (:discard (:anything t)))))))
           ((org-agenda-span 1))
           )
          ;; ("a" "Mega Agenda" agenda
          ;;  (org-super-agenda-mode)
          ;;  ((org-super-agenda-groups
          ;;    '(
          ;;      (:name "Critical Now" :priority "A")
          ;;      (:name "Opportunity Now" :priority "B")
          ;;      (:name "Over the horizon" :priority "C")
          ;;      )
          ;;    )
          ;;   ))
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
                 :order 1)))))
           ))

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
    (let ((start-time-stamp
           (if (time-less-p (org-read-date nil t "+0d 07:30") (current-time))
               (current-time)
             (org-read-date nil t "+0d 09:30"))))
      (org-insert-time-stamp start-time-stamp t))
    (execute-kbd-macro (read-kbd-macro "<tab>"))
    (org-insert-time-stamp (org-read-date nil t "+0d 17:30") t)
    (execute-kbd-macro (read-kbd-macro "<backtab>f:"))
    (org-clock-in)
    (save-buffer))

  ;; Export current clock subtree to Personio
  (defun bascht/org-clock-to-personio (month)
    (interactive (list (read-string "Monat?" (format-time-string "%Y-%m"))))
    (when (org-clock-is-active)
      (org-clock-out))
    (evil-window-left 0)
    (persp-switch "@Org")
    (find-file "/home/bascht/Documents/Zettelkasten/CustomerAlfaview.org")
    (goto-char (org-find-exact-headline-in-buffer "Arbeitszeiten"))
    (goto-char (org-find-exact-headline-in-buffer month))
    (search-forward ":LOGBOOK:")
    (save-excursion
      (forward-line 1)
      (cl-loop
       until (s-equals? (s-trim (thing-at-point 'line t)) ":END:")
       do (bascht/format-clock-line-as-csv (org-element-at-point))
       (forward-line 1))))

(defun bascht/format-clock-line-as-csv (clock-line)
  (let* ((timestamp (org-element-property :value clock-line))
         (ts-start (org-timestamp-to-time timestamp))
         (ts-end (org-timestamp-to-time timestamp t)))

    (message (format "go-personio --work-start \"%s\" --work-end \"%s\""
                     (format-time-string "%F %a %R %Z" ts-start)
                     (format-time-string "%F %a %R %Z" ts-end)))))

  ;; Temporary test-function to re-trigger the export and get the logs
  (defun bascht/test-org-clock-to-personio ()
    (interactive)
    (evil-window-left 1)
    (bascht/org-clock-to-personio)
    (evil-window-right 1))

  (defun bascht/alfatraining-hours-a-day (date)
    (cond
     ((string-match " Tue" date) "8:00")
     ((string-match " Wed" date) "8:00")
     ((string-match " Thu" date) "8:00")
     (t "0:00")))

  (defun bascht/org-file-show-headings (org-file)
    (interactive)
    (find-file (expand-file-name org-file))
    (counsel-org-goto)
    (org-reveal)
    (org-fold-show-subtree))

  (defun bascht/wzzk-find ()
    (interactive)
    (projectile-find-file-in-directory bascht/wzzk))

  (defun bascht/wzzk-find-today ()
    (interactive)
    (let ((journal-today (expand-file-name (format-time-string "journals/%Y-%m-%d.md") bascht/wzzk)))
      (unless (file-exists-p journal-today)
        (message (concat "No journal for today, creating " (concat journal-today "on the fly.")))
        (copy-file (expand-file-name "journals/_template.md" bascht/wzzk) journal-today))
      (find-file journal-today))
    (beginning-of-buffer)
    (replace-string "<% tp.file.creation_date() %>" (format-time-string "%Y-%m-%d %H:%m"))
    (replace-string "DailyNote <% tp.file.title.split('-')[0] %>" (format-time-string "DailyNote %Y"))
    (end-of-buffer))

  (defun bascht/wzzk-find-yesterday ()
    (interactive)
    (find-file (expand-file-name
                (format-time-string "%Y-%m-%d.md"
                                    (time-subtract (current-time) (days-to-time 1))) "~/WirZwei/Zettelkasten/journals")))

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
  (add-hook 'org-clock-out-hook 'save-buffer))

(use-package! org-roam
  :init
  (setq org-roam-directory (file-truename "~/WirZwei/ZettelkastenOrg")
        org-roam-completion-everywhere t
        org-roam-capture-templates
         '(("d" "default" plain
            "%?"
            :if-new (file+head "${title}.org" "#+title: ${title}\n")
            :unnarrowed t))
        org-id-link-to-org-use-id 'create-if-interactive)
  :config
  (org-roam-db-autosync-mode +1)
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode))

(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :init
  :after org
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))
