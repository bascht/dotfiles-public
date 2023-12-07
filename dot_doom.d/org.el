;;; org.el -*- lexical-binding: t; -*-

(after! org
  (setq
        calendar-week-start-day 1
        org-agenda-columns-add-appointments-to-effort-sum t
        org-agenda-default-appointment-duration 30
        org-agenda-file-regexp (format-time-string "\\`[^.].*\\.org\\'\\|\\`%Y%m[0-9]+\\'")
        org-agenda-hide-tags-regexp "presents"
        org-agenda-include-diary nil
        org-agenda-ndays 1
        org-agenda-start-day nil
        org-agenda-show-inherited-tags (quote always)
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-scheduled-if-done t
        org-agenda-span 1
        org-agenda-start-with-clockreport-mode nil
        org-agenda-sticky nil
        org-agenda-tags-column -105
        org-agenda-time-leading-zero t
        org-agenda-with-colors t
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup t
        org-agenda-use-tag-inheritance nil
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
        org-duration-format (quote (("h" . t) (special . 2)))
        org-link-elisp-confirm-function nil
        org-deadline-warning-days 5
        org-default-notes-file "~/Documents/Zettelkasten/refile.org"
        org-ellipsis " ⧩ "
        org-habit-following-days 3
        org-habit-graph-column 60
        org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?‖
        org-habit-completed-glyph ?✰
        org-habit-show-all-today t
        org-hide-emphasis-markers t
        org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil
        org-icalendar-alarm-time 120
        org-icalendar-combined-agenda-file "~/Nextcloud/OrgExport/Org.ics"
        org-icalendar-include-todo (quote all)
        org-icalendar-store-UID t
        org-icalendar-timezone "UTC+2:00"
        org-icalendar-use-deadline (quote (event-if-todo todo-due))
        org-icalendar-use-scheduled (quote (event-if-todo todo-start))
        org-icalendar-with-timestamps nil
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
        +org-capture-emails-file "Todo.org"
        org-show-notification-handler "notify-send"
        org-startup-folded t
        org-tag-faces (quote (("next" . "red") ("waiting" . "blue")))
        counsel-org-goto-all-outline-path-prefix 'file-name-nondirectory
        )

  (setq org-directory (expand-file-name (if (string= (system-name) "apfelstrudel")
                                            "~/Work/Zettelkasten"
                                          "~/Documents/Zettelkasten")))

  (setq org-agenda-files (list org-directory))

  (setq org-agenda-current-time-string "	┈ now ┈	")



  (setq org-capture-templates
        (if (string= (system-name) "apfelstrudel")
            '(("t" "Todo" entry (file "Todo.org")
               "* TODO %?\n  %i")
              ("m" "MailTodo" entry (file "Todo.org")
               "* TODO %:fromname %a%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
              ("p" "OpsRetro positive" entry (file+olp "Alfaview.org" "Meetings" "Ops-Retro") "* TODO :+1: %?\n  %i")
              ("n" "OpsRetro negative" entry (file+olp "Alfaview.org" "Meetings" "Ops-Retro") "* TODO :-1: %?\n  %i"))
          '(("t" "Todo" entry (file "Todo.org")
             "* TODO %?\n  %i")
            ("m" "MailTodo" entry (file "Todo.org")
             "* TODO %:fromname %a%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%i\n")
            ("p" "Print" entry (file+headline "Personal.org" "Drucken")
             "* TODO %a drucken :@home:@print:%?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%a\n")
            ("b" "BlogIdeas" entry (file+olp "Projects.org" "bascht.com" "BlogIdeas")
             "* TODO %?\n  %i"))))

  (custom-theme-set-faces
   'user
   `(org-level-3        ((t (:height 1.1 :weight light))))
   `(org-level-2        ((t (:height 1.2 :weight light))))
   `(org-level-1        ((t (:height 1.3 :weight normal))))
   `(org-document-title ((t (:height 1.5 :underline nil)))))

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  (setq org-agenda-time-grid
        (quote
         ((daily require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000)
          "......" "───────────────")))

  (setq org-modules
        (quote (org-habit
                org-checklist
                org-mouse)))

  (setq org-agenda-custom-commands
        '(("l" "Open loops"
           ((agenda ""))
           ((org-agenda-start-day "-1d")
            ;; (org-agenda-span 'week)
            (org-agenda-show-log nil)
            (org-agenda-ndays 2)
            (tags-todo "-imported")
            (org-agenda-start-with-clockreport-mode nil)
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
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
            (todo "" ((org-agenda-overriding-header "Projects")
                      (org-super-agenda-groups
                       '((:name none  ; Disable super group header
                          :children todo)
                         (:discard (:anything t))))))))
          ("P" agenda "Printable"
           ((ps-number-of-columns 2)
            (ps-landscape-mode t)
            (org-agenda-prefix-format " - [ ] ")
            (org-agenda-with-colors t)
            (org-agenda-remove-tags t)
            (org-agenda-add-entry-text-maxlines 2)
            (htmlize-output-type 'css))
           (concat (file-name-as-directory (getenv "XDG_RUNTIME_DIR")) "org/agenda.html"))))

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
                (format "%2f h" (/ (org-clock-get-clocked-time) 60.0)))
      (format "🕶 chilling")))

  (add-to-list 'org-global-properties
               '("Effort_ALL". "0:05 0:10 0:15 0:30 1:00 2:00 3:00 4:00"))

                                        ; Via https://www.reddit.com/r/emacs/comments/8kz8dv/tip_how_i_use_orgjournal_to_improve_my/
  (defun bascht/goto-yesterdays-journal ()
    (interactive)
    (find-file (expand-file-name (concat org-journal-dir (format-time-string "%Y%m%d" (time-subtract (current-time) (days-to-time 1)))))))

  ;; Find my work tree and clock into the respective day / create entry if it does not exist
  (defun bascht/alfatraining-clock-in ()
    (interactive)
    (save-excursion

      (find-file (expand-file-name "Alfaview.org" org-directory))
      (goto-char (point-min))

      (let ((month (format-time-string "%Y-%m"))
            (today (format-time-string "[%Y-%m-%d]")))
        (goto-char (org-find-exact-headline-in-buffer month))

        (if (integer-or-marker-p (org-find-exact-headline-in-buffer today))
            (goto-char (org-find-exact-headline-in-buffer today))
          (progn
            (org-insert-subheading nil)
            (insert today)))
        (org-clock-in)
        (save-buffer))))

  ;; Add the a small button to report the current clock line in a :clockreport to
  ;; Personio. This won't actually do the remote call, but just add a elisp: hyperlink.
  ;; That way we keep the table rendering nice and quick.
  ;; Skip the first 3 total / sum rows by matching the current item against =org-ts-regexp0=
  (defun bascht/ts-for-report-table (ts h)
    (save-excursion
      (if (string-match org-ts-regexp0 ts)
          (progn
            (beginning-of-buffer)
            (goto-char (org-find-exact-headline-in-buffer ts))
            (if (org-entry-get nil "PERSONIO_REPORTED")
                "✓ Reported"
              (progn
                (search-forward ":LOGBOOK:")
                (search-forward ":END:")
                (forward-line -1)
                (let* ((timestamp (org-element-property :value (org-element-at-point)))
                       (ts-start (org-timestamp-to-time timestamp))
                       (ts-end (org-timestamp-to-time timestamp t))
                       (hours (s-chop-suffix "h" h)))

                  (format "[[elisp:(bascht/ts-report-to-personio '%s %s)][Report]]" ts-start hours))
                )))
        "")))

  ; Split working hours into personio-compatible slots around a 1 hour break in
  ; case the report is longer than 4 hours. There are probably a 1000 easier
  ; ways to do this, but I'mma take proud in my hacky solution since it works
  ; and it was hard enough to cobble together.
  (defun bascht/ts-report-to-personio (ts hours)
    (point-to-register 'bascht/ts-report-current-table)
    (async-start
     (lambda ()
         (if (> hours 4)
             (let* ((first-shift-end (time-add ts (seconds-to-time (* 3600 4))))
                    (break-end (time-add first-shift-end 3600))
                    (shift-end (time-add break-end (seconds-to-time (* 3600 (- hours 4))))))

               (shell-command (format "report-to-personio \"%s\" \"%s\" \"%s\" \"%s\""
                                                (format-time-string "%R" ts)
                                                (format-time-string "%R" first-shift-end)
                                                (format-time-string "%R" break-end)
                                                (format-time-string "%R" shift-end)))
               )

           (let* ((shift-end (time-add ts (seconds-to-time (* hours 3600)))))
             (shell-command (format "report-to-personio \"%s\" \"%s\""
                                    (format-time-string "%R" ts)
                                    (format-time-string "%R" shift-end)))))

       (list (format-time-string "[%F]" ts) (format "Successfully reported %s hours to Personio" hours)))

     (lambda (result)
       (jump-to-register 'bascht/ts-report-current-table)
       (save-excursion
         (beginning-of-buffer)
         (goto-char (org-find-exact-headline-in-buffer (car-safe result)))
         (org-set-property "PERSONIO_REPORTED" "t"))
       (message (car-safe (cdr-safe result)))
       (org-table-recalculate))))

  (defun bascht/alfatraining-hours-a-day (date)
    (cond
     ((string-match "Mon" (s-trim date)) 4)
     ((string-match "Tue" (s-trim date)) 8)
     ((string-match "Wed" (s-trim date)) 8)
     ((string-match "Thu" (s-trim date)) 8)
     ((string-match "Fri" (s-trim date)) 2)
     (t "0:00")))

  (defun bascht/send-org-clock-to-ulanzi ()
    (if (org-clocking-p)
        (let* ((clocked-time (/ (org-clock-get-clocked-time) 60.0))
               (percent (round (*(/ clocked-time (bascht/alfatraining-hours-a-day (format-time-string "%a"))) 100)))
               (text (format "%.1fh" clocked-time)))

          (request "http://10.11.12.180/api/custom"
            :type "POST"
            :headers '(("Content-Type" . "application/json"))
            :data (json-encode `(("name" . "worklog")
                                 ("icon" . "11402")
                                 ("progress" . ,percent)
                                 ("text" . ,text))))
          )

      ))

  (defun bascht/org-file-show-headings (org-file)
    (interactive)
    (find-file (expand-file-name org-file))
    (counsel-org-goto)
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


(defun +mu4e/capture-msg-to-agenda (arg)
  "Refile a message and add a entry in `+org-capture-emails-file' with a
deadline.  Default deadline is today.  With one prefix, deadline
is tomorrow.  With two prefixes, select the deadline."
  (interactive "p")
  (let ((sec "^* Email")
        (msg (mu4e-message-at-point)))
    (when msg
      ;; put the message in the agenda
      (with-current-buffer (find-file-noselect
                            (expand-file-name +org-capture-emails-file org-directory))
        (save-excursion
          ;; find header section
          (goto-char (point-max))
            (let (org-M-RET-may-split-line
                  (lev (org-outline-level))
                  (folded-p (invisible-p (point-at-eol)))
                  (from (plist-get msg :from)))
              (when (consp (car from)) ; Occurs when using mu4e 1.8+.
                (setq from (car from)))
              ;;
              ;; place the subheader
              (org-end-of-meta-data) ; skip property drawer
              (org-insert-todo-heading 1)        ; insert a todo heading
              ;;
              ;; insert message and add deadline
              (insert (concat "Email beantworten: "
                              "[[mu4e:msgid:"
                              (plist-get msg :message-id) "]["
                              (truncate-string-to-width
                               (plist-get from :name) 25 nil nil t)
                              " - "
                              (truncate-string-to-width
                               (plist-get msg :subject) 40 nil nil t)
                              "]] :email:"))
              (org-deadline nil
                            (cond ((= arg 1) (format-time-string "%Y-%m-%d"))
                                  ((= arg 4) "+1d")))

              (org-update-parent-todo-statistics)

              ;; refold as necessary
              (if folded-p
                  (progn
                    (org-up-heading-safe)
                    (hide-subtree))
                (hide-entry)))))
      ;; refile the message and update
      (cond ((eq major-mode 'mu4e-view-mode)
             (mu4e-view-mark-for-refile))
            ((eq major-mode 'mu4e-headers-mode)
             (mu4e-headers-mark-for-refile)))
      (message "Refiled \"%s\" and added to the agenda for %s"
               (truncate-string-to-width
                (plist-get msg :subject) 40 nil nil t)
               (cond ((= arg 1) "today")
                     ((= arg 4) "tomorrow")
                     (t         "later"))))))

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

(use-package! org-journal
  :after (org)
  :init
  (if (string= (getenv "EMACS_SERVER_NAME") "workmacs")
      (progn
        (run-hooks 'doom-first-input-hook)
        (setq writeroom-width 90)
        (spacious-padding-mode)
        (global-hide-mode-line-mode)
        (ef-themes-select 'ef-cyprus)
        (add-hook 'org-journal-mode-hook
                  (lambda () (add-hook 'after-save-hook 'delete-frame)))
        (setq server-client-instructions nil) ;; hide noisy minibuffer
        (org-journal-open-current-journal-file)))
  :config
  (setq org-journal-dir "~/Documents/Worklog/"
        org-journal-enable-agenda-integration nil
        org-journal-file-format "%Y%m%d"
        org-journal-date-format "%A, %d/%m/%Y"
        org-journal-carryover-items nil))

(use-package! khalel
  :after org
  :config
  (khalel-add-capture-template))

(use-package! org-habit
  :init
  (setq
   org-habit-graph-column 1
   org-habit-preceding-days 10
   org-habit-following-days 1
   org-habit-show-habits-only-for-today nil))
