;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Determine if we're in comacs as early as possible
;; even if 'server-name is not set yet
(defun bascht/is-comacs()
  (string= (getenv "EMACS_SERVER_NAME") "comacs"))

(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "VictorMono Nerd Font")
      doom-big-font (font-spec :family "JetBrains Mono" :size 24)
      doom-serif-font (font-spec :family "IBM Plex Mono")
      doom-symbol-font (font-spec :family "Joypixels")
      doom-theme (if (bascht/is-comacs) 'doom-one-light 'ef-light)
      doom-scratch-dir (concat user-emacs-directory "scratch/" (or (getenv "EMACS_SERVER_NAME")  "default"))
      ef-themes-mixed-fonts t
      emojify-display-style 'unicode
      modus-themes-variable-pitch-ui t
      modus-themes-rainbow-headings t
      modus-themes-section-headings t
      modus-themes-scale-headings t
      company-idle-delay nil
      bascht/wzzk "~/WirZwei/Zettelkasten"
      bascht/wzzk-journals "~/WirZwei/Zettelkasten/journals"
      display-line-numbers-type nil
      delete-selection-mode nil
      writeroom-width 100
      ispell-dictionary "en_GB"
      ispell-aspell-data-dir "/etc/profiles/per-user/bascht/lib/aspell"
      ispell-aspell-dict-dir ispell-aspell-data-dir
      ruby-insert-encoding-magic-comment nil
      safe-local-variable-values '((buffer-read-only . 1))
      git-gutter-fr+-side (quote left-fringe)
      helm-org-rifle-show-path t
      frame-title-format (concat "%b - " user-login-name "@" (system-name))
      browse-url-browser-function 'browse-url-xdg-open browse-url-generic-program "browser"
      hl-todo-keyword-faces
      `(("TODO" warning bold)
        ("FIXME" error bold)
        ("HACK" font-lock-constant-face bold)
        ("REVIEW" font-lock-keyword-face bold)
        ("NOTE" success bold)
        ("DEPRECATED" font-lock-doc-face bold)
        ("REFACTOR" font-lock-comment-face)
        ("STYLE" ,(doom-color 'yellow))
        ("BUG" error bold)
        ("MAYBE" warning bold))
      )

(setq org-directory (if (string= (system-name) "apfelstrudel")
                        "~/Work/Zettelkasten/"
                      "~/Documents/Zettelkasten/"))


(map! :leader
      :desc "Copy URL link"
      "o U" #'link-hint-copy-link)
(map! :leader
      :desc "Open URL link"
      "o u" #'link-hint-open-link)


(setq doom-modeline-time nil)
(setq doom-modeline-persp-name t)
(setq doom-modeline-modal nil)

(nerd-icons-dired-mode -1)
(setq nerd-icons-font-family  "Symbols Nerd Font Mono")
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))


(custom-set-faces!
  '(mode-line :family  "VictorMono Nerd Font" :weight medium)
  '(mode-line-active :family "VictorMono Nerd Font" :weight medium)
  '(mode-line-inactive :family "VictorMono Nerd Font" :weight medium)
  '(avy-lead-face :inherit isearch :family doom-font :bold t :italic nil :background "deep pink" :height 1.0 :foreground "snow")
  )

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner '("""┻━┻ ︵ ¯\\(ツ)/¯ ︵ ┻━┻"""))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(map! :leader
      :desc "Org capture"    "SPC" #'org-capture
      :desc "Quick ace window" "w SPC" #'ace-window
      (:prefix-map ("l" . "bascht/personal")
       :desc "Start my daily review"  "d" #'bascht/daily-review
       :desc "Run table formatter"    "tf" #'org-table-calc-current-TBLFM

       (:prefix-map ("c" . "clocks")
        :desc "Clock in alfatraining" "a" #'bascht/alfatraining-clock-in
        :desc "Goto in recent clock"  "r" #'org-mru-clock-select-recent-task))

      (:prefix-map ("nw" . "WirZwei")
       :desc "Open todays wzzk"       "t" #'bascht/wzzk-find-today
       :desc "Open yesterdays wzzk"   "y" #'bascht/wzzk-find-yesterday
       :desc "Find file in wzzk"      "f" #'bascht/wzzk-find)

      :desc "Open file via zoxide"    "fz" #'zoxide-find-file

      :desc "Open yesterdays journal" "njy" #'bascht/goto-yesterdays-journal
      :desc "Find in Alfaview"        "nga" (lambda () (interactive) (bascht/org-file-show-headings "~/Documents/Zettelkasten/CustomerAlfaview.org"))
      :desc "Find in Knowledgebase"   "ngk" (lambda () (interactive) (bascht/org-file-show-headings "~/Documents/Zettelkasten/KnowledgeBase.org")))

(map! :after org
      :map org-mode-map
      :localleader

      (:prefix-map ("i" . "Insert")
       :desc "Link/Image"           "l" 'org-insert-link
       :desc "Item"                 "o" 'org-toggle-item
       :desc "Citation"             "c" 'org-ref-helm-insert-cite-link
       :desc "Footnote"             "f" 'org-footnote-action
       :desc "Table"                "t" 'org-table-create-or-convert-from-region
       :desc "Clipboard"            "c" 'org-download-clipboard
       :desc "Structure template"   "s" #'org-insert-structure-template
       :desc "Heading (respecting)" "h" #'org-insert-heading-respect-content
       :desc "TODO heading"         "t" #'org-insert-heading-respect-content))

(map! :after dired
      :map dired-mode-map
      :localleader

      :desc "Drag" "d" #'dwim-shell-command-drag
      :desc "Drop" "o" #'dwim-shell-command-drop
      :desc "Convert to GIF" "g" #'dwim-shell-command-convert-to-gif)

;; Remap C-Return in org-journal mode since I don't need any other
;; kind of headlines in org-journal files
(map! :after org-journal
      :map org-journal-mode-map
      :desc "Insert new Journal entry" "C-j" #'org-journal-new-entry
      :desc "Insert new Journal entry" "C-RET" #'org-journal-new-entry)

(map! :after markdown-mode
      :map evil-markdown-mode-map
      :i "M-b" #'backward-word
      )

(map! :after mu4e
      :map mu4e-view-mode-map :vn
      "T" (lambda () (interactive) (mu4e-view-mark-thread '(refile))))

(map! :after mu4e
      :map mu4e-headers-mode-map :vn
      "T" (lambda () (interactive) (mu4e-headers-mark-thread nil '(refile))))

(map! :after mu4e
      :map (mu4e-headers-mode-map mu4e-view-mode-map) :vn
      :desc "Back to last search" "<backspace>" #'mu4e-search-prev
      :desc "Forward to next search" "S-<backspace>" #'mu4e-search-next
      )

(map! :after mu4e
      :map (mu4e-headers-mode-map mu4e-view-mode-map)
      :localleader
      "g" (lambda () (interactive) (save-excursion
                                     (progn (goto-char (point-min))
                                            (search-forward "view it on GitLab")
                                            (backward-word)
                                            (shr-browse-url))))
      )

;; https://micro.rousette.org.uk/2021/01/03/a-useful-binding.html
(map! (:map 'override
       :v "v" #'er/expand-region
       :v "V" #'er/contract-region))

                                        ; Define quick helper switches to switch between languages while
                                        ; keeping distinct personal dictionaries for both of them
(defun bascht/switch-spellcheck (lang)
  (interactive)
  (setq ispell-personal-dictionary (concat "~/.local/share/ispell/bascht_" lang ".pws"))
  (ispell-change-dictionary lang) lang)

(defun bascht/switch-spellcheck-to-english ()
  (interactive)
  (bascht/switch-spellcheck "en_GB")
  (spell-fu-mode))

(defun bascht/switch-spellcheck-to-german ()
  (interactive)
  (bascht/switch-spellcheck "de_DE")
  (spell-fu-mode))
(use-package! zoxide :defer t)

(use-package! dwim-shell-command
  :defer t
  :init

  (defun dwim-shell-command-convert-to-gif ()
    "Convert file to a small GIF"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Converting <<f>> to GIF"
     "convert-to-gif <<f>>"
     :utils "convert-to-gif"
     ))

  (defun dwim-shell-command-drop ()
    "Drop stuff over to drop.bascht.space"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Drop to drop.bascht.space"
     "drop <<f>>"
     :utils "drop"
     :silent-success t
     ))

  (defun dwim-shell-command-drag ()
    "Drag stuff via dragon"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Drag files somewhere"
     "dragon --and-exit <<f>>"
     :utils "dragon"
     :silent-success t
     )))

(after! modus-themes
  (setq modus-themes-syntax '(faint green-strings yellow-comments yellow-alt-syntax)
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-fringes 'subtle
        modus-themes-hl-line '(accented intense)
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold intense)
        modus-themes-region '(bg-only)
        modus-themes-org-blocks 'tinted-background
        modus-themes-mode-line '(accented borderless (padding . 1))
        modus-themes-headings '((1 . (rainbow overline background 1.4))
                                (2 . (rainbow background 1.3))
                                (3 . (rainbow bold 1.2))
                                (t . (semilight 1.1)))))

(after! solaire-mode
  (solaire-global-mode -1))

(after! writeroom-mode
  (setq +zen-text-scale 0.8))

                                        ; Don't add #0 #1 #2 #3… workspaces :D
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

                                        ; Directly create a matching workspace for the project (when launched with `bin/tn')
(defun bascht/switch-to-or-load-workspace (name &optional directory)
  (interactive)
  (persp-mode)
  (if (+workspace-exists-p name)
      (+workspace-switch name)
    (progn (+workspace-new name)
           (+workspace-switch name)
           (magit-status-setup-buffer))))

(defun bascht/move-to-scratchpad()
  (shell-command "swaymsg move scratchpad"))

(defun bascht/worklog ()
  "Switch to my worklog workspace and append a new log"
  (interactive)
  (org-set-frame-title "Worklog")
  (org-journal-new-entry nil)
  (setq indicate-empty-lines nil)
  (evil-append nil))

(defun bascht/mu4e-change-from-to-catchall (msg)
  "Set the From address based on the To address of the original message if I reply."
  (setq user-mail-address
        (if (and msg (mu4e-message-contact-field-matches msg :to "bascht.com"))
            (plist-get (car-safe (mu4e-message-field msg :to)) :email)
          (cdr-safe (assoc 'user-mail-address (mu4e-context-vars (mu4e-context-current)))))))

(after! flycheck
  (flycheck-define-checker vale
    "A checker for prose"
    :command ("vale" "--output" "line" source)
    :standard-input nil
    :error-patterns
    ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end)) :modes (markdown-mode markdown-mode gfm-mode org-mode text-mode)))

(defun bascht/check-with-vale ()
  (add-to-list 'flycheck-checkers 'vale 'append))

(after! mu4e
  (setq
   org-msg-signature (with-current-buffer (find-file-noselect "/home/bascht/.signature") (buffer-string))
   mu4e-change-filenames-when-moving t
   message-send-mail-function 'message-send-mail-with-sendmail
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-sendmail-f-is-evil 't
   mu4e-update-interval 600
   mu4e-compose-complete-only-after "2016-01-01"
   mu4e-compose-dont-reply-to-self 't
   mu4e-compose-format-flowed t
   mu4e-compose-in-new-frame t
   mu4e-get-mail-command "mbsync -a"
   mu4e-search-include-related t
   mu4e-index-lazy-check nil
   mu4e-index-cleanup t
   mu4e-use-fancy-chars nil
   sendmail-program "msmtp"

   mu4e-headers-draft-mark     '("D" . "💈")
   mu4e-headers-flagged-mark   '("F" . "📍")
   mu4e-headers-new-mark       '("N" . "🆕")
   mu4e-headers-passed-mark    '("P" . "➡")
   mu4e-headers-replied-mark   '("R" . "↩")
   mu4e-headers-seen-mark      '("S" . "☑")
   mu4e-headers-trashed-mark   '("T" . "💀")
   mu4e-headers-attach-mark    '("a" . "📎")
   mu4e-headers-encrypted-mark '("x" . "🔒")
   mu4e-headers-signed-mark    '("s" . "🔑")
   mu4e-headers-unread-mark    '("u" . "✉")
   mu4e-headers-list-mark      '("l" . "")
   mu4e-headers-personal-mark  '("p" . "")
   mu4e-headers-calendar-mark  '("c" . "📅")
   mu4e-headers-fields '((:human-date . 6)
                         (:flags . 4)
                         (:account-stripe . 2)
                         (:from-or-to . 25)
                         ;; (:recipnum . 3)
                         ;; (:list . 20)
                         (:subject . 100))
   +mu4e-min-header-frame-width 142
   mu4e-headers-date-format "%d.%m."
   mu4e-headers-time-format "%H:%M"
   mu4e-search-results-limit 1500
   mu4e-headers-visible-lines 15
   mu4e-headers-visible-columns 135
   mu4e-use-fancy-chars t
   mu4e-index-cleanup t)
  )

  ;; TODO: Until mu 1.10 ~ names are replaced in upstream doom
  ;; (defalias '+mu4e~main-action-str-prettier-mu4e '+a--main-action-str-prettier-a)
  ;; (defalias '+mu4e~main-keyval-str-prettier-mu4e '+a--main-keyval-str-prettier-a)
  (defalias 'mu4e~view-open-file 'mu4e--view-open-file)
  (defalias 'mu4e~view-gather-mime-parts 'mu4e--view-gather-mime-parts)
  (defalias 'mu4e~view-mime-part-to-temp-file 'mu4e--view-mime-part-to-temp-file))

                                        ; Disable spell-fu-mode globally
(remove-hook 'text-mode-hook #'spell-fu-mode)

(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))

                                        ; Always open new project with dired
(defun bascht/projectile-open-dired (dir)
  (let ((default-directory (file-truename (expand-file-name dir))))
    (dirvish)))

(setq +workspaces-switch-project-function #'bascht/projectile-open-dired)

(add-hook! 'yaml-mode-hook
  (setq auto-fill-mode -1)
  (flycheck-select-checker 'yaml-yamllint))

(add-hook! 'git-commit-mode-hook
  (end-of-line)
  (spell-fu-mode)
  (bascht/switch-spellcheck "en_GB")
  (evil-insert-state))

(add-hook! 'org-capture-mode-hook
  (bascht/switch-spellcheck "de_DE")
  (evil-insert-state))

(add-hook! 'mu4e-compose-pre-hook
  (bascht/mu4e-change-from-to-catchall mu4e-compose-parent-message)
  (spell-fu-mode)
  (bascht/switch-spellcheck "de_DE")
  (evil-insert-state))

(add-hook! 'mu4e-view-mode-hook (variable-pitch-mode))

(setq +format-on-save-enabled-modules '(terraform-mode
                                        go-mode))

(use-package! markdown-mode
  :defer t
  :init
  (setq markdown-enable-wiki-links t
        markdown-wiki-link-search-type '(sub-directories parent-directories)
        markdown-wiki-link-fontify-missing t
        markdown-enable-math nil
        markdown-link-space-sub-char " "))


(after! magit
  (magit-wip-mode)
  (setq magit-log-arguments '("--graph" "--decorate" "--color")
        magit-delete-by-moving-to-trash nil
        git-commit-summary-max-length 80
        transient-values '((magit-rebase "--autosquash" "--autostash")
                           (magit-pull "--rebase" "--autostash")
                           (magit-revert "--autostash"))))
(defun bascht/file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

(add-hook 'prog-mode-hook (lambda () (setq company-idle-delay 0.2)))

(use-package! dictcc :defer t)
(after! dap
  (unless (display-graphic-p)
    (set-face-background 'dap-ui-marker-face "orange")
    (set-face-attribute 'dap-ui-marker-face nil :inherit nil)
    (set-face-background 'dap-ui-pending-breakpoint-face "lightpink")
    (set-face-attribute 'dap-ui-verified-breakpoint-face nil :inherit 'dap-ui-pending-breakpoint-face)))

(use-package! ef-themes)
(use-package! apropospriate-theme :defer t)
(use-package! khalel :defer t)
(use-package! obsidian
  :defer t
  :config
  (obsidian-specify-path "~/WirZwei/Zettelkasten"))

(use-package! org-alert
  :defer t
  :config
  (setq
   alert-default-style 'libnotify
   org-alert-notify-cutoff 15
   org-alert-notify-after-event-cutoff 15)
  (org-alert-enable))

(use-package! spacious-padding)

(after! markdown-mode
  (set-company-backend! 'markdown-mode '(:separate obsidian-tags-backend company-capf company-dabbrev company-yasnippet company-ispell)))

(add-hook! markdown-mode-hook 'spell-fu-mode)

(use-package! dirvish
  :config
  (dirvish-override-dired-mode)
  (setq dirvish-attributes '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size))
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dirvish-default-layout (list 0 0.4))
  (setq dirvish-hide-details t))

(map! :after dired
      :map dirvish-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-file)

(defun bascht/dirvish-tdir()
  (interactive)
  (find-file
   (string-trim (shell-command-to-string "mktemp -d"))))

(defun bascht/checkout-mr-after-creation ()
  (interactive)
  (magit-fetch)
  (sleep-for 5)
  (let* ((pullreq (forge-read-pullreq "Checkout pull request" t)))
    (magit-checkout (forge--branch-pullreq (forge-get-pullreq pullreq)))))

(defun bascht/create-mr-from-issue ()
  ;; Create a new GitLab merge request and re-use the issue title as the branch name and copy all labels
  (interactive)
  (let* ((issue (forge-read-issue "View issue" t)))
    (forge-visit (forge-get-issue issue))
    (forge-create-post)
    (add-hook 'kill-buffer-hook 'bascht/checkout-mr-after-creation 90 t)
    (insert "/create_merge_request")))

(defun bascht/projectile-get-started ()
  ;; Open up a new project and reset to main / master + pull
  (interactive)
  (magit-call-git "checkout" (magit-main-branch))
  (magit-run-git-with-editor "pull"))

(after! elfeed
  (elfeed-org)
  (use-package! elfeed-link)
  (setq elfeed-search-filter "@1-month-ago +unread"
        rmh-elfeed-org-files `(,(expand-file-name "Feeds.org" org-directory))))

(custom-set-variables
 '(safe-local-variable-values '((buffer-read-only . 1))))

(custom-set-faces
 '(mode-line ((t (:family "IBM Plex Mono" :weight normal :height 1.0))))
 '(mode-line-active ((t (:family "IBM Plex Mono" :height 1.0)))) ; For 29+
 '(mode-line-inactive ((t (:family "IBM Plex Mono" :height 1.0))))
 '(mu4e-header-highlight-face ((t (:background "lemon chiffon"))))
 '(org-document-title ((t (:height 1.5 :underline nil))))
 '(org-level-1 ((t (:height 1.3 :overline nil :weight normal))))
 '(org-level-2 ((t (:height 1.2 :overline nil :weight light))))
 '(org-level-3 ((t (:height 1.1 :overline nil :weight light)))))

(load! "chezmoi.el")
(load! "mail.el")
(if (not (bascht/is-comacs))
    (load! "org.el"))
(load! "forge.el")
