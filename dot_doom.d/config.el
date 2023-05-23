;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Determine if we're in comacs as early as possible
;; even if 'server-name is not set yet
(defun bascht/is-comacs()
  (string= (getenv "EMACS_SERVER_NAME") "comacs"))

(setq doom-font (font-spec :family "JetBrains Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      doom-serif-font (font-spec :family "Iosevka Aile" :size 16)
      doom-unicode-font (font-spec :family "Joypixels" :size 14)
      doom-theme (if (bascht/is-comacs) 'doom-one-light 'ef-light)
      doom-modeline-height 23
      ef-themes-mixed-fonts t
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
      ispell-aspell-data-dir "/home/bascht/.nix-profile/lib/aspell"
      ispell-aspell-dict-dir ispell-aspell-data-dir
      ruby-insert-encoding-magic-comment nil
      git-gutter-fr+-side (quote left-fringe)
      helm-org-rifle-show-path t
      doom-dashboard-ascii-banner-fn #'bascht/doom-dashboard-calvin
      frame-title-format (concat "%b - " user-login-name "@" (system-name))
      dired-async-mode t
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

(defun doom-dashboard-draw-ascii-banner-fn ()
  (let* ((banner
          '(
            "                      _ww   _a+”D"
            "               y#,  _r^ # _*^  y`"
            "              q0 0 a”   W*`    F   ____"
            "           ;  #^ Mw`  __`. .  4-~~^^`"
            "          _  _P   ` /'^           `www=."
            "        , $  +F    `                q"
            "        K ]                         ^K`"
            "      , #_                . ___ r    ],"
            "      _*.^            '.__dP^^~#,  ,_ *,"
            "      ^b    / _         ``     _F   ]  ]_"
            "       '___  '               ~~^    ]   ["
            "       :` ]b_    ~k_               ,`  yl"
            "         #P        `*a__       __a~   z~`"
            "         #L     _      ^------~^`   ,/"
            "          ~-vww*”v_               _/`"
            "                  ^”q_         _x”"
            "                   __#my..___p/`mma____"
            "               _awP”,`,^”-_”^`._ L L  #"
            "             _#0w_^_^,^r___...._ t [],”w"
            "            e^   ]b_x^_~^` __,  .]Wy7` x`"
            "             '=w__^9*$P-*MF`      ^[_.="
            "                 ^”y   qw/”^_____^~9 t"
            "                   ]_l  ,'^_`..===  x'"
            "                    ”>.ak__awwwwWW###r"
            "                      ##WWWWWWWWWWWWWW__"
            "                     _WWWWWWMM#WWWW_JP^”~-=w_"
            "           .____awwmp_wNw#[w/`     ^#,      ~b___."
            "            ` ^^^~^”W___            ]Raaaamw~`^``^^~"
            "                      ^~”~---~~~~~~`"))

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
      :desc "Convert to GIF" "g" #'dwim-shell-command-convert-to-gif
      )

(map!
   :after markdown-mode
   :map evil-markdown-mode-map
   :n [return] #'bascht/markdown-do
   :i "M-b" #'backward-word
   )

(map!
   :after mu4e
   :map mu4e-view-mode-map :vn
   "T" (lambda () (interactive) (mu4e-view-mark-thread '(refile))))

(map!
   :after mu4e
   :map mu4e-headers-mode-map :vn
   "T" (lambda () (interactive) (mu4e-headers-mark-thread nil '(refile))))

;; https://micro.rousette.org.uk/2021/01/03/a-useful-binding.html
(map!
 (:map 'override
   :v "v" #'er/expand-region
   :v "V" #'er/contract-region))

; Define quick helper switches to switch between languages while
; keeping distinct personal dictionaries for both of them
(defun bascht/switch-spellcheck (lang)
  (interactive)
  (message lang)
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
(use-package! zoxide)

(use-package! dwim-shell-command
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
     "swaymsg exec -- drop <<f>>"
     :utils "drop"
     :silent-success t
     ))

  (defun dwim-shell-command-drag ()
    "Drag stuff via dragon"
    (interactive)
    (dwim-shell-command-on-marked-files
     "Drag files somewhere"
     "swaymsg exec -- dragon --and-exit <<f>>"
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


; Don't add #0 #1 #2 #3… workspaces :D
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

; Directly create a matching workspace for the project (when launched with `bin/tn')
(defun bascht/switch-to-or-load-workspace (name directory)
  (interactive)
  (message "Started workspace switcher")
  (persp-mode)
  (if (+workspace-exists-p name)
      (+workspace-switch name)
    (progn (+workspace-new name)
           (+workspace-switch name)
           (find-file directory)
           (magit-status-setup-buffer))))

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
    ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end)) :modes (markdown-mode markdown-mode gfm-mode org-mode text-mode))
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
   mu4e-get-mail-command "mbsync -a"
   mu4e-headers-include-related t
   mu4e-index-lazy-check nil
   mu4e-index-cleanup t
   sendmail-program "msmtp"))

; Disable spell-fu-mode globally
(remove-hook 'text-mode-hook #'spell-fu-mode)

(add-to-list 'auto-mode-alist '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.journal\\'" . ledger-mode))

; Always open new project with dired
(defun bascht/projectile-open-dired (dir)
  (let ((default-directory (file-truename (expand-file-name dir))))
    (call-interactively #'projectile-dired)))

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

(add-hook! 'terraform-mode-hook   #'format-all-mode)

(defun bascht/markdown-do ()
  "Keep markdown-do from straight away going into gfm-mode and adding checkboxes"
  (interactive)
  (cond
   ((thing-at-point-looking-at obsidian--basic-wikilink-regex)
    (obsidian-follow-link-at-point))
   ((thing-at-point-looking-at markdown-regex-wiki-link)
    (markdown-follow-wiki-link-at-point))
   (t
    (markdown-do))))

(after! markdown
  (setq markdown-enable-wiki-links t
        markdown-wiki-link-search-type '(sub-directories parent-directories)
        markdown-wiki-link-fontify-missing t
        markdown-link-space-sub-char " "))


(after! magit
    (magit-wip-mode)
    (magit-todos-mode)
    (setq magit-log-arguments '("--graph" "--decorate" "--color")
                    magit-delete-by-moving-to-trash nil
                              git-commit-summary-max-length 80))
(defun bascht/file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))


(after! dap
  (unless (display-graphic-p)
    (set-face-background 'dap-ui-marker-face "orange")
    (set-face-attribute 'dap-ui-marker-face nil :inherit nil)
    (set-face-background 'dap-ui-pending-breakpoint-face "lightpink")
    (set-face-attribute 'dap-ui-verified-breakpoint-face nil :inherit 'dap-ui-pending-breakpoint-face)))

(after! obsidian (obsidian-specify-path "~/WirZwei/Zettelkasten"))

(use-package! dirvish
  :config
  (evil-make-overriding-map dirvish-mode-map 'normal)
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)

  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes '(vc-state subtree-state all-the-icons collapse git-msg file-time file-size))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("gg"  . beginning-of-buffer)
   ("gu"  . dired-up-directory)
   ("md"  . dwim-shell-command-drag)
   ("mo"  . dwim-shell-command-drop)
   ("G"   . end-of-buffer)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump))
  )

(defun bascht/dirvish-tdir()
  (interactive)
  (find-file
   (string-trim (shell-command-to-string "mktemp -d"))))

(custom-set-faces
 '(mode-line ((t (:family "Iosevka Aile" :height 1.0))))
 '(mode-line-active ((t (:family "Iosevka Aile" :height 1.0)))) ; For 29+
 '(mode-line-inactive ((t (:family "Iosevka Aile" :height 1.0))))
 '(mu4e-header-highlight-face ((t (:background "lemon chiffon"))))
 '(org-document-title ((t (:height 1.5 :underline nil))))
 '(org-level-1 ((t (:height 1.3 :weight normal))))
 '(org-level-2 ((t (:height 1.2 :weight light))))
 '(org-level-3 ((t (:height 1.1 :weight light)))))

(load! "chezmoi.el")
(load! "mail.el")
(if (not (bascht/is-comacs))
    (load! "org.el"))
(load! "forge.el")
