;;; bascht --- meine settings
;;; Commentary:
;;; Hacky as whatnot, but it's mime.
;;; Code:

(require 'package)
(require 'ag)
(require 'nyan-mode)
(require 'notmuch)
(require 'notmuch-address)
(require 'multiple-cursors-core)
(require 'yasnippet)
(require 'rspec-mode)
(require 'ansi-color)
(require 'org-alert)


(add-to-list 'load-path "~/.emacs.d/personal/")

(load-library "org-config")
(load-library "appearance-config")
(load-library "coding-config")
(load-library "mail-config")
(load-library "shortcut-config")

;; Archives from which to fetch.
(setq package-archives
      (append '(("melpa" . "http://melpa.org/packages/"))
              package-archives))

;;; Hello, Emacs!
(setq user-full-name "Sebastian Schulze")
(setq user-mail-address "github.com@bascht.com")

(org-bullets-mode 1)

;;; Global modes
(projectile-global-mode)
(global-hl-line-mode 0)
(yas-global-mode 1)

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

(setq browse-url-browser-function (quote browse-url-firefox))

;;; Enable Projectile  cache until I know what is going on
(setq projectile-enable-caching t)

;;; Directly jump into Commander.
(setq projectile-switch-project-action
      #'projectile-commander)

;;; Skip to next match group automatically
(setq helm-move-to-line-cycle-in-source nil)
(setq helm-ls-git-fuzzy-match t)

;;; Start with a German  dictionary
(ispell-change-dictionary "deutsch8")

;;; Show OrgMode Aganda on start.
(add-hook 'after-init-hook 'org-agenda-list)

(prelude-require-packages '(ace-jump-mode
                            ace-window
                            ag
                            anzu
                            cider
                            clojure-mode
                            coffee-mode
                            company
                            company-go
                            company-restclient
                            company-tern
                            csv-mode
                            dockerfile-mode
                            emmet-mode
                            expand-region
                            fish-mode
                            flycheck
                            flycheck-rust
                            flymake
                            flymake-go
                            flymake-puppet
                            flymake-jslint
                            flymake-shell
                            flymake-yaml
                            gerrit-download
                            git-commit
                            git-gutter
                            git-timemachine
                            gitconfig-mode
                            gitignore-mode
                            gnuplot
                            gnus-alias
                            graphviz-dot-mode
                            haml-mode
                            helm-ag
                            helm-dictionary
                            helm-dash
                            helm-flycheck
                            helm-git
                            helm-go-package
                            helm-google
                            helm-ls-svn
                            helm-orgcard
                            helm-projectile
                            helm-rb
                            helm-swoop
                            helm-unicode
                            inf-ruby
                            less-css-mode
                            lua-mode
                            magit-gerrit
                            magit-gh-pulls
                            markdown-mode
                            material-theme
                            moe-theme
                            monokai-theme
                            move-text
                            multiple-cursors
                            nginx-mode
                            notmuch
                            notmuch-labeler
                            nyan-mode
                            org
                            org-alert
                            org-beautify-theme
                            org-bullets
                            org-dashboard
                            org-pandoc
                            org-present
                            org-projectile
                            ox-ioslide
                            ox-reveal
                            paredit
                            password-store
                            php-mode
                            puppet-mode
                            rainbow-delimiters
                            restclient
                            rspec-mode
                            ruby-compilation
                            ruby-dev
                            ruby-test-mode
                            ruby-tools
                            rust-mode
                            sass-mode
                            scss-mode
                            smartparens
                            terraform-mode
                            toml-mode
                            tern
                            tern-context-coloring
                            undo-tree
                            vagrant
                            vagrant-tramp
                            visual-fill-column
                            writeroom-mode
                            yaml-mode
                            yasnippet))

;;; bascht.el ends here
