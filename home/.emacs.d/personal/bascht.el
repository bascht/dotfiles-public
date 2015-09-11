;;; bascht --- meine settings
;;; Commentary:
;;; Hacky as whatnot, but it's mime.
;;; Code:

(require 'package)
(require 'ag)
(require 'nyan-mode)
(require 'notmuch)
(require 'notmuch-address)
(require 'multiple-cursors)
(require 'yasnippet)
(require 'rspec-mode)
(require 'ansi-color)

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

(nyan-mode 1)
(org-bullets-mode 1)

;;; Global modes
(projectile-global-mode)
(global-company-mode)
(global-hl-line-mode 0)
(yas-global-mode 1)

(setq ispell-dictionary "german")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)

(setq browse-url-browser-function (quote browse-url-firefox))

;;; Enable Projectile  cache until I know what is going on
(setq projectile-enable-caching t)

;;; Skip to next match group automatically
(setq helm-move-to-line-cycle-in-source nil)
(setq helm-ls-git-fuzzy-match t)

;;; Start with an english dictionary
(setq ispell-dictionary 'english)

(prelude-require-packages '(ace-jump-mode
                            ace-window
                            ack-and-a-half
                            ag
                            anzu
                            cider
                            clojure-mode
                            coffee-mode
                            company
                            company-go
                            company-restclient
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
                            graphviz-dot-mode
                            haml-mode
                            helm-ag
                            helm-dictionary
                            helm-dash
                            helm-flycheck
                            helm-gist
                            helm-git
                            helm-go-package
                            helm-google
                            helm-ls-svn
                            helm-orgcard
                            helm-projectile
                            helm-rb
                            helm-unicode
                            inf-ruby
                            less-css-mode
                            lua-mode
                            magit-gerrit
                            magit-gh-pulls
                            markdown-mode
                            moe-theme
                            monokai-theme
                            move-text
                            multiple-cursors
                            nginx-mode
                            notmuch
                            notmuch-labeler
                            notmuch-unread
                            nyan-mode
                            org
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
                            undo-tree
                            vagrant
                            vagrant-tramp
                            visual-fill-column
                            writeroom-mode
                            yaml-mode
                            yasnippet))

;;; bascht.el ends here
