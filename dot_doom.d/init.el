;;; init.el -*- lexical-binding: t; -*-


(doom! :input
       :completion
       ;; (company +childframe +tng)           ; the ultimate code completion backend
       ;(ivy -fuzzy +prescient +icons) ; a search engine for love and life
       (corfu +icons +orderless +dabbrev)
       (vertico +icons)

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       (emoji +github +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       (modeline)        ; snazzy, Atom-inspired modeline, plus API
       indent-guides     ; highlighted indent columns
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       treemacs          ; a project drawer, like neotree but cooler
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format            ; automated prettiness
       ;; parinfer
       multiple-cursors  ; editing in many places at once
       snippets          ; my elves. They type so I don't have to

       :emacs
       (dired +icons +bindings)           ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +childframe)      ; tasing you for every semicolon you forget
       (spell +aspell)   ; tasing you for misspelling mispelling
       ;grammar           ; tasing grammar mistake every you make

       :tools
       (lsp +peek)
       (debugger +lsp)
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +docsets +dictionary) ; navigate your code and its documentation
       (magit +forge)    ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; pdf enhancements
       (terraform +treemacs-git-mode)         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       pass
       tree-sitter

       :os
       tty               ; improve the terminal Emacs experience

       :lang
       (emacs-lisp +tree-sitter)                                                  ; drown in parentheses
       (go +lsp +tree-sitter)                                                     ; the hipster dialect
       json                                                                       ; At least it ain't XML
       ledger                                                                     ; an accounting system in Emacs
       markdown                                                                   ; writing docs for people to ignore
       nix                                                                        ; I hereby declare "nix geht mehr!"
       (org +gnuplot +dragndrop +journal +pandoc +pomodoro +present +hugo +habit +noter) ; organize your plain life in plain text
       (ruby +rails)                                                              ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (sh +fish)                                                                 ; she sells {ba,z,fi}sh shells on the C xor
       (yaml +lsp +tree-sitter)                                                   ; JSON, but readable

       :email
       (mu4e +org)

       :app
       calendar
       ;everywhere        ; *leave* Emacs!? You must be joking
       (rss +org)        ; emacs as an RSS reader

       :config
       (default +bindings +smartparens))
