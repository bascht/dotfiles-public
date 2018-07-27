;;; bascht --- notmuch settings
;;; Commentary:
;;; notmuch and mail related settings

;;; Code:

(cond
 ((or
   (string-equal (system-name) "zog")
   (string-equal (system-name) "kandalingo"))
  

  (use-package mu4e)
  (use-package mu4e-contrib)
 

  (add-hook 'mu4e-view-mode-hook
            (lambda()
              ;; try to emulate some of the eww key-bindings
              (local-set-key (kbd "<tab>") 'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)
              (setq shr-use-font nil)
              (setq shr-color-visible-luminance-min 70)
              (advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))
              ))

  ;; Reflow lines (via https://github.com/djcb/mu/pull/570)
  (add-hook 'mu4e-compose-mode-hook
            (defun cpb-compose-setup ()
              "Use hard newlines, so outgoing mails will have format=flowed."
              (use-hard-newlines t 'guess)
              "Default to composing new mails in german"
              (adict-change-dictionary "german")
              ))

  (add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)
                                        ;(setq mu4e-html2text-command "pandoc -f html -t markdown")
  ;(use-package notmuch)

  ;(setq notmuch-address-command "/home/bascht/bin/address")
  ;(notmuch-address-message-insinuate)
  ;(setq notmuch-archive-tags '("-unread" "+archive"))
  (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
  (autoload 'gnus-alias-determine-identity "gnus-alias" "" t)

;  (defun notmuch-unread () "Filter for unread and inboxed messages."
;         (interactive)
;         (notmuch-search "tag:unread and tag:inbox"))
;
;  (defun notmuch-search-toggle-flagged ()
;    "Toggle flagged state on the current item."
;    (lambda ()
;      (interactive)
;      (if (member "flagged" (notmuch-search-get-tags))
;          (notmuch-search-tag (list (concat "+" "flagged"))))
;      (notmuch-search-tag (list(concat "-" "flagged")))
;      (notmuch-search-next-thread)))
;
;  (setq notmuch-crypto-process-mime t)
;  (setq notmuch-hello-sections
;    (quote
;     (notmuch-hello-insert-header notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer notmuch-hello-insert-inbox)))
;  (setq notmuch-message-headers (quote ("Subject" "To" "Cc" "Date" "Received")))
;  (setq notmuch-search-oldest-first nil)
;

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "msmtp"
        user-full-name "Sebastian Schulze")

  (setq message-sendmail-f-is-evil 't)
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-kill-buffer-on-exit t)

  (setq mail-host-address "bascht.com")
  (setq mu4e-maildir "/home/bascht/Mail")
  (setq mu4e-refile-folder (format "/bascht.com/Archiv.%s" (format-time-string "%Y")))
  (setq mu4e-sent-folder "/bascht.com/Sent")
  (setq mu4e-trash-folder "/bascht.com/Trash")
  (setq mu4e-drafts-folder "/bascht.com/Drafts")
  (setq mu4e-view-prefer-html nil)
  (setq mu4e-view-show-images nil)
  (setq mu4e-use-fancy-chars nil)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-openpgp-signers nil)
  ;(setq mu4e-html2text-command "pandoc -f html -t markdown")

  ;;; Sod it. Nobody is making proper HTML emails enyway
  (setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

  ;;; load mail-mode when starting up from mutt
  (setq auto-mode-alist (append '(("/tmp/mutt.*" . mail-mode)) auto-mode-alist))

  (setq mu4e-get-mail-command "offlineimap -qo -f INBOX")

  ;;; Contexts
  (defun file-string (file)
    "Read the contents of a file and return as a string."
    (with-current-buffer (find-file-noselect file)
      (buffer-string)))

  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "Bascht"
             :enter-func (lambda () (mu4e-message "Entering Bascht context"))
             :leave-func (lambda () (mu4e-message "Leaving Bascht context"))
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg 
                             (mu4e-message-contact-field-matches msg 
                                                                 :to "mail@bascht.com")))
             :vars '( ( user-mail-address      . "mail@bascht.com"  )
                      ( user-full-name         . "Sebastian Schulze" )
                      ( mu4e-compose-signature . (file-string "/home/bascht/.homesick/repos/private/home/.signature") ) )
             )
           ,(make-mu4e-context
             :name "RubyBerlin"
             :enter-func (lambda () (mu4e-message "Switch to the Ruby Berlin context"))
             ;; no leave-func
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg 
                             (mu4e-message-contact-field-matches msg 
                                                                 :to "bascht@rubyberlin.org")))
             :vars '( ( user-mail-address       . "bascht@rubyberlin.org" )
                      ( user-full-name          . "Sebastian Schulze (Ruby Berlin e.V.)" )
                      ( mu4e-compose-signature . (file-string "/home/bascht/.homesick/repos/private/home/.signature.rubyberlin") ) )

             )
           ,(make-mu4e-context
             :name "OrgaRubyOnIce"
             :enter-func (lambda () (mu4e-message "Switch to the RoI Orga context"))
             ;; no leave-func
             ;; we match based on the contact-fields of the message
             :match-func (lambda (msg)
                           (when msg 
                             (mu4e-message-contact-field-matches msg 
                                                                 :to "orga@rubyonice.com")))
             :vars '( ( user-mail-address       . "orga@rubyonice.com" )
                      ( user-full-name          . "Sebastian Schulze (Ruby on Ice)" )
                      ( mu4e-compose-signature . (file-string "/home/bascht/.homesick/repos/private/home/.signature.rubyonice") ) )

             )
           )
        )

  (add-to-list 'mu4e-bookmarks
               (make-mu4e-bookmark
                :name  "INBOX unread"
                :query "flag:unread AND maildir:/INBOX)"
                :key ?U)
               (make-mu4e-bookmark
                :name  "Bahn Tickets"
                :query "to:bahn.de@bascht.com and (subject:Booking* OR subject:Buchung*)"
                :key ?B))
  
  ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
  ;; guess or ask the correct context, e.g.

  ;; start with the first (default) context; 
  ;; default is to ask-if-none (ask when there's no context yet, and none match)
  ;(setq mu4e-context-policy 'pick-first)

  ;; compose with the current context is no context matches;
  ;; default is to ask 
  ;(setq mu4e-compose-context-policy 'pick-first)

  (setq mu4e-enable-notifications t)
  (setq mu4e-enable-async-operations t)
  (setq mu4e-user-mail-address-list (quote ("mail@bascht.com" "bascht@rubyberlin.org")))

  (setq mu4e-compose-dont-reply-to-self 't)

  (setq mu4e-enable-mode-line t)
  (with-eval-after-load 'mu4e-alert (mu4e-alert-set-default-style 'notifications))
  (with-eval-after-load 'mu4e (require 'mu4e-conversation))
  (setq mu4e-conversation-print-function (quote mu4e-conversation-print-tree))
  (global-mu4e-conversation-mode)

  (provide 'mail-config)

;;; mail-config.el ends here
