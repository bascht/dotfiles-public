;;; build --- test
;;; Commentary:
;; Build my current Emacs config in CI
;;; Code:

(require 'org)
(org-babel-tangle-file "README.org")
(find-file "init.el")
(eval-buffer)
(require 'flymake)
(flymake-mode)
(flymake-start)
(flymake-show-buffer-diagnostics)
(require 'elisp-autofmt)
(elisp-autofmt-buffer)
(provide 'build)
;;; build.el ends here
