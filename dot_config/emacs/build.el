;;; build --- test
;;; Commentary:
;; Build my current Emacs config in CI
;;; Code:

(require 'org)
(org-babel-tangle-file "README.org")
(find-file "init.el")
(eval-buffer)
(require 'flycheck)
(flycheck-buffer)
(provide 'build)
;;; build.el ends here
