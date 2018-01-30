;;; package --- Summary
;; init.el

;;; Commentary:

;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-utils)
(require 'init-elpa)
(require 'init-evil)
(require 'init-yasnippet)
(require 'init-irony)
(require 'init-company)
(require 'init-flycheck)
(require 'init-git-gutter)
(require 'init-origami)
(require 'init-org)
(require 'init-filetype)
(require 'init-buffer-manager)
(require 'init-theme)

(provide 'init)
;;; init.el ends here
