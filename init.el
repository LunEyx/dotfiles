;;; init.el --- Initialize All Stuff

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
(require 'init-theme)
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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(zerodark-theme yasnippet origami helm-descbinds git-gutter-fringe flycheck-irony evil-surround evil-nerd-commenter evil-magit evil-leader evil-easymotion elscreen company-irony))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
