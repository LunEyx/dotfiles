;;; package --- Summary
;; init-buffer-manager.el

;;; Commentary:

;;; Code:

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq-default ibuffer-saved-filter-groups
	(quote (("default"
	       ("Org" (mode . org-mode))
	       ("Dired" (mode . dired-mode))
	       ("Markdown" (or
			    (name . "^diary$")
			    (mode . markdown-mode)))
	       ("ReStructText" (mode . rst-mode))
	       ("Python" (or (mode . python-mode)
			     (mode . ipython-mode)
			     (mode . inferior-python-mode)))
	       ("Ruby" (or
			(mode . ruby-mode)
			(mode . enh-ruby-mode)
			(mode . inf-ruby-mode)))))))

(provide 'init-buffer-manager)
;;; init-buffer-manager ends here
