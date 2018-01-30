;;; package --- Summary
;; init-utils.el

;;; Commentary:

;;; Code:

;; ------------------------------------------------
;; Utility Config
;; ------------------------------------------------

;; Put all backup files to .saves
(setq backup-directory-alist `(("." . "~/.saves")))

;; Show matching parenthesis
(show-paren-mode t)

;; Linum-mode
(global-linum-mode t)

;; Let tab key insert tab
(setq-default indent-line-function 'insert-tab)

;; Set tab width
(setq-default tab-width 4)

(provide 'init-utils)
;;; init-utils.el ends here
