;;; init-utils.el --- configurations to utility items

;;; Commentary:

;;; Code:

;; ------------------------------------------------
;; Utility Config
;; ------------------------------------------------

;; Put all backup files to .saves
;; (setq backup-directory-alist `(("." . "~/.saves")))

;; Don't backup
(setq make-backup-files nil)

;; Show matching parenthesis
(show-paren-mode t)

;; Linum-mode
(global-linum-mode t)

;; Show current time at mode-line
(display-time-mode t)

;; Let tab key insert tab
(setq-default indent-line-function 'insert-tab)

;; Use space instead of tabs
(setq-default indent-tabs-mode nil)

;; Set tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(provide 'init-utils)
;;; init-utils.el ends here
