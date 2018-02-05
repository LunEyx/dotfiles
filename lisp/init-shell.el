;;; init-shell.el --- Fix GUI shell bug

;;; commentary:

;;; Code:
(require-packages 'exec-path-from-shell)

;; ------------------------------------------------
;; Core
;; ------------------------------------------------

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(provide 'init-shell)
;;; init-shell.el ends here
