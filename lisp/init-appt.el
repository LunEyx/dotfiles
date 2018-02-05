;;; init-appt.el --- Configurations to the appt

;;; commentary:

;;; Code:
(require 'appt)

;; ------------------------------------------------
;; configurations
;; ------------------------------------------------

(setq appt-display-interval 5)
(setq appt-message-warning-time 15)

;; ------------------------------------------------
;; Let appt-disp-window also show a notification
;; ------------------------------------------------

(defun appt-disp-window-with-notification(min-to-app new-time appt-msg)
  (shell-command (concat "terminal-notifier -title 'Remaining " min-to-app " minutes' -message '" appt-msg "'"))
  (appt-disp-window min-to-app new-time appt-msg))

(setq appt-disp-window-function 'appt-disp-window-with-notification)

(provide 'init-appt)
;;; init-appt.el ends here
