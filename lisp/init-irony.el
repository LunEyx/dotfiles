;;; package --- Summary
;; init-irony.el

;;; Commentary:

;;; Code:

(require-packages 'irony)

;; ------------------------------------------------
;; Core
;; ------------------------------------------------

;; Irony-mode
(require 'irony)
(with-eval-after-load 'irony
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(provide 'init-irony)
;;; init-irony.el ends here
