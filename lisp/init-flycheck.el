;;; package --- Summary
;; init-flycheck.el

;;; Commentary:

;;; Code:

(require-packages 'flycheck)

;; ------------------------------------------------
;; Core
;; ------------------------------------------------

;; Flycheck
(require 'flycheck)
(global-flycheck-mode t)
(setq-default flycheck-emacs-lisp-load-path 'inherit)
(setq-default flycheck-clang-args
    (list
        "-std=c++14"
        "-Wall"
        "-Wextra"
        "-Wno-unused-parameter"))
(setq-default flycheck-clang-include-path
    (list
        "include"
        "../include"
        "../lib/glad/include"))

;; ------------------------------------------------
;; Subordinate
;; ------------------------------------------------

;; Flycheck-irony
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
