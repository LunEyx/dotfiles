;;; package --- Summary
;; init-company.el

;;; Commentary:

;;; Code:

(require-packages 'company 'company-irony)

;; ------------------------------------------------
;; Core
;; ------------------------------------------------

;; Company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(company-tng-configure-default)
(setq company-idle-delay 0)
(setq company-tooltip-idle-delay 0)
(setq company-selection-wrap-around t)

;; ------------------------------------------------
;; Function
;; ------------------------------------------------

(defun complete-and-next-field ()
  "Complete company-mode selection and go to next field if exist."
  (interactive)
  (if (and company-candidates company-selection-changed)
    (company-complete-selection))
  (yas-next-field)
)

(defun complete-selection-or-expand-snippet ()
  "Complete company mode selection and expand the snippet if yas on."
  (interactive)
  (if (and company-candidates company-selection-changed)
    (company-complete-selection))
  (if yas-minor-mode (yas-expand))
)

;; ------------------------------------------------
;; Keybinding
;; ------------------------------------------------

(with-eval-after-load 'company
  (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map [ret] nil)
  (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous-or-abort)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-j") 'complete-selection-or-expand-snippet)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; ------------------------------------------------
;; Subordinate
;; ------------------------------------------------

;; Company-irony
(require 'company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(setq irony-additional-clang-options nil)

;; Company-yasnippet
(with-eval-after-load 'company
    (define-key yas-keymap (kbd "C-j") 'complete-and-next-field))

(defun company-mode-backend-with-yas (backend)
  "BACKEND: the current backend list."
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet)))
(setq company-backends (mapcar #'company-mode-backend-with-yas company-backends))

(provide 'init-company)
;;; init-company ends here
