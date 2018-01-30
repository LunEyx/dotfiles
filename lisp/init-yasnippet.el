;;; package --- Summary
;; init-yasnippet.el

;;; Commentary:

;;; Code:

(require-packages 'yasnippet)

;; Yasnippet
(require 'yasnippet)
(yas-global-mode t)
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))

(with-eval-after-load 'yasnippet
    (define-key yas-minor-mode-map [tab] nil)
    (define-key yas-minor-mode-map (kbd "TAB") nil)
    (define-key yas-minor-mode-map (kbd "C-j") yas-maybe-expand)
    (define-key yas-keymap [tab] nil)
    (define-key yas-keymap [backtab] nil)
    (define-key yas-keymap [(shift tab)] nil)
    (define-key yas-keymap (kbd "TAB") nil)
    (define-key yas-keymap (kbd "S-TAB") nil)
    (define-key yas-keymap (kbd "C-j") 'yas-next-field)
    (define-key yas-keymap (kbd "C-k") 'yas-prev-field))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
