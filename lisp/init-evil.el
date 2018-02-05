;;; package --- Summary
;; init-evil.el

;;; Commentary:

;;; Code:

(require-packages 'evil 'evil-leader 'evil-nerd-commenter 'evil-surround 'evil-easymotion)

;; Evil-leader
(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode t)
(defun luneyx-toggle-flycheck-error-list()
  (interactive)
  (if (not (get-buffer-window "*Flycheck errors*" 'visible))
    (flycheck-list-errors)
    (quit-windows-on "*Flycheck errors*")))
(defun split-window-left()
  (interactive)
  (split-window-right)
  (windmove-right))
(defun split-window-above()
  (interactive)
  (split-window-below)
  (windmove-down))
(defun split-terminal-window-left()
  (interactive)
  (split-window-left)
  (term "/bin/zsh"))
(defun split-terminal-window-below()
  (interactive)
  (split-window-above)
  (term "/bin/zsh"))
(evil-leader/set-key
  "th" 'previous-buffer
  "tl" 'next-buffer
  "tn" 'new-frame
  ",l" 'luneyx-toggle-flycheck-error-list
  "ll" 'luneyx-toggle-flycheck-error-list
  "sv" 'split-window-left
  "sh" 'split-window-above
  "stv" 'split-terminal-window-left
  "sth" 'split-terminal-window-below
  "ss" 'shrink-window
  "sw" 'enlarge-window
  "sa" 'shrink-window-horizontally
  "sd" 'enlarge-window-horizontally
)

;; Evil-mode
(require 'evil)
(setq evil-want-C-u-scroll t)
(when evil-want-C-u-scroll
   (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
   (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
   (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
   (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
(evil-ex-define-cmd "q[uit]" 'evil-window-delete)
(evil-mode t)

;; Evil-nerd-commenter
(require 'evil-nerd-commenter)
;; TODO: Sexy Comment
;; Vim key bindings
(evil-leader/set-key
  "c SPC" 'evilnc-comment-or-uncomment-lines
  "cy" 'evilnc-comment-and-kill-ring-save
  "cs" 'evilnc-comment-or-uncomment-lines
)

;; Evil-surround
(require 'evil-surround)
(global-evil-surround-mode t)

;; Evil-easymotion
;; TODO: Set leader key bindings
(require 'evil-easymotion)

(provide 'init-evil)
;;; init-evil.el ends here
