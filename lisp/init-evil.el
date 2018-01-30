;;; package --- Summary
;; init-evil.el

;;; Commentary:

;;; Code:

(require-packages 'evil 'evil-leader 'evil-nerd-commenter 'evil-surround 'evil-easymotion)

;; Evil-leader
(require 'evil-leader)
(evil-leader/set-leader ",")
(global-evil-leader-mode t)
(evil-leader/set-key
  "th" 'previous-buffer
  "tl" 'next-buffer
)

;; Evil-mode
(require 'evil)
(setq evil-want-C-u-scroll t)
(when evil-want-C-u-scroll
   (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
   (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
   (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
   (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))
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
