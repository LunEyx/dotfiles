;;; package --- Summary
;; init-theme.el

;;; Commentary:

;;; Code:

(require-package 'zerodark-theme)

;; ------------------------------------------------
;; Beautify
;; ------------------------------------------------

;; Color Theme
(load-theme 'zerodark t)
(zerodark-setup-modeline-format) ;; optionally setup modeline

;; Font Size
(add-to-list 'default-frame-alist '(font . "DejaVuSansMono Nerd Font-18"))

;; ------------------------------------------------
;; Disable GUI Item
;; ------------------------------------------------

;; Disable ToolBar
(tool-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; ------------------------------------------------
;; Screen Size Related
;; ------------------------------------------------

;; Maximize Screen Fix
(setq frame-resize-pixelwise t)

;; Always Full Screen
(set-frame-parameter nil 'fullscreen 'fullboth)

(provide 'init-theme)
;;; init-theme.el ends here
