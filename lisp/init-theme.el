;;; init-theme.el --- Configurations for theme

;;; Commentary:

;;; Code:

(require-package 'zerodark-theme)

;; ------------------------------------------------
;; Beautify
;; ------------------------------------------------

;; Color Theme
(require 'zerodark-theme)
(load-theme 'zerodark t)
(zerodark-setup-modeline-format) ;; optionally setup modeline

;; Font
;; Setting English Font
(set-face-attribute
 'default nil :font "DejaVuSansMono Nerd Font 18")
;; Chinese Font 配制中文字体
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Micro Hei Mono" :size 22)))

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
