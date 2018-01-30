;;; package --- Summary
;; init-git-gutter.el

;;; Commentary:

;;; Code:

(require-packages 'git-gutter-fringe)

;; Git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(setq-default left-fringe-width  20)
(setq-default right-fringe-width 20)
(fringe-helper-define 'git-gutter-fr:modified nil
  "..XXXX.."
  ".X....X."
  "X......X"
  "X......X"
  "X......X"
  "X......X"
  ".X....X."
  "..XXXX..")

(provide 'init-git-gutter)
;;; init-git-gutter.el ends here
