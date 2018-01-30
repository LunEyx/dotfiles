;;; package --- Summary
;; init-org.el

;;; Commentary:

;;; Code:

(require 'org)

;; ------------------------------------------------
;; Adjust TODO and DONE
;; ------------------------------------------------

;; (defun luneyx-org-checkbox-todo ()
;;   "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise."
;;   (let ((todo-state (org-get-todo-state)) beg end)
;;     (unless (not todo-state)
;;       (save-excursion
;;     (org-back-to-heading t)
;;     (setq beg (point))
;;     (end-of-line)
;;     (setq end (point))
;;     (goto-char beg)
;;     (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
;;                    end t)
;;         (if (match-end 1)
;;         (if (equal (match-string 1) "100%")
;;             (unless (string-equal todo-state "DONE")
;;               (org-todo 'done))
;;           (unless (string-equal todo-state "TODO")
;;             (org-todo 'todo)))
;;           (if (and (> (match-end 2) (match-beginning 2))

;;           (unless (string-equal todo-state "DONE")
;;             (org-todo 'done))
;;         (unless (string-equal todo-state "TODO")
;;           (org-todo 'todo)))))))))

(provide 'init-org)
;;; init-org.el ends here
