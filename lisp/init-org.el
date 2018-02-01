;;; package --- Summary
;; init-org.el

;;; Commentary:

;;; Code:

;; ------------------------------------------------
;; Org-mode
;; ------------------------------------------------

(require 'org)
(setq org-enforce-todo-dependencies t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords
	  '((type "NOTE(n)" "|")
		(sequence "TODO(t)" "|" "DONE(d!)")))
(setq org-todo-keyword-faces
	  '(("NOTE" . (:background "#4c3840" :foreground "Green" :weight bold))))

;; ------------------------------------------------
;; Adjust TODO and DONE
;; ------------------------------------------------

(defun luneyx-org-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise."
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
    (org-back-to-heading t)
    (setq beg (point))
    (end-of-line)
    (setq end (point))
    (goto-char beg)
    (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                   end t)
        (if (match-end 1)
        (if (equal (match-string 1) "100%")
            (if (string-equal todo-state "TODO")
              (org-todo "DONE"))
          (if (string-equal todo-state "DONE")
            (org-todo "TODO")))
          (if (and (> (match-end 2) (match-beginning 2))
               (equal (match-string 2) (match-string 3)))
          (if (string-equal todo-state "TODO")
            (org-todo "DONE"))
        (if (string-equal todo-state "DONE")
          (org-todo "TODO")))))))))

(add-hook 'org-checkbox-statistics-hook 'luneyx-org-checkbox-todo)

;; ------------------------------------------------
;; Org-agenda
;; ------------------------------------------------
(require 'org-agenda)
(setq org-agenda-files
			  (list "~/org/plan.org"
					"~/org/book.org"
					"~/org/gtd.org"
					"~/org/school.org"
					"~/org/luneyx-gtd/timetable.org"))

(provide 'init-org)
;;; init-org.el ends here
