;;; init-org.el --- configurations to the org-mode

;;; Commentary:

;;; Code:

;; ------------------------------------------------
;; Org-mode
;; ------------------------------------------------

(require 'org)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-enforce-todo-dependencies t)
(setq org-hide-leading-stars t)
(setq org-todo-keywords
	  '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(s)" "|" "DONE(d!)" "CANCELED(c@/!)")))
(setq org-refile-targets
      '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3) ("~/org/finished.org" :maxlevel . 3)))
;; (setq org-todo-keyword-faces
;; 	  '(("NOTE" . (:background "#4c3840" :foreground "Green" :weight bold))))
(setq org-babel-load-languages '((emacs-lisp . t)
                                 (c . t)
                                 (c++ . t)
                                 (ledger . t)
                                 (python . t)
                                 (ruby . t)))
                                 

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
      (list "~/org/inbox.org"
            "~/org/task.org"
            "~/org/note.org"
            "~/org/trash.org"
            "~/org/project.org"
            "~/org/timetable.org"))

;; ------------------------------------------------
;; org-agenda-with-appt
;; ------------------------------------------------
(require 'appt)

(defun luneyx-org-agenda-to-appt()
  (interactive)
  (appt-check)
  (org-agenda-to-appt t))

(run-at-time "12:05am" (* 24 3600) 'luneyx-org-agenda-to-appt)

;; ------------------------------------------------
;; org-capture
;; ------------------------------------------------
(require 'org-capture)

(setq org-capture-templates
      '(("n" "New" entry (file "~/org/inbox.org") "* %? %U\n  %i\n  %a\n" :empty-lines-after 1)
        ("t" "Task" entry (file+headline "~/org/task.org" "Tasks") "** %?\n   %i\n   %a" :empty-lines-after 1)
        ("i" "Idea" entry (file+headline "~/org/task.org" "Ideas") "** %?\n   %i\n   %a" :empty-lines-after 1)
        ("N" "Note" entry (file "~/org/note.org") "* %?\n  %i\n  %a" :empty-lines-after 1)
        ("p" "Project" entry (file "~/org/project.org") "** %?\n   %i\n   %a" :empty-lines-after 1)))

(provide 'init-org)
;;; init-org.el ends here
