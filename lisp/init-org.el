;;; init-org.el --- configurations to the org-mode
;;; Commentary:
;;; Code:

;; ------------------------------------------------
;; Org-mode
;; ------------------------------------------------

(require 'org)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-src-fontify-natively t)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-hide-leading-stars t)
(setq org-tags-column 68)
(setq org-todo-keywords
	  '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "MEETING(m)" "|" "DONE(d!)" "CANCELLED(c@/!)")))
(setq org-todo-keyword-faces '(("NEXT" . (:foreground "#FFD54F" :background "#3A3A3A" :weight bold))
                               ("WAITING" . (:foreground "#00E676" :background "#3A3A3A" :weight bold))
                               ("MEETING" . (:foreground "#18FFFF" :background "#3A3A3A" :weight bold))))
(setq org-tag-alist '((:startgroup . nil)
                      ("@Computer" . ?c)
                      ("@School" . ?s)
                      ("@Home" . ?h)
                      ("@Way" . ?w)
                      ("@Blog" . ?b)
                      (:endgroup . nil)))
(setq org-refile-targets
      '((org-agenda-files :level . 1)
        ("~/org/someday.org" :level . 1)
        ("~/org/finished.org" :level . 4)
        ("~/org/trash.org" :level . 1)))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
  (ledger . t)
  (python . t)
  (ruby . t)))
                                 
(require 'org-habit)
(add-to-list 'org-modules 'org-habit)
(setq org-habit-graph-column 2)
(setq org-habit-following-days 3)
(setq org-habit-preceding-days 8)

;; ------------------------------------------------
;; Org-agenda
;; ------------------------------------------------
(require 'org-agenda)
(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-tags-column 75)
(setq org-agenda-include-diary t)
(setq org-agenda-files
      (list "~/org/inbox.org"
            "~/org/gtd.org"
            "~/org/habit.org"
            "~/org/note.org"
            "~/org/tickler.org"
            "~/org/school.org"
            "~/org/timetable.org"))
(defun luneyx-show-parent-header()
  (let ((x (nth 1 (org-get-outline-path))))
    (if x
        (concat "[ " (org-format-outline-path (list (nth 1 (org-get-outline-path)))) " ]\n              ") "")))
(setq org-agenda-custom-commands '(("d"
                                    "Notes, Habits, Today's Agenda and TODOs"
                                    ((tags "note" ((org-agenda-overriding-header "Remarks")))
                                     (agenda "" ((org-agenda-overriding-header "Agenda")
                                                 (org-agenda-ndays 1)
                                                 (org-agenda-files (remove "~/org/habit.org" org-agenda-files))))
                                     (tags-todo "TODO=\"NEXT\"+CATEGORY=\"Task\"|TODO=\"NEXT\"+CATEGORY=\"Project\""
                                                ((org-agenda-overriding-header "Next Actions")
                                                 (org-agenda-files (remove "~/org/habit.org" org-agenda-files))
                                                 (org-agenda-prefix-format " %i %-12:c%?(luneyx-show-parent-header)%i")))
                                     ;; (tags-todo "TODO=\"NEXT\"+CATEGORY=\"Project\"|TODO=\"WAITING\"+CATEGORY=\"Project\""
                                     ;;            ((org-agenda-overriding-header "Projects")
                                     ;;             (org-agenda-prefix-format " %i %-12:c%?(concat \"[ \"(org-format-outline-path (list (nth 1 (org-get-outline-path)))) \" ]\")\n              %i")))
                                     (tags-todo "TODO=\"WAITING\"+CATEGORY=\"Task\"|TODO=\"WAITING\"+CATEGORY=\"Project\""
                                                ((org-agenda-overriding-header "Waiting")
                                                 (org-agenda-files (remove "~/org/habit.org" org-agenda-files))
                                                 (org-agenda-prefix-format " %i %-12:c%?(luneyx-show-parent-header)%i")))
                                     (agenda "" ((org-agenda-overriding-header "Today's Habits")
                                                 (org-agenda-ndays 1)
                                                 (org-agenda-files (list "~/org/habit.org"))))
                                     (org-agenda-list-stuck-projects "" ((org-stuck-projects '("+LEVEL=2/-DONE-CANCELLED" ("NEXT" "WAITING") nil ""))
                                                                         (org-agenda-files (list "~/org/gtd.org"))))))
                                   ("i"
                                    "View all inbox"
                                    ((tags "note" ((org-agenda-overriding-header "Remarks")))
                                     (tags-todo "inbox"
                                                ((org-agenda-overriding-header "Inbox")
                                                 (org-agenda-files '("~/org/inbox.org"))))))))

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
      '(("i" "Inbox" entry (file "~/org/inbox.org") "* %?\n  %i\n" :empty-lines 1)
        ("t" "Todo Inbox" entry (file "~/org/inbox.org") "* TODO %?\n  %i\n" :empty-lines 1)
        ("r" "Reference Current Line" entry (file "~/org/inbox.org") "* %?\n  %i\n  %a" :empty-lines 1)
        ("n" "Note" entry (file "~/org/note.org") "* %?\n  %i" :empty-lines 1)))

(provide 'init-org)
;;; init-org.el ends here
