;;; init-calendar.el --- Configurations for calendar

;;; Commentary:

;;; Code:
(require-packages 'calfw)

(require 'calfw)
(require 'calfw-cal)
(require 'calfw-org)
(setq cfw:org-agenda-schedule-args nil)
(setq cfw:render-line-breaker 'cfw:render-line-breaker-none)
(define-key cfw:org-schedule-map (kbd "g") 'cfw:refresh-calendar-buffer)
(define-key cfw:org-schedule-map (kbd "x") 'cfw:org-clean-exit)
(define-key cfw:org-schedule-map (kbd "d") 'cfw:change-view-day)
(define-key cfw:org-schedule-map (kbd "v d") 'cfw:change-view-day)
(define-key cfw:org-schedule-map (kbd "v w") 'cfw:change-view-week)
(define-key cfw:org-schedule-map (kbd "v 2 w") 'cfw:change-view-two-weeks)
(define-key cfw:org-schedule-map (kbd "v m") 'cfw:change-view-month)
(global-set-key
 (kbd "C-c m")
 (lambda()
   "Open a calfw calendar view."
   (interactive)
    (cfw:open-calendar-buffer
     :custom-map
     cfw:org-schedule-map
     :contents-sources
     (list
      (cfw:org-create-source) ; orgmode source
      (cfw:cal-create-source) ; diary source
      ))))

(require 'solar)
(setq calendar-latitude 22.28552)
(setq calendar-longitude 114.15769)
(setq calendar-location-name "Hong Kong")

(require 'calendar)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq calendar-mark-diary-entries-flag t)
(setq calendar-mark-holidays-flag t)

(require 'holidays)
(setq holiday-other-holidays
      '(;; January
        (holiday-fixed 1 1 "元旦")
        ;; Feburary
        (holiday-fixed 2 14 "情人節")
        ;; March
        (holiday-fixed 3 8 "婦女節")
        (holiday-easter-etc -2 "耶穌受難節")
        (holiday-easter-etc -1 "耶穌受難節翌日")
        (holiday-easter-etc 1 "復活節星期一")
        ;; April
        (holiday-fixed 4 1 "愚人節")
        (holiday-fixed 4 4 "兒童節")
        ;; May
        (holiday-fixed 5 1 "勞動節")
        (holiday-fixed 5 2 "佛誕")
        ;; June
        ;; July
        (holiday-fixed 7 1 "香港回歸")
        ;; August
        ;; September
        ;; October
        (holiday-fixed 10 1 "國慶日")
        (holiday-fixed 10 31 "萬聖節")
        ;; November
        ;; December
        (holiday-fixed 12 25 "聖誕節")
        (holiday-fixed 12 26 "節禮日")
        ;; Chinese Holidays
        (holiday-chinese 1 1 "農曆年初一")
        (holiday-chinese 1 2 "農曆年初二")
        (holiday-chinese 1 3 "農曆年初三")
        (when (holiday-chinese-qingming)
          (let ((qingming (nth 0 (nth 0 (holiday-chinese-qingming)))))
            (list (list qingming "清明節"))))
        (holiday-chinese 5 5 "端午節")
        (holiday-chinese 8 15 "中秋節")
        (holiday-chinese 8 16 "中秋節翌日")
        (holiday-chinese 9 9 "重陽節")
        (when (holiday-chinese-winter-solstice)
          (let ((winter-solstice (nth 0 (nth 0 (holiday-chinese-winter-solstice)))))
            (list (list winter-solstice "冬至"))))
        ))

(setq calendar-holidays
      (append holiday-other-holidays))

(provide 'init-calendar)
;;; init-calendar.el ends here
