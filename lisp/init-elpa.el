;;; package --- Summary
;; init-elpa.el

;;; Commentary:

;;; Code:

(require 'package)
;; 增加软件包仓库
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; ----------------------------------------------
;; Function
;; ----------------------------------------------

;; Install package if not installed
;;(defun require-package (package)
;;  (unless (package-installed-p package)
;;    (package-install package nil)))

(defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
	(if (package-installed-p package min-version)
		t
	  (if (or (assoc package package-archive-contents) no-refresh)
		  (if (boundp 'package-selected-packages)
			  ;; Record this as a package the user installed explicitly
			  (package-install package nil)
			(package-install package))
		(progn
		  (package-refresh-contents)
		          (require-package package min-version t)))))

;; Install packages if not installed
(defun require-packages (&rest packages)
  (dolist (p packages) (require-package p)))

;; 强行提前初始化ELPA。因为默认情况下Emacs在init.el加载完之后才开始初始化ELPA，
;; 而我们把大多数包的初始化函数都放在init.el中，如果不提前初始化ELPA会导致后面的
;; 初始化过程出错（对应的包文件还没有加载进来）。
(package-initialize)

(provide 'init-elpa)
;;; init-elpa.el ends here
