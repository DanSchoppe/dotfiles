;;; Dan Schoppe's Emacs init / configuration file



;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun load-init-file (file)
	(load (locate-user-emacs-file file)))
(defun load-external-file (file)
	(load (locate-user-emacs-file (concat "external/" file))))


;; Load initializations
(load-init-file "init-package")
(load-init-file "init-alias")
(load-init-file "init-behavior")
(load-init-file "init-calc")
(load-init-file "init-funcs")
(load-init-file "init-gui")
(load-init-file "init-org")
(load-init-file "init-markdown")

;; Languages
(load-init-file "init-c")
(load-init-file "init-python")
(load-init-file "init-ruby")
(load-init-file "init-sh")
(load-init-file "init-sql")
(load-init-file "init-web")

;; Load external libraries
;; (load-external-file "realgud-lldb.el")
