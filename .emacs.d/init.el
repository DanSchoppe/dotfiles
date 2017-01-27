;;; Dan Schoppe's Emacs init / configuration file

;; Load path

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

;; Languages
(load-init-file "init-c")
(load-init-file "init-web")
(load-init-file "init-python")
(load-init-file "init-sql")
(load-init-file "init-sh")

;; Load external libraries
(load-external-file "gud-lldb.el")
