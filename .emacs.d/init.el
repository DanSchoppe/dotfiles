;;; Dan Schoppe's Emacs init / configuration file

;; Set PATH
;; (exec-path-from-shell-initialize)

;; Load path
(add-to-list 'load-path (locate-user-emacs-file "external"))
(defun load-init-file (file)
	(load (locate-user-emacs-file file)))
(defun load-external-file (file)
	(load (locate-user-emacs-file "external" file)))


;; Load initializations
(load-init-file "init-package")
(load-init-file "init-alias")
(load-init-file "init-behavior")
(load-init-file "init-funcs")
(load-init-file "init-gui")
(load-init-file "init-org")

;; Languages
(load-init-file "init-c")
(load-init-file "init-web")
(load-init-file "init-python")
(load-init-file "init-sql")

;; Load external libraries
;; (load-external-file "gud-lldb.el")
