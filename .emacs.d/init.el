;;; Dan Schoppe's Emacs init / configuration file

(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(defun load-init-file (file)
	(load (locate-user-emacs-file file)))
(add-to-list 'load-path "~/.emacs.d/external")

;; Load initializations
(load-init-file "init-alias")
(load-init-file "init-behavior")
(load-init-file "init-calc")
(load-init-file "init-funcs")
(load-init-file "init-git")
(load-init-file "init-gui")
(load-init-file "init-org")
(load-init-file "init-markdown")
(load-init-file "init-copilot")

;; ;; Languages
;; (load-init-file "init-c")
;; (load-init-file "init-python")
;; (load-init-file "init-ruby")
;; (load-init-file "init-sh")
;; (load-init-file "init-sql")
(load-init-file "init-web")
