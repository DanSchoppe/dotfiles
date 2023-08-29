;; python shell
(setq python-shell-interpreter "python")

(require 'flycheck)
(require 'flycheck-pycheckers)
(add-hook 'after-init-hook #'global-flycheck-mode)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)
(setq flycheck-pycheckers-venv-root "~/.local/share/virtualenvs")
(setq flycheck-pycheckers-checkers '(pylint mypy3))
;; Python mode hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-check-syntax-automatically
		  '(mode-enabled new-line save idle-change))
	    (setq flycheck-idle-change-delay 0.1)))

;; Commenting and uncommenting region
(add-hook 'python-mode-hook (lambda ()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq python-indent-offset 4)))

(eval-after-load 'python
  #'(define-key python-mode-map (kbd "C-c C-c") nil))
(eval-after-load 'python-mode
  #'(define-key python-mode-map (kbd "C-c C-u") nil))
