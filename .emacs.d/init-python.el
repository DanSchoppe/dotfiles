;; Python mode hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-check-syntax-automatically
		  '(mode-enabled new-line save idle-change))
	    (setq flycheck-idle-change-delay 0.1)))
