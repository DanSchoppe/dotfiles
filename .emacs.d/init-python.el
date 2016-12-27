;; Python mode hook
(add-hook 'python-mode-hook
	  (lambda ()
	    (setq flycheck-check-syntax-automatically
		  '(mode-enabled new-line save idle-change))
	    (setq flycheck-idle-change-delay 0.1)))

;; Commenting and uncommenting region
(eval-after-load 'python-mode
  #'(define-key python-mode-map (kbd "C-c C-c") nil))
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
