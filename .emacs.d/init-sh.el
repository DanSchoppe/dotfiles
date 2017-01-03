;; sh-mode (shell-script-mode)
;; web-mode hook
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)
            ))
