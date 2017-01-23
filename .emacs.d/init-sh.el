;; sh-mode (shell-script-mode)
;; web-mode hook
(add-hook 'sh-mode-hook
          (lambda ()
            (setq sh-basic-offset 2)
            (setq sh-indentation 2)
            ))

;; shell inside of emacs
(add-hook 'shell-mode-hook
          (lambda ()
            'ansi-color-for-comint-mode-on
            (setq)))
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(setq multi-term-program "/Users/danschoppe/local/bin/zsh")
