;; sql mode hook
(add-hook 'sql-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))
