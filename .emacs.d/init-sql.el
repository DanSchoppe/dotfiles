;; sql mode hook
(add-hook 'sql-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))

(eval-after-load "sql"
  (load-library "sql-indent"))

(require 'sql-indent)
(setq-default sql-indent-offset 1)
