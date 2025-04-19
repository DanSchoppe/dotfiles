;; (require 'quelpa-use-package)
;;
;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "zerolfx/copilot.el"
;;                    :branch "main"
;;                    :files ("dist" "*.el")))
;; (add-hook 'prog-mode-hook 'copilot-mode)
;;
(add-hook 'prog-mode-hook
          (lambda ()
            (with-eval-after-load 'copilot
              (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion))))
