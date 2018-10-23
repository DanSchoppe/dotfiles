(require 'rvm)

;; Enable hs
(add-hook 'ruby-mode-hook
          (lambda () (hs-minor-mode)))

;; use rvm's default ruby for the current Emacs session
(rvm-use-default)

(setq rspec-use-rvm t)
(setq rspec-use-spring-when-possible nil)
