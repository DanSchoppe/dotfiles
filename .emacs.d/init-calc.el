;; Start calculator in total algebraic mode
(add-hook 'calc-mode-hook
          (lambda()
						(calc-total-algebraic-mode)))
