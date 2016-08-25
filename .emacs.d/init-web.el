;; Hide Show
(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'web-mode-hook 'hs-minor-mode)

;; Open files in web-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.codex\\'" . web-mode))

;; web-mode hook
(add-hook 'web-mode-hook
					(lambda ()
						('hs-minor-mode)
						(setq web-mode-markup-indent-offset 2)
						(setq web-mode-css-indent-offset 2)
						(setq web-mode-code-indent-offset 2)
						))

;; Adding supported file extensions to speedbar
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".jsx"))
