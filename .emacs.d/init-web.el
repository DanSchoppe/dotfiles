;; Hide Show
(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'web-mode-hook 'hs-minor-mode)

(setq-default js-indent-level 2)

;; Open files in web-mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Open files in restclient-mode
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; web-mode hook
(add-hook 'web-mode-hook
          (lambda ()
            ;; ('hs-minor-mode)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            ;; Fix Promise indentation
            ;; https://www.bountysource.com/issues/40358797-indentation-in-javascript-promise-and-other-chains
            (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
            ;; Comment using // style instead of block comment /* */ style
            (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
            (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
            (add-node-modules-path)
            ))

;; Adding supported file extensions to speedbar
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".jsx"))

;; (defun my-project-hook ()
;;   (when (string= (file-name-extension buffer-file-name) "ts")
;;     (typescript-mode)
;;     (tss-setup-current-buffer)))

;; Terraform
(require 'company-terraform)
(company-terraform-init)

;; Flycheck
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)
