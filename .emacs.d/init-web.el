;; Open files in web-mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(setq-default js-indent-level 2)

;; Support JSX formatting
(setq web-mode-content-types-alist
      '(("jsx" . "\\.js[x]?\\'")))

;; Open files in restclient-mode
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; web-mode hook
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            ;; Fix Promise indentation
            ;; https://www.bountysource.com/issues/40358797-indentation-in-javascript-promise-and-other-chains
            (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
            (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
            ;; Comment using // style instead of block comment /* */ style
            (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
            (add-to-list 'web-mode-comment-formats '("javascript" . "//" ))
            (add-node-modules-path)
            ))

;; standardfmt
;; Note: Had to modify node_modules to work around bug:
;; https://github.com/standard/standard/issues/1384#issuecomment-596027583
(require 'standardfmt)
(add-hook 'web-mode-hook #'standardfmt-mode)

;; Hide Show
(add-hook 'web-mode-hook 'hs-minor-mode)

;; Adding supported file extensions to speedbar
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".jsx"))

;; (defun my-project-hook ()
;;   (when (string= (file-name-extension buffer-file-name) "ts")
;;     (typescript-mode)
;;     (tss-setup-current-buffer)))
(add-hook 'typescript-mode-hook
          (lambda ()
            (setq typescript-indent-level 2)
            (add-node-modules-path)
            ))

;; Terraform
(require 'company-terraform)
(company-terraform-init)

;; Flycheck
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (flycheck-add-mode 'javascript-eslint 'typescript-mode)

;; Tide
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(setq tide-always-show-documentation t)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
