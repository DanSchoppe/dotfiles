;; Hide Show
(add-hook 'js-mode-hook 'hs-minor-mode)
(add-hook 'web-mode-hook 'hs-minor-mode)

(setq-default js-indent-level 2)

;; Open files in web-mode
(add-to-list 'auto-mode-alist '("\\.codex\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

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
            ))

;; Disable backtick (`) syntax highlighting for *.codex files
(add-hook 'find-file-hook 'codex-backtick-syntax-highlighting)
(defun codex-backtick-syntax-highlighting ()
  (when (string= (file-name-extension buffer-file-name) "codex")
    (modify-syntax-entry ?` " " js-mode-syntax-table)))

;; Adding supported file extensions to speedbar
(eval-after-load "speedbar" '(speedbar-add-supported-extension ".jsx"))

(defun my-project-hook ()
  (when (string= (file-name-extension buffer-file-name) "ts")
    (typescript-mode)
    (tss-setup-current-buffer)))
