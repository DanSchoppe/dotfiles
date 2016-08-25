;; Case insensitive line sorting
(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;; Dired Extra library (C-x C-j for instance)
(require 'dired-x)
; Load Dired X when Dired is loaded:
(add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(setq dired-omit-mode t) ; Turn on Omit mode.

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Ace jump (Quick Emacs cursor navigation)
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Projectile (project definition)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; Ag (silver searcher)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(setq ag-reuse-window t)

;; Magit (git)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-x C-g") 'magit-status)
