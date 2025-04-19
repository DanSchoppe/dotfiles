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

;; Ag (silver searcher)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(setq ag-reuse-window t)
