;; Set PATH
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Save Emacs Sessions
(desktop-save-mode 1)

;; Disable view-emacs-news
(global-unset-key (kbd "C-h C-n"))

;; Auto-revert buffers if updated on disk
(global-auto-revert-mode t)
;; Reloading buffer from disk
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; Enable clipboard integration
(setq x-select-enable-clipboard t)

;; Configure file backups
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.saves"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups;; Tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-trailing-whitespace 1)

;; Disable wrapped lines
(set-default 'truncate-lines t)

;; Word navigation for camelCase
(global-subword-mode 1)

;; Locking buffer to window
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil "" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; Unique buffer name scheme
(setq uniquify-buffer-name-style "post-forward")
(setq uniquify-separator "=")

;; Electric pair (autopair)
(electric-pair-mode 1)

;; Matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Cycling buffers
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

;; Auto-indenting yanked text
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key (kbd "C-y") 'yank-and-indent)

;; Hide Show
(global-set-key (kbd"C-v") 'hs-toggle-hiding)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c |") 'mc/vertical-align-with-space)

;; Regex
(require 'visual-regexp-steroids)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-M-5") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-M-m") 'vr/mc-mark)
;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
(define-key global-map (kbd "C-M-r") 'vr/isearch-backward)
(define-key global-map (kbd "C-M-s") 'vr/isearch-forward)

;; Terminal
'(compilation-scroll-output (quote first-error))
;; '(term ((t (:inherit default))))

;; Tramp
'(tramp-auto-save-directory "/tmp")

;; Company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
