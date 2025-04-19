(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Color theme
(load-theme 'solarized-dark t)

;; Make PATH available:
(exec-path-from-shell-initialize)

;; save sessions between restarts
(desktop-save-mode 1)

;; Automatically revert buffers when the file changes on disk
(global-auto-revert-mode t)

;; Prevent backup file creation:
(setq make-backup-files nil)

;; Tabs and Whitespace
(setq-default indent-tabs-mode nil
              tab-width 2
              require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Enable window layout undo / redo:
(winner-mode 1)

;; Turn javascript-mode on for *scratch*:
(with-current-buffer "*scratch*" (javascript-mode))
(setq initial-scratch-message nil)

;; Disable wrapped lines
(set-default 'truncate-lines t)

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Enable electric-pair-mode for auto-pairing
(electric-pair-mode 1)

;; Show matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Word navigation for camelCase
(global-subword-mode 1)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c |") 'mc/vertical-align-with-space)

;; Keybindings
(global-set-key (kbd "C-x a") 'mark-whole-buffer) ;; select-all
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up-command 4)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down-command 4)))

;; Buffer switching
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)

;; Buffer-move
(require 'buffer-move)
(global-set-key (kbd "C-M-i") 'buf-move-up)
(global-set-key (kbd "C-M-k")  'buf-move-down)
(global-set-key (kbd "C-M-j")  'buf-move-left)
(global-set-key (kbd "C-M-l")  'buf-move-right)

;; Window navigation
(global-set-key (kbd "M-j") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-i") 'windmove-up)
(global-set-key (kbd "M-k") 'windmove-down)

;; Window resizing
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; Comment / uncomment regions:
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)


;; Use ibuffer for listing open buffers
(defalias 'list-buffers 'ibuffer)

;; Show git line symbols
(global-git-gutter-mode 1)

;; Helm completion
(require 'helm)
(setq helm-display-buffer-default-height 0.3)
(setq helm-echo-input-in-header-line t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-s") 'helm-occur)

;; Avoid performance issues in files with very long lines.
(global-so-long-mode 1)

;; GUI-related Customizations
(when (display-graphic-p)
  ;; Set font size
  (set-face-attribute 'default nil :height 130)

  ;; Disable startup screen and toolbar
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)

  ;; Maximize window on startup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
)

(provide 'init)
