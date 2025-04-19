(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Fetch latest package list
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Automatically install missing packages

;; Load custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; =====================
;; General Configuration
;; =====================
(setq make-backup-files nil) ;; Disable backup files
(setq-default indent-tabs-mode nil
              tab-width 2
              require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq truncate-lines t) ;; Disable wrapped lines
(setq line-number-mode t
      column-number-mode t)
(electric-pair-mode 1) ;; Auto-pair brackets
(show-paren-mode 1) ;; Highlight matching parentheses
(setq show-paren-delay 0)
(global-auto-revert-mode t) ;; Revert buffers on disk changes
(global-so-long-mode 1) ;; Handle long lines gracefully
(winner-mode 1) ;; Undo/redo window layouts
(desktop-save-mode 1) ;; Save sessions
(global-subword-mode 1) ;; Better camelCase navigation
(with-current-buffer "*scratch*" (javascript-mode))
(setq initial-scratch-message nil)

;; =====================
;; GUI Customization
;; =====================
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 130) ;; Set font size
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1) ;; Disable toolbar
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (load-theme 'solarized-dark t))
(defalias 'list-buffers 'ibuffer) ;; Use ibuffer for buffer listing

;; =====================
;; Package Configuration
;; =====================
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)) ;; $PATH

;; Helm
(use-package helm
  :defer t
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-M-s" . helm-occur))
  :custom
  (helm-display-buffer-default-height 0.3)
  (helm-echo-input-in-header-line t)
  (helm-M-x-fuzzy-match t)
  :config
  (helm-mode 1))

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c |" . mc/vertical-align-with-space)))

;; Git Gutter
(use-package git-gutter
  :defer t
  :custom
  (git-gutter:update-interval 2)
  :config
  (global-git-gutter-mode 1))

;; Buffer Move
(use-package buffer-move
  :bind (("C-M-i" . buf-move-up)
         ("C-M-k" . buf-move-down)
         ("C-M-j" . buf-move-left)
         ("C-M-l" . buf-move-right)))

;; ===================
;; Custom Keybindings
;; ===================
(global-set-key (kbd "C-x C-p") 'previous-buffer)
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x a") 'mark-whole-buffer) ;; Select all
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up-command 4)))
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down-command 4)))
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)
(global-set-key (kbd "M-j") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-i") 'windmove-up)
(global-set-key (kbd "M-k") 'windmove-down)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

(provide 'init)
