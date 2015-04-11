;;; Dan Schoppe's Emacs init / configuration file

;; Tabs
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Text size
(set-face-attribute 'default nil :height 95) ;; value in 1/10pt, so 100 will be 10pt font

;; Melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Ace jump (Quick Emacs cursor navigation)
(add-to-list 'load-path "~/emacs.d/elpa/ace-jump-mode-20140616.115/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-classic)

;; Multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Window resize
(global-set-key (kbd "S-C-<left>") 'Shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Flycheck (error checking)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Projectile (project definition)
(projectile-global-mode)
(setq projectile-enable-caching t)

;; flx-ido (Improved flex matching for text completion system)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Autopair (bracket matching)
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; irony-mode smart auto-completion
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(add-hook 'after-init-hook 'global-company-mode)

;;;; Yasnippet and Autocomplete
;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(require 'auto-complete-config)
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;;
;;(ac-config-default)
;;;;; set the trigger key so that it can work together with yasnippet on tab key,
;;;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;;;; activate, otherwise, auto-complete will
;;(ac-set-trigger-key "TAB")
;;(ac-set-trigger-key "<tab>")
;;
;;(require 'auto-complete-clang)
;;;;(global-set-key (kbd "C-`") 'ac-complete-clang)
;;;;(setq ac-auto-start nil)
;;;;(setq ac-quick-help-delay 0.2)
;;(define-key ac-mode-map  [(control tab)] 'auto-complete)
;;(defun my-ac-config ()
;;  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;  ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;  (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;  (global-auto-complete-mode t))
;;(defun my-ac-cc-mode-setup ()
;;  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
;;(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;;;; ac-source-gtags
;;(my-ac-config)

;; CEDET (Collection of Emacs Development Environment Tools)
;;(load-file "~/Applications/cedet-1.1/common/cedet.el")
;;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;;(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; Buffer-move
(require 'buffer-move)
(global-set-key (kbd "M-S-<up>") 'Buf-move-up)
(global-set-key (kbd "M-S-<down>")   'buf-move-down)
(global-set-key (kbd "M-S-<left>")   'buf-move-left)
(global-set-key (kbd "M-S-<right>")  'buf-move-right)
