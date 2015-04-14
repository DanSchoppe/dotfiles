;;; Dan Schoppe's Emacs init / configuration file

;; Tabs
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Text size
(set-face-attribute 'default nil :height 95) ;; value in 1/10pt, so 100 will be 10pt font

;; Word navigation for camelCase
(global-subword-mode 1)

;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(setq auto-installed-packages
      '(ace-jump-mode
	autopair
	color-theme
	company
	company-irony
	irony-mode
	exec-path-from-shell
	flycheck
	magit
	multiple-cursors
	projectile
	))

;; Set PATH
(exec-path-from-shell-initialize)

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

;; Magit (git)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; RTags
(require 'rtags)
(global-set-key [C-return] 'rtags-find-symbol-at-point)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;; (setq c++-mode-hook nil)
;;(add-hook 'c++-mode-hook
;;          (lambda ()
;;            (setq c-basic-offset 2)
;;            (c-set-offset 'innamespace 0)
;;            (setq tab-width 2)
;;            (setq indent-tabs-mode nil)
;;            (setq flycheck-check-syntax-automatically
;;                  '(mode-enabled new-line save))
;;            (setq flycheck-clang-language-standard "c++1y")
;;            (setq flycheck-clang-standard-library "libc++")
;;            (setq flycheck-clang-include-path
;;                  '("/usr/lib/c++/v1"
;;                    "/Users/seanbillig/local/include"
;;                    "/Users/seanbillig/local/include/libxml2"
;;                    "/Users/seanbillig/code/modeler_new/external"
;;                    "/Users/seanbillig/code/core/dex/external"
;;                    "/Users/seanbillig/code/core/newparser/external"
;;                    "/Users/seanbillig/code/core/dex_newparser/src"
;;                    "/Users/seanbillig/code/core/dex_newparser/external"
;;                    "/Users/seanbillig/code/core/dex_newparser/build/debug"
;;                    "/Users/seanbillig/code/core/dex/OSX/deps"
;;                    "/Users/seanbillig/code/core/dex/build/debug"
;;                    "/Users/seanbillig/code/core/dex/build/msgpack/include"
;;                    "/Users/seanbillig/code/core/spirit_x3/include"))))

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
(global-set-key (kbd "C-SPC") 'company-complete)

;; company-irony completion backend for irony-mode
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Buffer-move
(require 'buffer-move)
(global-set-key (kbd "M-S-<up>") 'Buf-move-up)
(global-set-key (kbd "M-S-<down>")   'buf-move-down)
(global-set-key (kbd "M-S-<left>")   'buf-move-left)
(global-set-key (kbd "M-S-<right>")  'buf-move-right)
