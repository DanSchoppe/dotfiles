;;; Dan Schoppe's Emacs init / configuration file

;; Save Emacs Sessions
(desktop-save-mode 1)

;; Disable startup screen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(js3-auto-indent-p t)
 '(js3-consistent-level-indent-inner-bracket nil)
 '(js3-curly-indent-offset 0)
 '(js3-enter-indents-newline t)
 '(js3-global-externs (quote ("require" "console")))
 '(js3-idle-timer-delay 0.4)
 '(js3-indent-on-enter-key t)
 '(js3-pretty-vars nil)
 '(js3-strict-trailing-comma-warning nil))

;; Tabs
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-trailing-whitespace 1)

;; Text size
(set-face-attribute 'default nil :height 95) ;; value in 1/10pt, so 100 will be 10pt font

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Word navigation for camelCase
(global-subword-mode 1)

;; Scrolling
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

;; Moving between windows with Shift+Arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Reloading buffer from disk
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; Matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Switching between source and header files
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-get-other-file)))

;; Commenting and uncommenting region
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-u") nil))
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Auto-indenting yanked text
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))
(global-set-key (kbd "C-y") 'yank-and-indent)

;; Open header files in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
;; Open codex files in js-mode
(add-to-list 'auto-mode-alist '("\\.codex\\'" . js-mode))
(add-hook 'js-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq js-indent-level 2)
            ;; (smart-tabs-mode-enable)
            ;; (smart-tabs-advice js-indent-line js-indent-level)
            ))

;; Org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-src-fontify-natively t)
(setq org-src-window-setup (quote current-window))
(setq org-src-ask-before-returning-to-edit-buffer nil)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; Packages
(package-initialize)
(setq package-enable-at-startup nil)
(setq auto-installed-packages
      '(ace-jump-mode
	autopair
	buffer-move
	color-theme
	company
	company-irony
	irony-mode
	exec-path-from-shell
	flx-ido
	flycheck
	magit
	multiple-cursors
	projectile
	rtags
	undo-tree
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

;; Dired Extra library (C-x C-j for instance)
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

;; Undo tree
(require 'undo-tree)
(global-undo-tree-mode 1)

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
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; Window navigation
(global-set-key (kbd "C-c C-s") 'windmove-left)
(global-set-key (kbd "C-c C-d") 'windmove-down)
(global-set-key (kbd "C-c C-f") 'windmove-right)
(global-set-key (kbd "C-c C-e") 'windmove-up)

;; Buffer-move
(require 'buffer-move)
(global-set-key (kbd "M-S-<up>") 'buf-move-up)
(global-set-key (kbd "M-S-<down>")   'buf-move-down)
(global-set-key (kbd "M-S-<left>")   'buf-move-left)
(global-set-key (kbd "M-S-<right>")  'buf-move-right)

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
(defun ido-define-keys () ;; C-n/p for selecting buffer
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
;; Display results vertically rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; Autopair (bracket matching)
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;; Magit (git)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; RTags
(require 'rtags)
(global-set-key (kbd "M-RET") 'rtags-find-symbol-at-point)

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "C-x C-n") 'flycheck-next-error)
(global-set-key (kbd "C-x C-p") 'flycheck-previous-error)

;; (setq c++-mode-hook nil)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'innamespace 0)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)
            (setq flycheck-check-syntax-automatically
                  '(mode-enabled new-line save idle-change))
	    (setq flycheck-idle-change-delay 0.1)
            (setq flycheck-clang-language-standard "c++1y")
            (setq flycheck-clang-standard-library "libc++")
            (setq flycheck-clang-include-path
                  '("/Users/danschoppe/local/Cellar/llvm/3.5.1/include/c++/v1"
		    "/Users/danschoppe/local/include"
                    "/Users/danschoppe/local/include/libxml2"
                    "/Users/danschoppe/Code/core/dex/src"
                    "/Users/danschoppe/Code/core/dex/external"
                    "/Users/danschoppe/Code/core/dex/build/debug"
                    "/Users/danschoppe/Code/core/dex/OSX/deps"
                    "/Users/danschoppe/Code/core/dex/build/msgpack/include"))))


;;;; irony-mode smart auto-completion
;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)
;;;; replace the `completion-at-point' and `complete-symbol' bindings in
;;;; irony-mode's buffers by irony-mode's function
;;(defun my-irony-mode-hook ()
;;  (define-key irony-mode-map [remap completion-at-point]
;;    'irony-completion-at-point-async)
;;  (define-key irony-mode-map [remap complete-symbol]
;;    'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;(global-set-key (kbd "<C-return>") 'company-complete)
;;
;;;; company-irony completion backend for irony-mode
;;(eval-after-load 'company
;;  '(add-to-list 'company-backends 'company-irony))
;;;; (optional) adds CC special commands to `company-begin-commands' in order to
;;;; trigger completion at interesting places, such as after scope operator
;;;;     std::|
;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Company-rtags
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-rtags))
(setq rtags-completions-enabled t)
(global-set-key (kbd "<C-return>") 'company-complete)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)







(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term ((t (:inherit default)))))
(put 'erase-buffer 'disabled nil)
