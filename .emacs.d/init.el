;;; Dan Schoppe's Emacs init / configuration file

;; load-path
(add-to-list 'load-path "~/.emacs.d/external")

;; Installed packages
(setq package-enable-at-startup nil)
(setq auto-installed-packages
      '(ace-jump-mode
	ag
	autopair
	buffer-move
	color-theme
	company
	exec-path-from-shell
	flx
	flx-ido
	;; flycheck
	git-gutter
	icicles
	indent-guide
	magit
	multiple-cursors
	popup
	projectile
	;; realgud
	rtags
	undo-tree
	visual-regexp
	visual-regexp-steroids
	web-mode
	))

;; Melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;; Activate all the packages
(package-initialize)

;; Fetch the list of packages available
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install the missing packages
(dolist (package auto-installed-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; Load external libraries
(load "gud-lldb.el")

;; Aliases
;; (defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(defalias 'perl-mode 'cperl-mode) ; always use cperl-mode

(defalias 'select-all 'mark-whole-buffer)
(global-set-key (kbd "C-x a") 'select-all)

;; Disable top tool-bar
(tool-bar-mode -1)

;; Auto-revert buffers if updated on disk
(global-auto-revert-mode t)

;; Save Emacs Sessions
(desktop-save-mode 1)

;; Enable winner-mode for cycling through window layouts
(winner-mode 1)

;; Disable startup screen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-scroll-output (quote first-error))
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

;; Save Emacs Sessions
(desktop-save-mode 1)

;; Enable clipboard integration
(setq x-select-enable-clipboard t)

;; Tabs
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq show-trailing-whitespace 1)

;; Text size
(set-face-attribute 'default nil :height 110) ;; value in 1/10pt, so 100 will be 10pt font

;; Disable wrapped lines
(set-default 'truncate-lines t)

;; Indent guide (vertical indentation indicators)
;; (set-face-attribute 'indent-guide-face nil
;; 		    :foreground "RoyalBlue1")
(setq indent-guide-recursive t)

;; Clean up mode-line
(when (require 'diminish nil 'noerror)
  (eval-after-load "company"
    '(diminish 'company-mode ""))
  (eval-after-load "hideshow"
    '(diminish 'hs-minor-mode ""))
  (eval-after-load "autopair"
    '(diminish 'autopair-mode ""))
  (eval-after-load "projectile"
    '(diminish 'projectile-mode ""))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode ""))
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode ""))
  (eval-after-load "git-gutter"
    '(diminish 'git-gutter-mode "")))
(setq vc-handled-backends ())

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Show current function
(which-func-mode 1)

;; Word navigation for camelCase
(global-subword-mode 1)

;; Scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

;; Reloading buffer from disk
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; Locking buffer to window
(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil "" nil
  (set-window-dedicated-p (selected-window) sticky-buffer-mode))

;; Hide Show
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(global-set-key (kbd"C-v") 'hs-toggle-hiding)

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
(add-hook 'org-mode-hook
          (lambda ()
	    (local-set-key  (kbd "C-v") 'org-cycle)))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-src-fontify-natively t)
(setq org-src-window-setup (quote current-window))
(setq org-src-ask-before-returning-to-edit-buffer nil)
(setq org-startup-indented t)
(setq org-startup-truncated nil)

;; Set PATH
(exec-path-from-shell-initialize)

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
(global-set-key (kbd "C-c |") 'mc/vertical-align-with-space)

;; Window resize
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; Window navigation
;; (global-set-key (kbd "S-<left>") 'windmove-left)
;; (global-set-key (kbd "S-<right>") 'windmove-right)
;; (global-set-key (kbd "S-<up>") 'windmove-up)
;; (global-set-key (kbd "S-<down>") 'windmove-down)
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-j") nil))
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-l") nil))
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-i") nil))
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-k") nil))
(global-set-key (kbd "M-j") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-i") 'windmove-up)
(global-set-key (kbd "M-k") 'windmove-down)

;; Toggle window dedication (similar to sticky-windows)
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
	 (set-window-dedicated-p window
				 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))
(global-set-key [pause] 'toggle-window-dedicated)

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

;; git-gutter
(global-git-gutter-mode +1)
(setq git-gutter:update-interval 1)

;; Magit (git)
(require 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

;; RTags
(require 'rtags)
(rtags-enable-standard-keybindings)

;; RTags symbols
(global-set-key (kbd "M-RET") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-c r r") 'rtags-reparse-file)
(setq rtags-tracking t)

;; RTags error checking
(rtags-diagnostics)
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-p") nil))
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-n") nil))
(global-set-key (kbd "C-c C-n") 'rtags-next-diag)
(global-set-key (kbd "C-c C-p") 'rtags-previous-diag)
(setq rtags-display-current-error-as-tooltip t)
;; (setq rtags-track-container t)
(setq rtags-error-timer-interval 0)
(set-face-attribute 'rtags-errline nil
                    :background nil
                    :underline '(:color "red" :style wave))
(set-face-attribute 'rtags-fixitline nil
                    :background nil
                    :underline '(:color "Magenta" :style wave))
(set-face-attribute 'rtags-skippedline nil
                    :background nil
                    :foreground "windowFrameColor")
(set-face-attribute 'rtags-warnline nil
                    :background nil
                    :underline '(:color "Orange" :style wave))

;; Company-rtags autocomplete
(require 'company-rtags)
(add-to-list 'company-backends 'company-rtags)
(setq company-idle-delay 1)
(setq company-rtags-begin-after-member-access t)
(setq company-minimum-prefix-length 1)
(setq rtags-completions-enabled t)
(global-set-key (kbd "<C-return>") 'company-complete)

;; Company mode
(add-hook 'after-init-hook 'global-company-mode)

;; Flycheck
;; (require 'flycheck)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;; (eval-after-load 'cc-mode
;;   #'(define-key c++-mode-map (kbd "C-c C-p") nil))
;; (eval-after-load 'cc-mode
;;   #'(define-key c++-mode-map (kbd "C-c C-n") nil))
;; (global-set-key (kbd "C-c C-n") 'flycheck-next-error)
;; (global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

;; (setq c++-mode-hook nil)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'innamespace 0)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))
            ;; (setq flycheck-check-syntax-automatically
            ;;       '(mode-enabled new-line save idle-change))
	    ;; (setq flycheck-idle-change-delay 0.1)
	    ;; (setq flycheck-display-errors-delay 0)
            ;; (setq flycheck-clang-language-standard "c++1y")
            ;; (setq flycheck-clang-standard-library "libc++")
	    ;; (setq flycheck-clang-args
	    ;; 	  '("-ferror-limit=123"))
            ;; (setq flycheck-clang-include-path
            ;;       '("/Users/danschoppe/local/Cellar/llvm/3.5.1/include/c++/v1/"
	    ;; 	    "/Users/danschoppe/local/include"
            ;;         "/Users/danschoppe/local/include/libxml2"
            ;;         "/Users/danschoppe/Code/core/dex/src"
            ;;         "/Users/danschoppe/Code/core/dex/external"
            ;;         "/Users/danschoppe/Code/core/dex/external/lz4"
            ;;         "/Users/danschoppe/Code/core/dex/build/debug"
            ;;         "/Users/danschoppe/Code/core/dex/OSX/deps"
            ;;         "/Users/danschoppe/Code/core/dex/build/msgpack/include"
	    ;; 	    "/usr/include/c++/v1"
	    ;; 	    "/home/dan/Code/core/dex/src"
	    ;; 	    "/home/dan/Code/core/dex/external"
	    ;; 	    "/home/dan/Code/core/dex/build/debug"
	    ;; 	    "/home/dan/Code/core/dex/build/msgpack/include"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term ((t (:inherit default))))
 '(which-func ((t nil))))
(put 'erase-buffer 'disabled nil)
