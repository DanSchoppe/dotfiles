;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable top tool-bar
(tool-bar-mode -1)

;; Enable winner-mode for cycling through window layouts
(winner-mode 1)

;; Text size
(set-face-attribute 'default nil :height 110) ;; value in 1/10pt, so 100 will be 10pt font

;; Maximize window on startup
(toggle-frame-maximized)

;; Configure scratch buffer
(setq initial-major-mode 'javascript-mode)
(setq initial-scratch-message nil)

;; Color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-classic)

;; Clean up mode-line
(when (require 'diminish nil 'noerror)
  (eval-after-load "company"
    '(diminish 'company-mode ""))
  (eval-after-load "hideshow"
    '(diminish 'hs-minor-mode ""))
  (eval-after-load "projectile"
    '(diminish 'projectile-mode ""))
  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode ""))
  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode ""))
  (eval-after-load "git-gutter"
    '(diminish 'git-gutter-mode "")))
(setq vc-handled-backends ())

;; Display function name in mode-line
(which-func-mode 1)

;; Show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Show current function
(which-function-mode 1)

;; Scrolling
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(eval-after-load 'eww-mode #'(define-key eww-mode-map (kbd "M-n") nil))
(eval-after-load 'eww-mode #'(define-key eww-mode-map (kbd "M-p") nil))
(global-set-key "\M-n"  (lambda () (interactive) (scroll-up   4)) )
(global-set-key "\M-p"  (lambda () (interactive) (scroll-down 4)) )

;; Window splitting preferences
(setq split-height-threshold 160)
(setq split-width-threshold nil)

;; Indent guide (vertical indentation indicators)
;; (set-face-attribute 'indent-guide-face nil
;;        :foreground "RoyalBlue1")
;; (setq indent-guide-recursive t)

;; Window resize
(global-set-key (kbd "M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "M-<down>") 'shrink-window)
(global-set-key (kbd "M-<up>") 'enlarge-window)

;; Window navigation
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-j") nil))
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-l") nil))
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-i") nil))
(eval-after-load 'cc-mode #'(define-key c++-mode-map (kbd "M-k") nil))
(eval-after-load 'calc-mode #'(define-key c++-mode-map (kbd "M-j") nil))
(eval-after-load 'calc-mode #'(define-key c++-mode-map (kbd "M-l") nil))
(eval-after-load 'calc-mode #'(define-key c++-mode-map (kbd "M-i") nil))
(eval-after-load 'calc-mode #'(define-key c++-mode-map (kbd "M-k") nil))
(global-set-key (kbd "M-j") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-i") 'windmove-up)
(global-set-key (kbd "M-k") 'windmove-down)

;; Buffer-move
(require 'buffer-move)
(global-set-key (kbd "C-M-i") 'buf-move-up)
(global-set-key (kbd "C-M-k")  'buf-move-down)
(global-set-key (kbd "C-M-j")  'buf-move-left)
(global-set-key (kbd "C-M-l")  'buf-move-right)

;; Proper ANSI colors in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; git-gutter
(global-git-gutter-mode +1)
(setq git-gutter:update-interval 1)

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
