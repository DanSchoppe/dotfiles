;; Hide Show
(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Switching between source and header files
(add-hook 'c-mode-common-hook
					(lambda()
						(local-set-key  (kbd "C-c o") 'ff-get-other-file)))

;; Commenting and uncommenting region
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-u") nil))
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Open header files in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

;; C++ mode hook
(add-hook 'c++-mode-hook
          (lambda ()
            (setq-default c-basic-offset 2)
            (c-set-offset 'innamespace 0)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)
            (setq flycheck-check-syntax-automatically
                  '(mode-enabled new-line save idle-change))
	    (setq flycheck-idle-change-delay 0.1)
	    (setq flycheck-display-errors-delay 0)
            (setq flycheck-clang-language-standard "c++1y")
            (setq flycheck-clang-standard-library "libc++")
	    (setq flycheck-clang-args
		  '("-ferror-limit=123"))))

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-p") nil))
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-n") nil))
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)
(setq flycheck-clang-include-path
	(list
	 (expand-file-name "~/local/include")
	 (expand-file-name "~/local/include/libxml2")
	 (expand-file-name "~/local/include/libxslt")
	 (expand-file-name "~/code/core/dex_93/src")
	 (expand-file-name "~/code/core/dex_93/external")
	 (expand-file-name "~/code/core/dex_93/external/expected")
	 (expand-file-name "~/code/core/dex_93/build/debug")
	 (expand-file-name "~/code/core/dex_93/OSX/deps")
	 (expand-file-name "~/code/core/dex_93/build/debug")))

;; ;; RTags
;; (require 'rtags)
;; (rtags-enable-standard-keybindings)

;; ;; RTags symbols
;; (global-set-key (kbd "M-<return>") 'rtags-find-symbol-at-point)
;; (global-set-key (kbd "C-M-<return>") 'rtags-find-references-at-point)
;; (global-set-key (kbd "C-c r r") 'rtags-reparse-file)
;; (setq rtags-tracking t)

;; ;; RTags error checking
;; (rtags-diagnostics)
;; (eval-after-load 'cc-mode
;;   #'(define-key c++-mode-map (kbd "C-c C-p") nil))
;; (eval-after-load 'cc-mode
;;   #'(define-key c++-mode-map (kbd "C-c C-n") nil))
;; (global-set-key (kbd "C-c C-n") 'rtags-next-diag)
;; (global-set-key (kbd "C-c C-p") 'rtags-previous-diag)
;; (setq rtags-display-current-error-as-tooltip t)
;; (setq rtags-track-container t)
;; (setq rtags-error-timer-interval 0)
;; (set-face-attribute 'rtags-errline nil
;;                     :background nil
;;                     :underline '(:color "red" :style wave))
;; (set-face-attribute 'rtags-fixitline nil
;;                     :background nil
;;                     :underline '(:color "Magenta" :style wave))
;; (set-face-attribute 'rtags-skippedline nil
;;                     :background nil
;;                     :foreground "windowFrameColor")
;; (set-face-attribute 'rtags-warnline nil
;;                     :background nil
;;                     :underline '(:color "Orange" :style wave))

;; ;; Company-rtags autocomplete
;; (require 'company-rtags)
;; (add-to-list 'company-backends 'company-rtags)
;; (setq company-idle-delay 0)
;; (setq company-rtags-begin-after-member-access t)
;; (setq company-minimum-prefix-length 1)
;; (setq rtags-completions-enabled t)
;; (global-set-key (kbd "C-<return>") 'company-complete)
