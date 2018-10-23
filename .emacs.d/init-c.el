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
            (setq-default c-default-style "linux")
            (c-set-offset 'innamespace 0)
            (setq tab-width 2)
            (setq indent-tabs-mode nil)
            ;; (lsp-cquery-enable)
            ;; (lsp-ui-mode)
            ;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
            ;; (setq flycheck-check-syntax-automatically
            ;;       '(mode-enabled new-line save idle-change))
            ;; (setq flycheck-idle-change-delay 0.1)
            ;; (setq flycheck-display-errors-delay 0)
            ;; (setq flycheck-clang-language-standard "c++1y")
            ;; (setq flycheck-clang-standard-library "libc++")
            ;; (setq flycheck-clang-args
            ;;       '("-ferror-limit=123"))
            )
          )

;; Flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-p") nil))
(eval-after-load 'cc-mode
  #'(define-key c++-mode-map (kbd "C-c C-n") nil))
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

;; ;; RTags
;; (require 'rtags)
;; (rtags-enable-standard-keybindings)
;;
;; ;; RTags symbols
;; (global-set-key (kbd "M-<return>") 'rtags-find-symbol-at-point)
;; (global-set-key (kbd "C-M-<return>") 'rtags-find-references-at-point)
;; (global-set-key (kbd "C-c r r") 'rtags-reparse-file)
;;
;; ;; RTags error checking
;; (setq rtags-autostart-diagnostics t)
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
;;
;; ;; Company-rtags autocomplete
;; (require 'company-rtags)
;; (push 'company-rtags company-backends)
;; (setq company-idle-delay 3)
;; (setq company-rtags-begin-after-member-access t)
;; (setq company-minimum-prefix-length 1)
;; (setq rtags-completions-enabled t)
;; (global-set-key (kbd "C-<return>") 'company-complete)
;; (setq rtags-display-result-backend 'helm)

;; cquery
;; https://github.com/cquery-project/cquery/wiki/Emacs
;; (setq cquery-executable "/path/to/cquery/build/release/bin/cquery")
;; (push 'company-lsp company-backends)
;; (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
;; (setq cquery-sem-highlight-method 'font-lock)
;; alternatively:
;; (setq cquery-sem-highlight-method 'overlay)
;; rainbow semantic highlighting:
;; (cquery-use-default-rainbow-sem-highlight)
;; (setq lsp-ui-doc-enable nil)
;; (setq lsp-ui-sideline-enable nil)
;; (setq lsp-ui-peek-always-show t)
