(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t) ;; Automatically install missing packages

;; Load custom file
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(let ((system-file (expand-file-name "~/.emacs.d/system.el")))
  (when (file-exists-p system-file)
    (load system-file)))

(use-package exec-path-from-shell
  :demand t ;; Load immediately, don't defer
  :config
  (exec-path-from-shell-initialize)) ;; $PATH

;; =====================
;; General Configuration
;; =====================
(setq inhibit-startup-screen t)

;; Don't auto-save or lock files:
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; GUI app-specific customization:
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 140) ;; Set font size
  (tool-bar-mode -1) ;; Disable toolbar
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-dark t)))

;; Use ibuffer for buffer listing
(defalias 'list-buffers 'ibuffer)

;; Disable backup files
(setq make-backup-files nil)

;; Whitespace
(setq-default indent-tabs-mode nil
              tab-width 2
              require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Line visualization
(setq truncate-lines t)
(setq line-number-mode t
      column-number-mode t)

;; Parentheses
(electric-pair-mode 1) ;; Auto-pair brackets
(show-paren-mode 1) ;; Highlight matching parentheses
(setq show-paren-delay 0)

;; Revert buffers on disk changes
(global-auto-revert-mode t)
(global-set-key (kbd "C-x C-r") 'revert-buffer)

;; Handle long lines gracefully
(global-so-long-mode 1)

;; Undo/redo window layouts
(winner-mode 1)

;; Session saving
(desktop-save-mode 1) ;; Save sessions
(setq desktop-restore-eager 5) ;; Reduce startup time: only restore 5 buffers immediately, rest lazily

;; camelCase cursor navigation
(global-subword-mode 1)

;; Make *scratch* more useful
(with-current-buffer "*scratch*" (javascript-mode))
(setq initial-scratch-message nil)

;; Squelch warnings about native compilation failures; it'll fall back to byte-compiled code
(setq native-comp-async-report-warnings-errors nil)

;; =====================
;; Package Configuration
;; =====================
;; Helm
(use-package helm
  :bind (("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-M-s" . helm-occur))
  :custom
  (helm-split-window-inside-p t)  ; Split inside current window
  (helm-echo-input-in-header-line t)
  (helm-M-x-fuzzy-match t)
  :config
  (helm-mode 1))

;; Project search
(use-package ag
  :bind ("C-c s" . ag-project)
  :config
  (setq ag-highlight-search t
        ag-reuse-buffers t
        ag-reuse-window t
        ag-arguments '("--smart-case" "--stats" "--hidden")))

;; Company: Auto-completion popup
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2))

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

;; Undo tree
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

;; Buffer Move
(use-package buffer-move
  :bind (("C-M-i" . buf-move-up)
         ("C-M-k" . buf-move-down)
         ("C-M-j" . buf-move-left)
         ("C-M-l" . buf-move-right)))

;; Magit
(use-package magit)

;; =====================
;; Custom Keybindings
;; =====================
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

;; =====================
;; Note taking
;; =====================
(use-package markdown-mode)

;; Live preview in Emacs using eww (built-in browser)
(defun my/markdown-preview ()
  "Preview markdown in eww."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (shell-command (format "pandoc -f markdown -t html %s -o /tmp/markdown-preview.html" file-name))
      (if (get-buffer "*eww*")
          (with-current-buffer "*eww*"
            (eww-reload))
        (eww-open-file "/tmp/markdown-preview.html")))))

(defun my/markdown-preview-auto ()
  "Auto-update markdown preview on save."
  (when (get-buffer "*eww*")
    (shell-command (format "pandoc -f markdown -t html %s -o /tmp/markdown-preview.html" (buffer-file-name)))
    (with-current-buffer "*eww*"
      (eww-reload))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-p") 'my/markdown-preview)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'my/markdown-preview-auto nil t))))

;; =====================
;; Programming
;; =====================
(use-package typescript-mode)
(use-package yaml-mode)
(use-package markdown-mode)
(use-package dotenv-mode)
(use-package terraform-mode)
;; javascript-mode, python-mode, and sh-mode are built-in to Emacs

;; Components (JavaScript focus, for example):
;;   - Tree-sitter (js-ts-mode/tsx-ts-mode): Fast, accurate syntax highlighting
;;   - Eglot: Built-in LSP client for IDE features
;;   - typescript-language-server: Language server (handles JS/JSX/TS/TSX) from homebrew
;;   - Company: Auto-completion popup UI
;;
;; Capabilities:
;;   - Syntax highlighting via tree-sitter grammars
;;   - Real-time diagnostics (linting/type errors)
;;   - Jump to definition (M-.) and back (M-,)
;;   - Hover documentation (eldoc-mode)
;;   - Rename refactoring (M-x eglot-rename)
;;   - Code actions (M-x eglot-code-actions)
;;   - Auto-completion from LSP (company-mode)

;; Ensure language servers are available (from Homebrew or whatever):
;;   - JavaScript/TypeScript: typescript-language-server
;;   - Python: pyright
;;   - JSON: vscode-json-language-server (via vscode-langservers-extracted)
;;   - YAML: yaml-language-server
;;   - Bash: bash-language-server

;; Eglot: Lightweight LSP client
(use-package eglot
  :hook ((js-ts-mode . eglot-ensure)
       (tsx-ts-mode . eglot-ensure)
       (typescript-ts-mode . eglot-ensure)
       (python-ts-mode . eglot-ensure)
       (json-ts-mode . eglot-ensure)
       (yaml-ts-mode . eglot-ensure)
       (bash-ts-mode . eglot-ensure)))

;; Tree-sitter for syntax highlighting
(when (functionp 'treesit-available-p)
  (defun my/ensure-treesit-grammar (language)
    "Install tree-sitter LANGUAGE grammar if not available."
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language)))

  (setq treesit-language-source-alist
        '((javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (python "https://github.com/tree-sitter/tree-sitter-python" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json" "master" "src")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master" "src")
          (bash "https://github.com/tree-sitter/tree-sitter-bash" "master" "src")))

  ;; These get installed to ~/.emacs.d/tree-sitter/
  (my/ensure-treesit-grammar 'javascript)
  (my/ensure-treesit-grammar 'typescript)
  (my/ensure-treesit-grammar 'tsx)
  (my/ensure-treesit-grammar 'python)
  (my/ensure-treesit-grammar 'json)
  (my/ensure-treesit-grammar 'yaml)
  (my/ensure-treesit-grammar 'bash)

  (setq major-mode-remap-alist
        '((javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (python-mode . python-ts-mode)
          (js-json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (sh-mode . bash-ts-mode)))
 (setq treesit-font-lock-level 4))

;; JavaScript
;; JavaScript configuration
(setq js-indent-level 2)

;; Restclient mode for .http files
(use-package restclient
  :mode ("\\.http\\'"))

(provide 'init)
