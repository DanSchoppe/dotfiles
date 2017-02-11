;; Installed packages
(setq package-enable-at-startup nil)
(setq auto-installed-packages '(
  ag
  autopair
  buffer-move
  cmake-mode
  color-theme
  company-irony
  doremi-cmd
  exec-path-from-shell
  flx-ido
  flycheck
  flymake-json
  git-gutter-fringe
  hackernews
  icicles
  indent-guide
  magit
  markdown-preview-mode
  multi-term
  multiple-cursors
  popup
  projectile
  realgud
  rtags
  sql-indent
  undo-tree
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

;; Fetch the list of packages available
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Install the missing packages
(dolist (package auto-installed-packages)
  (unless (package-installed-p package)
    (package-install package)))
