;; Installed packages
(setq package-enable-at-startup nil)
(setq auto-installed-packages '(
  add-node-modules-path
  ag
  buffer-move
  cmake-mode
  color-theme
  company-irony
  company-rtags
  company-terraform
  cquery
  docker
  dockerfile-mode
  doremi-cmd
  exec-path-from-shell
  flx-ido
  flycheck
  flycheck-pycheckers
  flymake-json
  git-gutter-fringe
  hackernews
  helm-ag
  helm-rtags
  icicles
  impatient-mode
  indent-guide
  magit
  magit-gh-pulls
  markdown-mode
  markdown-preview-mode
  mocha
  multi-term
  multiple-cursors
  popup
  realgud
  restclient
  rspec-mode
  rtags
  rudel
  rvm
  shackle
  solarized-theme
  sql-indent
  terraform-mode
  toml-mode
  undo-tree
  visual-regexp-steroids
  web-mode
  yaml-mode
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

;; Load path
(package-initialize)

;; Install the missing packages
(dolist (package auto-installed-packages)
  (unless (package-installed-p package)
    (package-install package)))
