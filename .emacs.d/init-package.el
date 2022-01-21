;; Installed packages
(setq package-enable-at-startup nil)
(setq auto-installed-packages '(
  add-node-modules-path
  ag
  atomic-chrome
  buffer-move
  cmake-mode
  color-theme
  company-irony
  company-rtags
  company-terraform
  cquery
  docker
  dockerfile-mode
  exec-path-from-shell
  flx-ido
  flycheck
  flycheck-pycheckers
  flymake-json
  git-gutter-fringe
  hackernews
  helm-ag
  helm-rtags
  impatient-mode
  lsp-ui
  magit
  magit-gh-pulls
  magit-popup
  markdown-mode
  markdown-preview-mode
  mocha
  multi-term
  multiple-cursors
  popup
  rainbow-mode
  realgud
  restclient
  rspec-mode
  rtags
  rudel
  rvm
  scala-mode
  shackle
  solarized-theme
  sql-indent
  string-inflection
  sublimity
  tide
  terraform-mode
  toml-mode
  undo-tree
  visual-regexp-steroids
  web-mode
  wsd-mode
  xml-rpc
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

;; Install the missing packages
(dolist (package auto-installed-packages)
  (unless (package-installed-p package)
    (package-install package)))
