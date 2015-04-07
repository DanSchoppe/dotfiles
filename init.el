;; Tabs
(setq indent-tabs-mode nil)
(setq tab-width 2)

;; Melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Ace jump
(add-to-list 'load-path "~/emacs.d/elpa/ace-jump-mode-20140616.115/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-classic)
