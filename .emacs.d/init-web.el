;; Open files in javascript-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . javascript-mode))

(setq-default js-indent-level 2)

;; standardfmt
(require 'standardfmt)
(add-hook 'javascript-mode-hook #'standardfmt-mode)

;; Configuration based largely on:
;; https://emacs-lsp.github.io/lsp-mode/tutorials/reactjs-tutorial/#installing-the-typescript-language-server
(add-hook 'prog-mode-hook #'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'

;; Open files in restclient-mode
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))
