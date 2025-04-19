;; Aliases
;; (defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'list-buffers 'ibuffer) ; always use ibuffer
(defalias 'perl-mode 'cperl-mode) ; always use cperl-mode

(defalias 'select-all 'mark-whole-buffer)
(global-set-key (kbd "C-x a") 'select-all)
