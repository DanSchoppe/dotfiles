(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-switch-indent-offset 2)
 '(package-selected-packages
   '(exec-path-from-shell company-terraform editorconfig quelpa-use-package use-package quelpa yasnippet zenburn-theme lsp-ui shackle solarized-theme ag atomic-chrome buffer-move color-theme cmake-mode dockerfile-mode git-gutter git-gutter-fringe helm-ag magit markdown-mode markdown-preview-mode multiple-cursors restclient terraform-mode undo-tree yaml-mode avy company flycheck helm-lsp helm-xref json-mode lsp-mode lsp-treemacs projectile which-key))
 '(undo-tree-auto-save-history nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(getenv "PATH")
