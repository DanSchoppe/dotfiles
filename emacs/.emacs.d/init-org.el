(require 'org)

;; Org mode hook
(add-hook 'org-mode-hook
          (lambda ()
						(local-set-key  (kbd "C-v") 'org-cycle)))

;; Keymappings
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;; Variable customization
(setq org-log-done t)
(setq org-src-fontify-natively t)
(setq org-src-window-setup (quote current-window))
(setq org-src-ask-before-returning-to-edit-buffer nil)
(setq org-startup-indented t)
(setq org-startup-truncated nil)

;; Export customization
(setq org-export-preserve-breaks t)
(setq org-export-with-section-numbers nil)
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-tables nil)
(setq org-export-with-tags nil)
(setq org-export-with-toc 0)
