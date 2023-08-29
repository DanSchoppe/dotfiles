(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
  "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; https://gist.github.com/fredRos/0e3a845de95ec654538f
(setq markdown-command "pandoc -c file:///Users/dan/.emacs.d/github-pandoc.css --from gfm -t html5 --mathjax --highlight-style pygments --standalone --metadata pagetitle=Github")
(eval-after-load 'gfm-mode
  #'(define-key gfm-mode-map (kbd "M-n") nil))
(eval-after-load 'gfm-mode
  #'(define-key gfm-mode-map (kbd "M-p") nil))
'(define-key gfm-mode-map (kbd "M-p") nil)
'(define-key gfm-mode-map (kbd "M-n") nil)
