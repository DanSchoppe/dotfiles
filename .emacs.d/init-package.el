;; Installed packages
(setq package-enable-at-startup nil)
(setq auto-installed-packages
      '(ace-jump-mode
        ag
        autopair
        buffer-move
        color-theme
        company
        exec-path-from-shell
        flx
        flx-ido
        flycheck
        git-gutter
        hackernews
        icicles
        indent-guide
        json-mode
        magit
        multiple-cursors
        popup
        projectile
        ;; realgud
        rtags
        sql-indent
        undo-tree
        visual-regexp
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
