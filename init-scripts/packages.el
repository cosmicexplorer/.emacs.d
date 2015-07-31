;;; setup packages for emacs

(require 'package)

;;; add package lists
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

;;; not really sure what this bit does but nothing works without it
(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages '(
                      2048-game
                      async
                      auctex
                      auto-complete
                      better-defaults
                      cider
                      cloc
                      clojure-mode
                      coffee-mode
                      color-theme
                      company
                      csharp-mode
                      dash
                      ecb
                      ein
                      epl
                      epresent
                      espuds
                      evil
                      flycheck
                      flycheck-package
                      ggtags
                      go-mode
                      helm
                      helm-gtags
                      helm-swoop
                      js2-mode
                      less-css-mode
                      linum
                      linum-relative
                      literate-coffee-mode
                      magit
                      markdown-mode
                      minimap
                      misc-cmds
                      multiple-cursors
                      omnisharp
                      package-build
                      paredit
                      php-mode
                      pkg-info
                      queue
                      rainbow-delimiters
                      rainbow-mode
                      s
                      slime
                      smartrep
                      undo-tree
                      w3m
                      web-beautify
                      xterm-color
                      ))

;;; do the install (slow upon startup, but only for the first time)
(loop for p in my-packages
  do (unless (package-installed-p p)
      (package-install p)))

(when (get-buffer "*Compile-Log*") (delete-windows-on "*Compile-Log*"))
