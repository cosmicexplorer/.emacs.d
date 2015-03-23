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

(defvar my-packages)
(setq my-packages '(                    ; setq over defvar for easier testing
                    2048-game           ; (can't update value with defvar)
                    auctex
		    better-defaults
                    cider
                    clojure-mode
                    coffee-mode
                    color-theme
                    company
                    dash
                    ein
                    epl
                    evil
                    espuds
                    go-mode
                    helm
                    helm-swoop
                    less-css-mode
                    linum
                    linum-relative
                    magit
                    markdown-mode
                    minimap
                    misc-cmds
                    multiple-cursors
                    noflet
                    paredit
                    pkg-info
                    php-mode
                    queue
                    rainbow-mode
                    rainbow-delimiters
                    s
                    slime
                    smartrep
                    undo-tree
                    web-beautify
                    w3m
                    ))

;;; do the install (slow upon startup, but only for the first time)
(loop for p in my-packages
  do (unless (package-installed-p p)
       (package-install p)))
