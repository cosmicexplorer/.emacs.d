;;; let's get these packages loaded

;;; my own stuff matters too
(add-to-list 'load-path (concat init-home-folder-dir "utils"))

;;; add packages that don't exist on melpa; typically from emacswiki, but some
;;; are from more dubious sources
(add-to-list 'load-path (concat init-home-folder-dir "lisp"))

;;; get my color themes
(add-to-list 'load-path (concat init-home-folder-dir "color-themes"))

;;; ESS is done in `./package-setup.el'

(require 'tramp)
(require 'highlight-80+)
(require 'misc-cmds)                    ; i forget what uses this lol
(require 'multiple-cursors)
(require 'clang-format)
(require 'helm-config)
(require 'helm-swoop)
(require 'revbufs)
(require 'smart-compile)
(require 'undo-tree)
(require 'smart-tab)
(require 'linum-relative)
(require 'rainbow-delimiters)
(require 'ido)
(require 'saveplace)
(require 'qmake)
(require 'ansi-color)
(require 'browse-kill-ring)
(require 'comint)
(require 'color-theme)
(require 'color-theme-danny)            ; mine
(require 'web-beautify)
(require 'cloc)
(require 'pp-c-l)
(require 'erc)
(require 'erc-nicklist)                 ; this file is in /lisp/
(require 'erc-highlight-nicknames)      ; this file is in /lisp/

;;; my stuff
(require 'utilities)
(require 'functions)
(require 'long-lines)
(require 'unix-find)
