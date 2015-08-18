;;; let's get these packages loaded

;;; my own stuff matters too
(add-to-list 'load-path (concat init-home-folder-dir "utils"))
(add-to-list 'load-path (concat init-home-folder-dir "integrations"))

;;; add packages that don't exist on melpa; typically from emacswiki, but some
;;; are from more dubious sources
(add-to-list 'load-path (concat init-home-folder-dir "lisp"))

;;; ESS is done in `./package-setup.el'

(require 'ansi-color)
(require 'browse-kill-ring)
(require 'clang-format)
(require 'cloc)
(require 'color-theme)
(require 'comint)
(require 'ecb)
(require 'ensime)
(require 'erc)
(require 'erc-highlight-nicknames)      ; this file is in /lisp/
(require 'erc-nicklist)                 ; this file is in /lisp/
(require 'ggtags)
(require 'helm-config)
(require 'helm-gtags)
(require 'helm-swoop)
(require 'highlight-80+)
(require 'ido)
(require 'linum-relative)
(require 'magit)
(require 'misc-cmds)                    ; i forget what uses this lol
(require 'multiple-cursors)
(require 'omnisharp)
(require 'package)
(require 'pp-c-l)
(require 'qmake)
(require 'rainbow-delimiters)
(require 'rainbow-delimiters)
(require 'rainbow-mode)
(require 'revbufs)
(require 'saveplace)
(require 'smart-compile)
(require 'smart-tab)
(require 'tramp)
(require 'undo-tree)
(require 'web-beautify)

;;; my stuff
(require 'utilities)
(require 'functions)
(require 'long-lines)
(require 'unix-find)
(require 'ob-coffee)
