;;; let's get these packages loaded

;;; add packages that don't exist on melpa; typically from emacswiki, but some
;;; are from more dubious sources
(add-to-list 'load-path (concat init-home-folder-dir "/lisp"))

;;; get my colors themes up!
(add-to-list 'load-path (concat init-home-folder-dir "/color-themes"))

;;; ESS
(add-to-list 'load-path (concat init-home-folder-dir "/ESS/lisp"))

(require 'tramp)
(require 'highlight-80+)
(require 'misc-cmds)                    ; i forget what uses this lol
(require 'multiple-cursors)
(require 'clang-format)
(require 'helm-config)
(require 'ess-site)
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
(require 'browse-kill-ring)
(require 'color-theme)
(require 'color-theme-danny)            ; mine
(require 'web-beautify)
(require 'cloc)
