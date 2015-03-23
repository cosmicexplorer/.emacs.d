;;; let's get these packages loaded

;;; add packages that don't exist on melpa; typically from emacswiki, but some
;;; are from more dubious sources
(add-to-list 'load-path (concat init-home-folder-dir "lisp"))

(require 'tramp)
(require 'highlight-80+)
(require 'misc-cmds)                    ; i forget what uses this lol
