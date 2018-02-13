;;; let's get these packages loaded

;;; my own stuff matters too
(add-to-list 'load-path (concat init-home-folder-dir "utils"))
(add-to-list 'load-path (concat init-home-folder-dir "integrations"))

;;; add packages that don't exist on melpa; typically from emacswiki, but some
;;; are from more dubious sources
(add-to-list 'load-path (concat init-home-folder-dir "lisp"))

(defun org-babel-make-language-alias (&rest args))

;;; ESS is done in `./package-setup.el'

(require 'ansi-color)
(require 'browse-kill-ring)
(require 'clang-format)
(require 'comint)
(require 'ensime)
(require 'helm-config)
(require 'helm-gtags)
(require 'helm-swoop)
(require 'highlight-80+)
(require 'ido)
(require 'linum-relative)
(require 'magit)
(require 'misc-cmds)                    ; i forget what uses this lol
(require 'multiple-cursors)
(require 'pp-c-l)
(require 'qmake)
(require 'rainbow-delimiters)
(require 'rainbow-mode)
(require 'revbufs)
(require 'saveplace)
(require 'smart-compile)
(require 'smart-tab)
(require 'tramp)
(require 'undo-tree)
(require 'web-beautify)
(require 'js2-mode)
(require 'highlight-parentheses)
(require 'wgrep)
(require 'sourcemap)
(require 'eww)
(require 'git-gutter-fringe)
(require 'pdf-tools)
(require 'mmm-mode)
(require 'cuda-mode)
(require 'llvm-stuff)
(require 'llvm-mode)
(require 'scala-mode)
(require 'haskell)
(require 'rx)
(require 'widget)
(require 'epa-file)
(require 'helm-rg)

;;; my stuff
(require 'utilities)
(require 'functions)
(require 'long-lines)
(require 'unix-find)
(require 'ob-coffee)
(require 'jison-mode)
