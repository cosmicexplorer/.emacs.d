;;; -*- lexical-binding: t -*-


;;;;; Bring every package we define ourselves or download from MELPA into scope.
;;;;; TODO: is `require' less performant than relying on autoloads? Could we cut down on startup
;;;;;       time by using a lazier approach? Or is this the right way to lazy load?


;;;;; Make our own packages visible!

;;; NB: my own stuff matters too!!! <33333333 ^_^_^_^_^!

;;; "utils": reusable functions and macros go here.
(add-to-list 'load-path (home-dir-path "utils"))
;;; "integrations": a breeding ground for new MELPA packages!
(add-to-list 'load-path (home-dir-path "integrations"))
;;; "lisp": packages that don't exist on MELPA; typically from emacswiki, but some
;;; are from more dubious sources (an old professor??).
(add-to-list 'load-path (home-dir-path "lisp"))

;;; FIXME: ...what does this fix?
(defun org-babel-make-language-alias (&rest args))

;;; ESS is done in `./package-setup.el'

(require 'ess)
(require 'ess-help)
(require 'ansi-color)
(require 'clang-format)
(require 'comint)
(require 'helm)
(require 'helm-mode)
;; This package attempts to use `minibuffer-local-must-match-filename-map',
;; which appears to have been recently removed.
(require 'helm-gtags)
(require 'helm-swoop)
(require 'highlight-80+)
(require 'ido)
(require 'linum-relative)
(require 'magit)
(require 'magit-popup)
(require 'misc-cmds)                    ; i forget what uses this lol
(require 'multiple-cursors)
(require 'rainbow-delimiters)
(require 'rainbow-mode)
(require 'revbufs)
(require 'saveplace)
(require 'smart-compile)
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
(require 'scala-mode)
(require 'haskell)
(require 'rx)
(require 'widget)
(require 'epa-file)
(require 'pcase)
(require 'pabbrev)
(require 'helpful)
(require 'minibuffer-line)
(require 'company)
(require 'rmail)
(require 'server)

;;; my stuff
(require 'utilities)
(require 'functions)
(require 'long-lines)
(require 'unix-find)
(require 'ob-coffee)
(require 'jison-mode)
(require 'mutable-state-init)
