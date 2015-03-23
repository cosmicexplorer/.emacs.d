;;; setup done for large packages which need a kick in the pants to get going

;;; ein
(require 'ein)
(setq ein:use-auto-complete-superpack t)
(require 'smartrep)
(setq ein:use-smartrep t)

;;; slime
(setq inferior-lisp-program (executable-find "sbcl"))
(setq slime-contribs '(slime-fancy))
(load-file (expand-file-name "~/.emacs.d/quicklisp/slime-helper.el"))
