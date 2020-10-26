;;; utils-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "adapters" "adapters.el" (0 0 0 0))
;;; Generated autoloads from adapters.el

(register-definition-prefixes "adapters" '("adapters--"))

;;;***

;;;### (autoloads nil "book-txt-view" "book-txt-view.el" (0 0 0 0))
;;; Generated autoloads from book-txt-view.el

(register-definition-prefixes "book-txt-view" '("book-txt-"))

;;;***

;;;### (autoloads nil "functions" "functions.el" (0 0 0 0))
;;; Generated autoloads from functions.el

(register-definition-prefixes "functions" '("*>" "+cc-control-sequence+" "+dired-run-lisp-" "TeX-quote-region" "actual-setup-submodules" "add-" "ag-args" "alist-process-modify-result" "async-shell-buffers" "at-" "bash-eval-region-rmd" "beg-of-" "buf-info" "c-" "char-is-capitalized-p" "cmd" "create-regex-from-wildcard" "cur" "cxx-" "cycle-" "do-" "ediff-" "end-of-" "eval-" "factorial" "for" "frontier-of-text-for-line" "function-or-symbol" "get-" "git-" "goto-file-line-at-rev-magit" "html-" "important-buffer" "is-" "iterate-from" "just-" "k-to-c" "keep-tabs-for-derived-modes" "kill-" "message-" "move-point-to-" "my-" "pp-code-subproc" "pre" "push-buffer-to-kill-ring" "python-eval-region-rmd" "quit-and-kill" "right-sexp-or-camel" "rmd-" "run-" "rx-match-no-match-sym" "save-" "skewer-eval-buffer-or-region" "space-non-newline-regexp" "squish-number-to-width" "subproc-error" "switch-" "sym-or-key" "temp-buffer-name-regexp" "thick-rx-pred" "toggle-" "trailing-whitespace-" "valid-" "view-macro-expansion" "w3m-" "was-last-output" "whitespace" "with" "wordp" "xml" "xor" "yank-push" "|->" "|>"))

;;;***

;;;### (autoloads nil "long-lines" "long-lines.el" (0 0 0 0))
;;; Generated autoloads from long-lines.el

(register-definition-prefixes "long-lines" '("long-line"))

;;;***

;;;### (autoloads nil "unix-find" "unix-find.el" (0 0 0 0))
;;; Generated autoloads from unix-find.el

(autoload 'unix-find "unix-find" "\
Recognizes :[i]name, :[i]wholename, :[i]regex, :[i]wholeregex, :not,
:maxdepth, :mindepth, :type, :perm, :binary (which uses `file-binary-p'), and
:size. Doesn't care about the positioning of :maxdepth and :mindepth. :type
recognizes 'd', 'f', 'p', 'l', and 's', and :size only accepts a number of
bytes, as well as a > or < sign in front. Performs breadth-first
search. Probably pretty slow.

\(fn DIR &rest ARGS)" nil nil)

(autoload 'unix-find-mode "unix-find" "\
Compilation derived mode to display results of `find'

\(fn)" nil nil)

(autoload 'u-find "unix-find" "\
Parses and converts arguments with hyphen syntax (-name, -regex, etc) to
atoms as a sexp for input to `unix-find' (:name, :regex, etc). Displays default
prompt according to `unix-find-begin-prompt'.

\(fn &optional PREFIX-ARG)" t nil)

(register-definition-prefixes "unix-find" '("cleanup-find-buffers" "files-except-tree" "unix-find-"))

;;;***

;;;### (autoloads nil "utilities" "utilities.el" (0 0 0 0))
;;; Generated autoloads from utilities.el

(register-definition-prefixes "utilities" '("apply-log" "buffer-binary-p" "cdr" "check-type" "concat-n" "do-for-line" "file-binary-p" "line-" "make-lambda-maybe-not" "nconcat" "nthcdraf" "or-fun" "other-window-prefix-wrapper" "prependn" "remove-from-plist" "str2" "trim-whitespace"))

;;;***

(provide 'utils-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; utils-autoloads.el ends here
