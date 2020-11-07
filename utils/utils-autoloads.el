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

;;;### (autoloads nil "fix-info-buffer-names" "fix-info-buffer-names.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from fix-info-buffer-names.el

(defvar fix-info-rename-buffer-mode nil "\
Non-nil if Fix-Info-Rename-Buffer mode is enabled.
See the `fix-info-rename-buffer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `fix-info-rename-buffer-mode'.")

(custom-autoload 'fix-info-rename-buffer-mode "fix-info-buffer-names" nil)

(autoload 'fix-info-rename-buffer-mode "fix-info-buffer-names" "\
Toggle Fix-Info-Rename-Buffer mode on or off.
With a prefix argument ARG, enable Fix-Info-Rename-Buffer mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil, and toggle it if ARG is
‘toggle’.

When Fix-Info-Rename-Buffer is enabled, all Info buffers' names are
automatically changed to include the current header.
See the command \\[fix-info-rename-buffer].

\(fn &optional ARG)" t nil)

(advice-add 'Info-set-mode-line :override (lambda nil (setq mode-line-buffer-identification "%1b")))

(register-definition-prefixes "fix-info-buffer-names" '("fix-info-"))

;;;***

;;;### (autoloads nil "functions" "functions.el" (0 0 0 0))
;;; Generated autoloads from functions.el

(register-definition-prefixes "functions" '("*>" "+cc-control-sequence+" "+dired-run-lisp-" "TeX-quote-region" "actual" "add-" "ag-args" "alist-process-modify-result" "async-shell-buffers" "at-" "bash-eval-region-rmd" "beg-of-" "buf-info" "c-" "char-is-capitalized-p" "cmd" "create-regex-from-wildcard" "cur" "cxx-" "cycle-" "do-" "ediff-" "end-of-" "eval-" "factorial" "for" "frontier-of-text-for-line" "function-or-symbol" "get-" "git-" "goto-file-line-at-rev-magit" "html-" "important-buffer" "is-" "iterate-from" "just-" "k-to-c" "keep-tabs-for-derived-modes" "kill-" "message-" "move-point-to-" "my-" "pp-code-subproc" "pre" "push-buffer-to-kill-ring" "python-eval-region-rmd" "quit-and-kill" "right-sexp-or-camel" "rmd-" "run-" "rx-match-no-match-sym" "save-" "skewer-eval-buffer-or-region" "space-non-newline-regexp" "squish-number-to-width" "subproc-error" "switch-" "sym-or-key" "temp-buffer-name-regexp" "thick-rx-pred" "toggle-" "trailing-whitespace-" "valid-" "view-macro-expansion" "w3m-" "was-last-output" "whitespace" "with" "wordp" "xml" "xor" "yank-push" "|->" "|>"))

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

;;;### (autoloads nil "warning-words" "warning-words.el" (0 0 0 0))
;;; Generated autoloads from warning-words.el

(autoload 'warning-highlights-mode "warning-words" "\
Highlight words of warning.

If called interactively, enable Warning-Highlights mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'find-warning-words "warning-words" "\


\(fn PFX)" t nil)

(autoload 'find-warnings-in-dir "warning-words" "\


\(fn DIR)" t nil)

(register-definition-prefixes "warning-words" '("warning-highlights-"))

;;;***

(provide 'utils-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; utils-autoloads.el ends here
