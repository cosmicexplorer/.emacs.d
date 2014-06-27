;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "dired-xattr/dired-xattr" "dired-xattr/dired-xattr.el"
;;;;;;  (21385 12361 82102 0))
;;; Generated autoloads from dired-xattr/dired-xattr.el

(autoload 'dired-xattr-add-overlay "dired-xattr/dired-xattr" "\
Colorize the perm column from current `dired' buffer according
MacOSX Finder label colors.

Does nothing if `dired-default-directory' is not a local
directory of if buffer has more that
`dired-xattr-max-buffer-lines' lines.

\(fn &optional FORCE)" t nil)

;;;***

;;;### (autoloads nil "go-mode/go-mode" "go-mode/go-mode.el" (21385
;;;;;;  12359 948913 0))
;;; Generated autoloads from go-mode/go-mode.el

(autoload 'go-mode "go-mode/go-mode" "\
Major mode for editing Go source text.

This mode provides (not just) basic editing capabilities for
working with Go code. It offers almost complete syntax
highlighting, indentation that is almost identical to gofmt and
proper parsing of the buffer content to allow features such as
navigation by function, manipulation of comments or detection of
strings.

In addition to these core features, it offers various features to
help with writing Go code. You can directly run buffer content
through gofmt, read godoc documentation from within Emacs, modify
and clean up the list of package imports or interact with the
Playground (uploading and downloading pastes).

The following extra functions are defined:

- `gofmt'
- `godoc'
- `go-import-add'
- `go-remove-unused-imports'
- `go-goto-imports'
- `go-play-buffer' and `go-play-region'
- `go-download-play'
- `godef-describe' and `godef-jump'
- `go-coverage'

If you want to automatically run `gofmt' before saving a file,
add the following hook to your emacs configuration:

\(add-hook 'before-save-hook #'gofmt-before-save)

If you want to use `godef-jump' instead of etags (or similar),
consider binding godef-jump to `M-.', which is the default key
for `find-tag':

\(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd \"M-.\") #'godef-jump)))

Please note that godef is an external dependency. You can install
it with

go get code.google.com/p/rog-go/exp/cmd/godef


If you're looking for even more integration with Go, namely
on-the-fly syntax checking, auto-completion and snippets, it is
recommended that you look at goflymake
\(https://github.com/dougm/goflymake), gocode
\(https://github.com/nsf/gocode), go-eldoc
\(github.com/syohex/emacs-go-eldoc) and yasnippet-go
\(https://github.com/dominikh/yasnippet-go)

\(fn)" t nil)

(add-to-list 'auto-mode-alist (cons "\\.go\\'" 'go-mode))

(autoload 'gofmt-before-save "go-mode/go-mode" "\
Add this to .emacs to run gofmt on the current buffer when saving:
 (add-hook 'before-save-hook 'gofmt-before-save).

Note that this will cause go-mode to get loaded the first time
you save any file, kind of defeating the point of autoloading.

\(fn)" t nil)

(autoload 'godoc "go-mode/go-mode" "\
Show go documentation for a query, much like M-x man.

\(fn QUERY)" t nil)

(autoload 'go-download-play "go-mode/go-mode" "\
Downloads a paste from the playground and inserts it in a Go
buffer. Tries to look for a URL at point.

\(fn URL)" t nil)

;;;***

;;;### (autoloads nil "highlight-parentheses/highlight-parentheses"
;;;;;;  "highlight-parentheses/highlight-parentheses.el" (21421 266
;;;;;;  731863 887000))
;;; Generated autoloads from highlight-parentheses/highlight-parentheses.el

(autoload 'highlight-parentheses-mode "~/.emacs.d/el-get/highlight-parentheses/highlight-parentheses" "\
Minor mode to highlight the surrounding parentheses.

\(fn &optional ARG)" t nil)

(defvar global-highlight-parentheses-mode nil "\
Non-nil if Global-Highlight-Parentheses mode is enabled.
See the command `global-highlight-parentheses-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-parentheses-mode'.")

(custom-autoload 'global-highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" nil)

(autoload 'global-highlight-parentheses-mode "highlight-parentheses/highlight-parentheses" "\
Toggle Highlight-Parentheses mode in all buffers.
With prefix ARG, enable Global-Highlight-Parentheses mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-Parentheses mode is enabled in all buffers where
`(lambda nil (highlight-parentheses-mode 1))' would do it.
See `highlight-parentheses-mode' for more information on Highlight-Parentheses mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil "php-mode/php-mode" "php-mode/php-mode.el"
;;;;;;  (21385 12360 333586 0))
;;; Generated autoloads from php-mode/php-mode.el

(let ((loads (get 'php 'custom-loads))) (if (member '"php-mode/php-mode" loads) nil (put 'php 'custom-loads (cons '"php-mode/php-mode" loads))))

(defvar php-extra-constants 'nil "\
A list of additional strings to treat as PHP constants.")

(custom-autoload 'php-extra-constants "php-mode/php-mode" t)

(add-to-list 'interpreter-mode-alist (cons "php" 'php-mode))

(autoload 'php-mode "php-mode/php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(dolist (pattern '("\\.php[s345t]?\\'" "\\.phtml\\'" "Amkfile" "\\.amk$")) (add-to-list 'auto-mode-alist `(,pattern . php-mode) t))

;;;***

;;;### (autoloads nil "yasnippet/yasnippet" "yasnippet/yasnippet.el"
;;;;;;  (21385 12363 140094 0))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas-minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, `yas-expand', normally bound to
the TAB key, expands snippets of code depending on the major
mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the command `yas-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet/yasnippet" nil)

(autoload 'yas-global-mode "yasnippet/yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
