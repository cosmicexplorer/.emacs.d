;;; -*- lexical-binding: t -*-

;;; configuration for various language modes

;;; indentation silliness
(setq-default indent-tabs-mode nil)     ;; use spaces not tabs
(setq tab-width 2)                      ; 4-spacers get at me
(add-hook 'prog-mode-hook (lambda () (setq fill-column 80)))
(add-hook 'text-mode-hook (lambda () (setq fill-column 80)))

;;; commenting
(defun make-comments-like-c ()
  (setq comment-start "/*" comment-end "*/" comment-padding " "))
;; i hate the young whippersnappers and their strange and weird ways
(add-hook 'c++-mode-hook #'make-comments-like-c)
(add-hook 'c-mode-hook #'make-comments-like-c)
(add-hook 'fundamental-mode-hook (lambda ()
                                   (setq comment-start "-" comment-end "")))
;;; format comments like a normal person
(add-hook 'r-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'ess-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; " comment-end "")))
(add-hook 'cmake-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'asm-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'js2-mode-hook (lambda () (setq comment-start "// " comment-end "")))
(add-hook 'css-mode-hook
          (lambda () (setq comment-start "// " comment-end "")))

;;; assembly before S
(push '("\\.[sS]\\'" . asm-mode) auto-mode-alist)

;;; more ess-mode nonsense
(add-hook 'ess-mode-hook #'warning-highlights-mode)
(add-hook 'ess-mode-hook #'auto-fill-mode)
(add-hook 'ess-mode-hook #'highlight-80+-mode)

(defun my-inf-ess-end-send-input ()
  (interactive)
  (goto-char (point-max))
  (funcall-interactively #'inferior-ess-send-input))

(let* ((ess-s-l-loc (locate-library "ess-s-l"))
       (ess-s-l-not-elc
        (replace-regexp-in-string "\\.elc\\'" ".el" ess-s-l-loc)))
  (when (file-exists-p ess-s-l-not-elc)
    (byte-compile-file ess-s-l-not-elc t)))

(defvar my-ess-eval-history nil)
(defvar my-ess-switch-to-process nil)

(defun set-ess-switch-process ()
  (interactive)
  (setq my-ess-switch-to-process t))

(defconst my-ess-minibuf-keymap
  (let ((map (make-sparse-keymap)))
    (easy-mmode-set-keymap-parents map (list minibuffer-local-map ess-mode-map))
    (define-key map (kbd "C-RET") #'set-ess-switch-process)))

;; (let ((ess-proc (ess-command--normalise-proc proc no-prompt-check)))
;;   (insert
;;    (with-temp-buffer
;;      (setq-local ess-local-process-name (process-name ess-proc))
;;      (ess-string-command "2 + 4\n" (current-buffer) .2) (buffer-string))))

(defun my-ess-eval-this (inp)
  (interactive
   (progn
     (setq my-ess-switch-to-process nil)
     (list
      (read-from-minibuffer
       "ESS expression: " nil my-ess-minibuf-keymap nil 'my-ess-eval-history))))
  (if (not current-prefix-arg)
      ;; just put it in process buffer
      (ess-execute inp 'buffer)
    (with-temp-buffer
      (setq prefix-arg nil)
      (funcall-interactively #'ess-execute inp nil )))

  (when my-ess-switch-to-process (ess-switch-to-end-of-ESS))
  (when current-prefix-arg ))

;;; knitr support
(require 'poly-R)
(require 'poly-markdown)

(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-markdown+r-mode))

(defconst rmd-export-cmd-fmt-str
  "Rscript - && pandoc \"%s\" -o \"%s\"")
(defconst rmd-knitr-fmt-str
  "require(knitr); require(markdown); knit(\"%s\",\"%s\")")

(defun rmd-export-sentinel (proc _)
  (message "%s" (with-current-buffer (process-buffer proc) (buffer-string))))

(defun rmd-export-pdf (infile)
  (interactive (list (buffer-file-name)))
  (let* ((interfile (replace-regexp-in-string "\\.[rR]md\\'" ".md" infile))
         (outfile (replace-regexp-in-string "\\.[rR]md\\'" ".pdf" infile))
         (proc
          (make-process
           :command (list shell-file-name shell-command-switch
                          (format rmd-export-cmd-fmt-str interfile outfile))
           :name "*rmd-export*"
           :buffer (generate-new-buffer "rmd-export")
           :connection-type 'pipe
           :sentinel #'rmd-export-sentinel)))
    (process-send-string proc (format rmd-knitr-fmt-str infile interfile))
    (process-send-eof proc)))

;;; highlight cursor and auto-fill when over 80 chars in certain modes
(defvar no-auto-fill-modes
  '(litcoffee-mode tex-mode markdown-mode elisp-byte-code-mode))
(defun selective-turn-on-auto-fill ()
  (unless (member major-mode no-auto-fill-modes)
    (highlight-80+-mode)
    (auto-fill-mode)))
(add-hook 'prog-mode-hook #'selective-turn-on-auto-fill)
(defvar coffee-string-interpolation-regexp "#{[^}]*}")

;;; perl
(require 'cperl-mode)
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 2)
(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))
(add-hook
 'cperl-mode-hook
 (lambda ()
   (turn-on-eldoc-mode)
   (set (make-local-variable 'eldoc-documentation-function)
        'my-cperl-eldoc-documentation-function)))

;;; yes, this perl re.pl business is a massive hack. i claim none of it would be
;;; required if re.pl wasn't also a massive hack (who doesn't put newlines after
;;; their own repl's output????)
(defun perl-repl-get-real-str ()
  (buffer-substring-no-properties
   comint-last-output-start
   (process-mark (get-buffer-process (current-buffer)))))

;;; fixes 'print "hey"'
(defun perl-repl-fix-both-early-and-late-insertions (str)
  (goto-char (point-max))
  ;; the noerror argument shouldn't matter, but provided anyway
  (let ((real-str (perl-repl-get-real-str))
        (inhibit-read-only t))
    (when (string-match-p
           (concat (regexp-quote perl-repl-prompt) ".+\\'") real-str)
      (backward-char (length real-str))
      (re-search-forward (regexp-quote perl-repl-prompt))
      (delete-region (- (point) (length perl-repl-prompt)) (point))
      (newline)
      (goto-char (1- (point-max)))
      (let ((prev-final-char (char-after)))
        (insert perl-repl-prompt)
        (delete-char 1)
        (backward-char (length perl-repl-prompt))
        (insert prev-final-char)
        (newline))))
  (goto-char (point-max)))

;;; fixes 'foreach ((1, 2, 3)) { print "$_ hey " }'
(defun perl-repl-fix-late-insertions (str)
  (ignore-errors
    (goto-char (point-max))
    (let ((real-str (perl-repl-get-real-str)))
      (when (and (or (string-match-p (concat "\\`"
                                             (regexp-quote perl-repl-prompt))
                                     real-str)
                     (not (string-match-p (regexp-quote perl-repl-prompt)
                                          real-str)))
                 (not (string-match-p "Welcome to the perl re\\.pl!" real-str)))
        (let ((inhibit-read-only t))
          (backward-char (length real-str))
          (when (< (+ (point) (length perl-repl-prompt)) (point-max))
            (insert (buffer-substring-no-properties
                     (+ (point) (length perl-repl-prompt))
                     (point-max))))
          (forward-char (length perl-repl-prompt))
          (delete-region (point) (point-max))
          (goto-char (point-max)))))))

;;; fixes '2 + 2'
(defun perl-repl-fix-early-insertions (str)
  (goto-char (point-max))
  (let ((real-str (perl-repl-get-real-str)))
    (backward-char (length perl-repl-prompt))
    (when (and (string-equal
                (buffer-substring-no-properties (point) (point-max))
                perl-repl-prompt)
               (not (char-equal (char-before) (str2char "\n"))))
      (newline))
    (goto-char (point-max))))

(defun perl-repl-trim-leading-whitespace (str)
  (let ((real-str (perl-repl-get-real-str)))
    (when (string-match-p (regexp-quote perl-repl-prompt) real-str)
      (goto-char (- (point) (length real-str)))
      (while (string-match-p "[[:space:]\r\n]" (char-to-string (char-after)))
        (delete-char 1))
      (goto-char (point-max)))))

(defvar perl-repl-prompt "$ ")

(defvar perl-repl-prompt-regexp (concat "^" (regexp-quote perl-repl-prompt)))

(setq perl-repl-output-filter-functions
      '(perl-repl-fix-both-early-and-late-insertions
        perl-repl-fix-late-insertions
        perl-repl-fix-early-insertions
        perl-repl-trim-leading-whitespace
        ansi-color-process-output
        comint-postoutput-scroll-to-bottom
        comint-watch-for-password-prompt))

(define-derived-mode perl-repl-mode comint-mode "re.pl"
  "\\<cperl-mode-map>"
  :syntax-table cperl-mode-syntax-table
  (save-excursion
    (hi-lock-mode 1)
    (hi-lock-set-pattern "\\`Welcome to the perl re.pl!"
                         'font-lock-variable-name-face))
  (set (make-local-variable 'font-lock-defaults)
       '(cperl-font-lock-keywords t))
  (setf comint-prompt-read-only t)
  (setf comint-use-prompt-regexp t)
  (setf comint-prompt-regexp perl-repl-prompt-regexp)
  (setf comint-output-filter-functions perl-repl-output-filter-functions)
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) perl-repl-prompt-regexp))

(defun re.pl ()
  (interactive)
  (if (executable-find "re.pl")
      (progn
        (pop-to-buffer
         (make-comint-in-buffer
          "re.pl" nil (convert-standard-filename
                       (concat init-home-folder-dir
                               "init-scripts/perl-repl-helper.sh"))))
        (perl-repl-mode))
    (message "Sorry, re.pl must be installed for this command to work.")))

;;; c/c++/java
(setq-default c-basic-offset 2) ;; cc-mode uses this instead of tab-width
;;; stop auto-inserting newlines after semicolons i don't like that
(setq c-hanging-semi&comma-criteria nil)
(setq c-default-style nil)
;;; doesn't work, hence camel-case-{left,right}-word in functions.el
(subword-mode)
(setq c-electric-flag nil)
(add-hook
 'c-initialization-hook
 '(lambda ()
    (add-keybinding-to-mode-maps
     "RET" 'newline-and-indent-fix-cc-mode
     c-mode-map
     c++-mode-map
     java-mode-map)))
(defconst c-namespace-style
  '("cc-mode"
    (c-offsets-alist . ((innamespace . [0])))))
(c-add-style "better-c++-mode" c-namespace-style)

(make-variable-buffer-local 'comment-region-function)
(make-variable-buffer-local 'comment-insert-comment-function)
(defun add-star-comment-region ()
  (setq comment-region-function #'comment-region-default)
  (setq comment-insert-comment-function #'c-comment-end-of-line))
(add-hook 'c-mode-hook #'add-star-comment-region)
(add-hook 'c++-mode-hook #'add-star-comment-region)
;;; don't indent namespaces wtf
(c-set-offset 'innamespace 0)
;;; add modes for random c++ filetypes
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.txx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))

;;; lol c#
(defcustom csharp-indent-level 4
  "Level of indentation used for C# buffers. NOT part of csharp-mode!"
  :type 'integer
  :group 'csharp)
(defcustom csharp-indent-namespaces t
  "Whether to indent namespaces in C# buffers. NOT part of csharp-mode!"
  :type 'boolean
  :group 'csharp)
(defconst csharp-cc-style
  `("cc-mode"
    (c-offsets-alist . ((innamespace
                         ,(make-vector
                           1 (if csharp-indent-namespaces
                                 csharp-indent-level 0)))))
    (c-basic-offset . ,csharp-indent-level)))

(defun csharp-hack-newline ()
  (interactive)
  (let ((annoying-identifiers
         (delete-dups
          (append '(")") (c-lang-const c-type-list-kwds csharp)
                  (c-lang-const c-block-stmt-1-kwds csharp)
                  (c-lang-const c-block-stmt-2-kwds csharp)
                  '("else")))))
    (cond
     ((and (char-before) (char-equal (char-before) (str2char "."))
           (not (in-comment-p)))
      (backward-char)
      (newline-and-indent)
      (forward-char))
     ((string-match-p
       (concat (regexp-opt annoying-identifiers) "[[:space:]]*$")
       (buffer-substring-no-properties
        (line-beginning-position) (point)))
      (let ((pt (point))
            (next-pt nil))
        (insert ";")
        (newline-and-indent)
        (setq next-pt (point))
        (goto-char pt)
        (delete-char 1)
        (goto-char (1- next-pt))))
     ((and (string-match-p
            "([[:space:]]*$" (buffer-substring-no-properties
                     (line-beginning-position) (point)))
           (char-equal (char-after) (str2char ")")))
      (newline) (indent-for-tab-command))
     (t (newline-and-indent)))))

(defun csharp-hack-parenthesis ()
  (interactive)
  (when (and
           (not (whitespacep (char-before)))
           (string-match-p
            (concat (regexp-opt
                     (c-lang-const c-block-stmt-2-kwds csharp) 'words)
                    "[[:space:]]*$")
            (buffer-substring-no-properties
             (line-beginning-position) (point))))
    (insert " "))
  (insert "()")
  (backward-char))


(make-submodule
 "OmniSharpServer"
 (if (eq system-type 'windows-nt) "msbuild.exe" "xbuild")
 nil nil)

(eval-after-load 'csharp-mode
  '(progn
     (c-add-style "csharp-mode-style" csharp-cc-style)
     (add-hook 'csharp-mode-hook (lambda () (c-set-style "csharp-mode-style")))
     (font-lock-add-keywords 'csharp-mode '(("else" . font-lock-keyword-face)))
     (when use-omnisharp
       (eval-after-load 'omnisharp
         '(progn
            (setq omnisharp-server-executable-path
                  (concat init-home-folder-dir
                          "OmniSharpServer/OmniSharp/bin/Debug/OmniSharp.exe"))
            (when (file-exists-p omnisharp-server-executable-path)
              (if (not (executable-find "curl"))
                  (send-message-to-scratch
                   "Omnisharp built, but curl not found! fix it.")
                (eval-after-load 'company
                  '(add-to-list 'company-backends 'company-omnisharp))
                (add-hook 'csharp-mode-hook 'omnisharp-mode))))))))

(defun start-omnisharp-server-for-directory (dir)
  (interactive "Mdirectory to start omnisharp for: ")
  (save-buffer)
  (set-process-filter
   (start-process (concat "omnisharp-server@" (file-truename dir))
                  (concat "*OmniSharp-server@" (file-truename dir) "*")
                  omnisharp-server-executable-path dir)
   (lambda (proc ev)
     (when (string-match-p "Solution has finished loading" ev)
       (message "%s %s %s" "Omnisharp solution for"
                (buffer-name (process-buffer proc)) "has finished loading."))
     (with-current-buffer (process-buffer proc)
       (goto-char (point-max))
       (insert ev)))))

;;; shell
(defun setup-sh-indentation ()
  (warning-highlights-mode-activate)
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))
(add-hook 'sh-mode-hook #'setup-sh-indentation)
(defun zsh-mode ()
  (interactive)
  (set (make-local-variable 'sh-shell) 'zsh)
  (sh-mode))
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . zsh-mode))
(add-to-list 'auto-mode-alist '("\\`\\.zshrc\\'" . zsh-mode))
(add-hook 'shell-mode-hook #'turn-off-auto-fill)

;;; lisp and related
(defmacro add-fun-to-hooks (fun &rest hooks)
  `(progn ,@(mapcar (lambda (hook) `(add-hook (quote ,hook) ,fun)) hooks)))
(put 'add-fun-to-hooks 'lisp-indent-function 1)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of
Lisp code." t)
;;; running these add-hooks all at once in a mapcan leads to slime keybindings
;;; in modes where they shouldn't be, and I don't know why. it may have
;;; something to do with dynamic scope; anyway, this macro seems to work
(add-fun-to-hooks #'enable-paredit-mode
  emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook ielm-mode-hook
  lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook racket-mode-hook
  racket-repl-mode-hook clojure-mode-hook elisp-byte-code-mode-hook)

;;; start scratch buffer in paredit mode
(with-current-buffer (get-buffer "*scratch*")
  (enable-paredit-mode)
  (eldoc-mode))

(setq inferior-lisp-program "sbcl")

 (eval-after-load "slime"
  '(define-key slime-autodoc-mode-map (kbd "SPC")
     (lambda (arg) (interactive "p")
       (if (use-region-p)
           (progn
             (delete-region (region-beginning) (region-end))
             (insert " "))
         (slime-autodoc-space arg)))))

;;; python
;;; use python-mode for scons files
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))
(add-hook 'python-mode-hook #'eldoc-mode)
(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.aurora\\'" . python-mode))

;;; js/css/html
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))

(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

;;; config files
(add-to-list 'auto-mode-alist '("\\.jsbeautifyrc\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.clang-format\\'" . conf-mode))

(defun html-eldoc-function ()
  (let ((context (car (last (save-excursion (sgml-get-context))))))
    (when context
      (let ((ctx-ref (cdr (assoc (aref context 4) html-tag-help))))
        (when ctx-ref
          (format
           "%s: %s"
           (propertize
            (aref context 4)
            'face
            (or (cdr (assoc (aref context 4) html-tag-face-alist))
                'font-lock-function-name-face))
           ctx-ref))))))
(add-hook
 'html-mode-hook
 (lambda ()
   (set (make-local-variable 'eldoc-documentation-function)
        #'html-eldoc-function)
   (eldoc-mode)))

;;; eldoc everywhere!
(global-eldoc-mode)

;;; syntax highlighting
(global-font-lock-mode 1)               ; turn on syntax highlighting
(setq font-lock-maximum-decoration t)   ; turn it ALL the way on

;;; add code folding with hs-minor-mode
(add-hook 'prog-mode-hook (lambda () (ignore-errors (hs-minor-mode))))

;;; pretty sure this has to be done with a hook but i forget why
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;;; slime
(load-my-init-script "slime-setup")

;;; clojure
(load-my-init-script "cider-setup")

(defcustom no-gfm nil "Turn off gfm mode."
  :type 'boolean
  :safe t)

;;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(unless (or (executable-find "grip-no-header")
            (file-exists-p "/usr/local/bin/grip-no-header"))
  (user-error
   "%s %s" "grip-no-header cannot be found! install from"
   "https://github.com/cosmicexplorer/grip"))

(defun setup-markdown-mode ()
  (if (and (not no-gfm)
           (not (derived-mode-p 'gfm-mode))
           (zerop (call-process
                   "git" nil nil nil "rev-parse" "--git-dir")))
      (gfm-mode)
    (let ((cmd (format
                "pandoc -f markdown%s -t html -"
                (if markdown-enable-math "+tex_math_dollars" ""))))
      (setq-local markdown-command cmd))))

(defun set-gfm-markdown-command ()
  (set (make-local-variable 'markdown-command)
       "pandoc -f markdown_github -t html -"))

(add-hook 'gfm-mode-hook #'set-gfm-markdown-command)

(defun set-markdown-local-vars-hook ()
  (add-hook 'hack-local-variables-hook #'setup-markdown-mode t t))

(add-hook 'markdown-mode-hook #'set-markdown-local-vars-hook)

;;; coffeescript
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map (kbd "M-;")
       'coffeescript-comment-do-what-i-really-mean)
     (add-hook 'coffee-mode-hook #'rainbow-mode)))
(add-hook 'coffee-mode-hook (lambda () (setq coffee-tab-width 2)))
(define-derived-mode cjsx-mode coffee-mode "CJSX"
  "Major mode for editing CJSX."
  (coffee-mode)
  (set (make-local-variable 'coffee-command) "cjsx")
  (setq mode-name "CJSX")
  (setq major-mode 'cjsx-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . cjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-hook 'litcoffee-mode-hook
          (lambda ()
            (set (make-local-variable 'coffee-args-compile)
                 (cons "-l" coffee-args-compile))))

;;; latex
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
(defconst latex-widen-cash-regexp "[^[:space:]]")
(defconst latex-cash-default-delim "$")
(defun latex-double-cash (pfx)
  (interactive "P")
  (let ((ins-str (if pfx (read-string "delimiter to insert: ")
                  latex-cash-default-delim)))
    (re-search-backward latex-widen-cash-regexp)
    (forward-char 1)
    (insert ins-str)
    (let ((right (save-excursion
                   (re-search-forward latex-widen-cash-regexp)
                   (forward-char -1)
                   (point))))
      (delete-region (point) right))
    (insert ins-str)
    (forward-char -1)))

;;; haskell
(require 'haskell-mode)

(add-hook 'haskell-mode-hook #'intero-mode)

(defconst hlint-output-start-regexp
  "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\): \\([^:]+\\): \\(.+\\)\n")
(defconst hlint-trim-ws-regexp
  "\\`[[:space:]\n]+\\|[[:space:]\n]\\'")
(defconst hlint-typo-regexp
  (format "\\`%s%s\\'"
          "Found:[[:space:]\n]+\\(\\(?:.\\|[[:space:]]\\)+\\)\n"
          "Why not:[[:space:]\n]+\\(\\(?:.\\|[[:space:]]\\)+\\)"))
(defconst hlint-error-type-alist
  '(("Suggestion" . info)
    ("Warning" . warning)
    ("Error" . error)))

;;; makes hlint able to correct with intero!
(defun hlint-checker-fun (output _ buffer)
  (with-temp-buffer
    (insert output)
    (goto-char (point-min))
    (cl-remove-if
     #'null
     (cl-loop
      while (re-search-forward hlint-output-start-regexp nil t)
      collect (let ((file (match-string 1))
                    (line (string-to-number (match-string 2)))
                    (col (string-to-number (match-string 3)))
                    (type (match-string 4))
                    (name (match-string 5))
                    (pt (point)))
                (re-search-forward "\n\n")
                (let ((txt (replace-regexp-in-string
                            hlint-trim-ws-regexp ""
                            (buffer-substring-no-properties pt (1- (point)))))
                      (level (or (cdr (assoc type hlint-error-type-alist))
                                 'info)))
                  (when (string-match hlint-typo-regexp txt)
                    (let ((found (match-string 1 txt))
                          (rep (match-string 2 txt)))
                      (with-current-buffer buffer
                        (add-to-list
                         'intero-suggestions
                         (list :type 'fix-typo
                               :typo found
                               :replacement rep
                               :column col
                               :line line) t))))
                  (flycheck-error-new-at
                   line col level (format "%s\n%s" name txt)
                   :checker 'haskell-hlint-better
                   :id type
                   :filename (buffer-file-name buffer)
                   :buffer buffer)))))))

(flycheck-define-command-checker 'haskell-hlint-better
  "A Haskell style checker using hlint.
See URL `https://github.com/ndmitchell/hlint'."
  :command '("hlint"
             (option-list "-X" flycheck-hlint-language-extensions concat)
             (option-list "-i=" flycheck-hlint-ignore-rules concat)
             (option-list "-h" flycheck-hlint-hint-packages concat)
             (config-file "-h" flycheck-hlintrc)
             (eval flycheck-hlint-args)
             source-inplace)
  :error-parser #'hlint-checker-fun
  :modes '(haskell-mode literate-haskell-mode))

(add-to-list 'flycheck-checkers 'haskell-hlint-better)

(eval-after-load 'intero
  '(flycheck-add-next-checker 'intero 'haskell-hlint-better t))

(defvar-local stack-project-root nil)

(defun stack-set-project-root ()
  (let* ((result (shell-command-to-string
                  "stack path --project-root --stack-root --silent"))
         (stack-root (and (string-match "^stack-root: \\(.+\\)$" result)
                          (match-string 1 result)))
         (project-root (and (string-match "^project-root: \\(.+\\)$" result)
                            (match-string 1 result)))
         (root (if (string-prefix-p stack-root project-root)
                   (expand-file-name ".")
                 project-root)))
    (setq stack-project-root root)))

(add-hook 'haskell-mode-hook #'stack-set-project-root)

(defadvice intero-parse-errors-warnings-splices
    (around set-errors-for-hlint activate)
  (let* ((buf (ad-get-arg 1))
         (fname
          (with-current-buffer buf
            (file-relative-name (buffer-file-name) stack-project-root)))
         (tmp-file (intero-temp-file-name buf)))
    ad-do-it
    (cl-loop with ret = ad-return-value
             with file-literal = (regexp-quote tmp-file)
             for err in ret
             for err-msg = (flycheck-error-message err)
             for fixed-msg = (replace-regexp-in-string
                              file-literal fname err-msg nil t)
             do (setf (flycheck-error-message err) fixed-msg))))

(defun say-yes (&rest args) t)

(defcustom haskell-mode-generate-tags-p nil
  "Whether `haskell-mode-generate-tags' does anything."
  :type 'boolean
  :safe #'say-yes)

(defadvice haskell-mode-generate-tags (around do-not-generate-tags activate)
  (when haskell-mode-generate-tags-p ad-do-it))

(defun turn-on-toggle-button (but)
  (unless (widget-get but :value)
    (widget-toggle-action but)))

(defconst activate-buttons-alist
  '((toggle . turn-on-toggle-button)))

(defun press-all-buttons-and-exit ()
  (interactive)
  (widget-map-buttons
   (lambda (but _)
     (when-let ((action (cdr (assoc (widget-type but) activate-buttons-alist))))
       (funcall action but)
       nil)))
  (exit-recursive-edit))

(defadvice intero-multiswitch (around press-q-to-quit activate)
  (let ((widget-keymap
         (copy-keymap widget-keymap)))
    (define-key widget-keymap (kbd "q") #'abort-recursive-edit)
    (define-key widget-keymap (kbd "<C-return>") #'press-all-buttons-and-exit)
    ad-do-it))

;;; scala
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(defadvice sbt:find-root (around spoof-root activate)
  (let ((res ad-do-it))
    (unless res
      (setq ad-return-value
            (read-file-name "sbt project root: " nil "./" nil "./")))))
;;; TODO: also something so ensime runs "sbt gen-ensime" for you
;;; ensime sets company-idle-delay to 0 cause it's trash
(defun reset-company-idle-delay ()
  (setq company-idle-delay (default-value 'company-idle-delay)))
(add-hook 'scala-mode-hook #'reset-company-idle-delay t)
(setq auto-mode-alist
      (cl-remove-if (lambda (el) (eq (cdr el) 'scala-mode)) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))

(defvar-local prev-indent-pt nil)
(defadvice scala-indent:indent-line (after add-space activate)
  (when (looking-back "//" (line-beginning-position))
    (insert " ")
    (setq prev-indent-pt (point))))
(defadvice comment-dwim (after maintain-space activate)
  (when prev-indent-pt
    (goto-char prev-indent-pt)
    (setq prev-indent-pt nil)))

;;; lua stuff
(add-to-list 'auto-mode-alist '("\\.nse$" . lua-mode))

;;; ruby stuff
(eval-after-load 'inf-ruby
  '(progn
     (defun ruby-send-buffer ()
       (interactive)
       (ruby-send-region-and-go (point-min) (point-max)))))

(eval-after-load 'company
  '(push 'company-robe company-backends))

;;; ocaml
(eval-after-load 'tuareg
  '(add-to-list 'auto-mode-alist '("\\.ocamlinit$" . tuareg-mode)))

;;; prolog
(add-to-list 'auto-mode-alist '("\\.pro$" . prolog-mode))

;;; pdf-tools rox
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-view-mode))

;;; bison is dum
(add-to-list 'auto-mode-alist '("\\.l\\'" . jison-flex-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . jison-bison-mode))

;;; llvm stuff
(add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode))

;;; elisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-padding " ")))

;;; jq
(add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))
