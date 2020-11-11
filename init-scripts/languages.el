;;; -*- lexical-binding: t -*-

;;; configuration for various language modes
(add-hook #'prog-mode-hook #'display-fill-column-indicator-mode)

;;; indentation silliness
(setq-default indent-tabs-mode nil)     ;; use spaces not tabs
(setq tab-width 2)                      ; 4-spacers get at me

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
(add-to-list 'auto-mode-alist '("\\.[sS]\\'" . asm-mode))

;;; Add preferred custom .emacsrc file.
(add-to-list 'auto-mode-alist '("\\.emacsrc\\'" . emacs-lisp-mode))

;;; more ess-mode nonsense
(add-hook 'ess-mode-hook #'warning-highlights-mode)
(add-hook 'ess-mode-hook #'auto-fill-mode)
(with-eval-after-spec highlight-80+
  (add-hook 'ess-mode-hook #'highlight-80+-mode))

(defun my-inf-ess-end-send-input ()
  (interactive)
  (goto-char (point-max))
  (funcall-interactively #'inferior-ess-send-input))

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

;;; FIXME: make this work!
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
(add-to-list 'auto-mode-alist '("\\.[rR]\\'" . R-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]script\\'" . R-mode))

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
(with-eval-after-spec highlight-80+
  (defconst no-auto-fill-modes
    '(litcoffee-mode tex-mode markdown-mode elisp-byte-code-mode))
  (defun selective-turn-on-auto-fill ()
    (unless (member major-mode no-auto-fill-modes)
      (highlight-80+-mode)
      (auto-fill-mode)))
  (add-hook 'prog-mode-hook #'selective-turn-on-auto-fill))

(require 'highlight-sexp)
(add-hook 'lisp-mode-hook #'highlight-sexp-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-sexp-mode)

(require 'highlight-stages)
(add-hook 'lisp-mode-hook #'highlight-stages-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-stages-mode)

;;; coffeescript!!!!
(require 'coffee-mode)
(defun coffee-compile-region (start end)
  "Compiles a region and displays the JavaScript in a buffer called
`coffee-compiled-buffer-name'."
  (interactive "r")
  (coffee-cleanup-compile-buffer)
  (coffee-start-generate-sourcemap-process start end))

(defvar coffee-string-interpolation-regexp "#{[^}]*}")

(defun my-coffee--get-compile-buffer (src-buf)
  (with-current-buffer (get-buffer-create coffee-compiled-buffer-name)
    (setq-local default-directory (buffer-local-value 'default-directory src-buf))
    (current-buffer)))

(defun coffee-start-compile-process (curbuf line column)
  (lambda (start end)
    (let ((proc (apply 'start-file-process "coffee-mode"
                       (my-coffee--get-compile-buffer curbuf)
                       coffee-command (append coffee-args-compile '("-s" "-p"))))
          (curfile (buffer-file-name curbuf)))
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel
       proc (coffee-compile-sentinel curbuf curfile line column))
      (with-current-buffer curbuf
        (process-send-region proc start end))
      (process-send-string proc "\n")
      (process-send-eof proc)
      (setq coffee--process proc))))


(defconst my-coffee--comint-single-line-prompt "coffee>")
(defconst my-coffee--comint-multiline-prompt "------>")
(defconst my-coffee--comint-multiline-continuation-prompt ".......")

(require 'rx)
(defconst my-coffee--fused-prompt-regexp
  (rx
   (: bol
      (| (eval my-coffee--comint-single-line-prompt)
         (eval my-coffee--comint-multiline-prompt)))))

;;; c/c++/java
(setq-default c-basic-offset 2) ;; cc-mode uses this instead of tab-width
;;; stop auto-inserting newlines after semicolons i don't like that
(setq c-hanging-semi&comma-criteria nil)
(setq c-default-style nil)
;;; doesn't work, hence camel-case-{left,right}-word in functions.el
(subword-mode)
(setq c-electric-flag nil)
(defconst cc-mode-maps (list c-mode-map c++-mode-map java-mode-map))
(add-hook
 'c-initialization-hook
 '(lambda ()
    (add-keybinding-to-mode-maps
     "RET" #'newline-and-indent-fix-cc-mode cc-mode-maps)))
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

;;; shell
(defun setup-sh-indentation ()
  (ignore-errors (warning-highlights-mode-activate))
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

(with-eval-after-spec paredit
  (add-to-list 'paredit-space-for-delimiter-predicates #'at-lisp-splice-p)
  (add-to-list 'paredit-space-for-delimiter-predicates #'at-elisp-char-literal-p))

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

(add-to-list 'auto-mode-alist '("\\.aurora\\'" . python-mode))

;;; js/css/html
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(defconst json-mode-map
  (make-keymap-from-bindings
   '(("<C-tab>" . json-fmt))))

(define-derived-mode json-mode javascript-mode "JSON"
  "Major mode for editing json documents.")
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.\\(?:js\\)?beautifyrc\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.babelrc" . json-mode))

;;; config files
(add-to-list 'auto-mode-alist '("\\.clang-format\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.ini\\b" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.pantsrc" . conf-mode))

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
(load-my-script "slime-setup" "init-scripts")

;;; clojure
(load-my-script "cider-setup" "init-scripts")

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
  (cond
   ((derived-mode-p 'gfm-mode))
   ((and (not no-gfm)
         (zerop (call-process
                 "git" nil nil nil "rev-parse" "--git-dir")))
    (gfm-mode))
   (t (let ((cmd (format
                  "pandoc -f markdown%s -t html -"
                  (if markdown-enable-math "+tex_math_dollars" ""))))
        (setq-local markdown-command cmd)))))

(add-hook 'markdown-mode-hook #'setup-markdown-mode)

(defun set-gfm-markdown-command ()
  (setq-local markdown-command "pandoc -f markdown_github -t html -"))

(add-hook 'gfm-mode-hook #'set-gfm-markdown-command)

;;; coffeescript
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map (kbd "M-;")
       'coffeescript-comment-do-what-i-really-mean)
     (add-hook 'coffee-mode-hook #'rainbow-mode)
     (setq coffee-args-compile (cl-remove "-m" coffee-args-compile :test #'string-equal))))
(add-hook 'coffee-mode-hook (lambda () (setq coffee-tab-width 2)))
(define-derived-mode cjsx-mode coffee-mode "CJSX"
  "Major mode for editing CJSX."
  (coffee-mode)
  (setq mode-name "CJSX")
  (setq major-mode 'cjsx-mode))
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . cjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(add-hook 'litcoffee-mode-hook
          (lambda ()
            (set (make-local-variable 'coffee-args-compile)
                 (cons "-l" coffee-args-compile))))

(defvar my-coffee--compiled-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode my-coffee--compiled-output-mode js-mode "WE LOVE COFFEE!!!!!"
  "Major mode for inspecting coffeescript compile output."
  :after-hook (read-only-mode 1))

(add-hook 'coffee-after-compile-hook (lambda (&rest _args)
                                       (with-current-buffer (get-buffer coffee-compiled-buffer-name)
                                         (my-coffee--compiled-output-mode))))


(defun my-coffee--clear-sideways-whitespace-on-line ()
  "If there is any whitespace to the left or right of point on this line, delete it!"
  (re-search-backward "\\([^[:space:]]\\)\\([[:space:]]*\\)")
  (replace-match "" nil nil nil 2)
  (re-search-forward "\\([[:space:]]*\\)\\([^[:space:]]\\)")
  (replace-match "" nil nil nil 1))

(defun my-coffee-insert-type-parameter ()
  "Insert a generic type parameter, for use at the beginning of defining a generic function.

Point is placed at: ###::<|point|>###."
  (interactive)
  (my-coffee--clear-sideways-whitespace-on-line)
  (insert " ###::<>###")
  (backward-char 4))

(defun my-coffee-insert-fn-type-annotation ()
  "Insert a type annotation for an argument, variable, or really whatever!!!

There is some whitespace trickery, then point is inserted at: ###: |point|###."
  (interactive)
  (my-coffee--clear-sideways-whitespace-on-line)
  (insert "###: ###")
  (backward-char 3))

(defun my-coffee-goto-beg-of-type-annotation ()
  "When inside a type annotation with ###::?###, go to the beginning of the first #!"
  (interactive)
  (re-search-backward "###::?"))

(defun my-coffee-goto-end-of-type-annotation (&optional no-insert-space)
  "When inside a type annotation with ###::?###, go to the end of the last #!"
  (interactive "PP")
  (re-search-forward "###[^:]")
  (backward-char)
  (when (and (looking-at-p "[[:space:]]*$")
             (not no-insert-space))
    (my-coffee--clear-sideways-whitespace-on-line)
    (insert " ")))

(defvar my-coffee-enhanced-flow-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-a <up>") #'my-coffee-insert-type-parameter)
    (define-key map (kbd "M-a M-a") #'my-coffee-insert-fn-type-annotation)
    (define-key map (kbd "M-a <left>") #'my-coffee-goto-beg-of-type-annotation)
    (define-key map (kbd "M-a M-<left>") #'my-coffee-goto-beg-of-type-annotation)
    (define-key map (kbd "M-a <right>") #'my-coffee-goto-end-of-type-annotation)
    (define-key map (kbd "M-a M-<right>") #'my-coffee-goto-end-of-type-annotation)
    map))

(define-derived-mode my-coffee-enhanced-flow-mode litcoffee-mode "ENHANCE!!"
  "Major mode for editing literate coffeescript with flow type annotations.")

(add-hook 'my-coffee-enhanced-flow-mode-hook (z (auto-fill-mode -1)))
(with-eval-after-spec highlight-80+
  (add-hook 'my-coffee-enhanced-flow-mode-hook (z (highlight-80+-mode -1))))

(add-hook 'litcoffee-mode-hook (z (unless (derived-mode-p 'my-coffee-enhanced-flow-mode)
                                    (my-coffee-enhanced-flow-mode))))

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
(defvar prolog-program-name nil
  "Because `prolog-mode' doesn't like defining its variables.")
(setq prolog-program-name
      (cl-find-if #'executable-find '("swipl" "prolog")))

;;; prolog, but a new version used in spack!
(add-to-list 'auto-mode-alist '("\\.lp\\'" . prolog-mode))

;;; pdf-tools rox
(add-to-list 'auto-mode-alist '("\\.pdf$" . pdf-view-mode))

;;; bison is dum
(add-to-list 'auto-mode-alist '("\\.l\\'" . jison-flex-mode))
(add-to-list 'auto-mode-alist '("\\.y\\'" . jison-bison-mode))

;;; llvm stuff
(add-to-list 'auto-mode-alist '("\\.ll\\'" . llvm-mode))

;;; elisp
(add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode)

;;; jq
(add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))

;;; antlr
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

;;; rust

(defun add-pipes-to-local-electric-pairs ()
  (let ((pipe-char ?|))
    (unless (alist-get pipe-char electric-pair-pairs)
      (setq-local electric-pair-pairs `((,pipe-char . ,pipe-char) ,@electric-pair-pairs)))))

(require 'rustic)
(add-hook 'rust-mode-hook #'rustic-mode)
(add-hook 'rustic-mode-hook #'eldoc-mode)
(add-hook 'rustic-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'add-pipes-to-local-electric-pairs)
(add-hook 'rust-mode-hook (z (setq comment-start "/* "
                                   comment-end " */")))

(setq rustic-racer-rust-src-path
        (format "%s/%s"
                (trim-whitespace (shell-command-to-string "rustc --print sysroot"))
                "lib/rustlib/src/rust/src"))


;;; Pants support!
(define-derived-mode build-file-mode python-mode "BUILD"
  (setq-local python-indent-offset 4))
(add-to-list 'auto-mode-alist '("\\(\\`\\|\\<\\)BUILD\\(\\.[a-zA-Z]+\\)?\\'" . build-file-mode))

;;; makefile support!
(defun makefile-insert-tab ()
  (insert "	"))
(add-hook 'makefile-mode-hook
          (z (setq-local indent-line-function #'makefile-insert-tab)))

(defconst pants-cur-year-header-fmt-leading-whitespace "
# coding=utf-8
# Copyright %s Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

from __future__ import absolute_import, division, print_function, unicode_literals
")

(defun pants-python-header ()
  (let* ((cur-year (format-time-string "%Y"))
         (formatted-text
          (format pants-cur-year-header-fmt-leading-whitespace
                  cur-year)))
    (replace-regexp-in-string (rx bos (one-or-more (or whitespace "\n"))) "" formatted-text)))

(defun pants-insert-python-header ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert (pants-python-header))))
