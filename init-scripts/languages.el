;;; -*- lexical-binding: t -*-

;;; configuration for various language modes

;;; indentation silliness
(setq-default indent-tabs-mode nil)     ;; use spaces not tabs
(setq tab-width 2)                      ; 4-spacers get at me

;;; commenting
(defun make-comments-like-c ()
  (setq comment-start "/*" comment-end "*/" comment-padding " "))
;; i hate the young whippersnappers and their strange and weird ways
(add-hook 'c++-mode-hook #'make-comments-like-c)
(add-hook 'c-mode-hook #'make-comments-like-c)
(add-hook 'java-mode-hook #'make-comments-like-c)
(add-hook 'fundamental-mode-hook (lambda ()
                                   (setq comment-start "-" comment-end "")))
;;; format comments like a normal person
(add-hook 'r-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'ess-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; " comment-end "")))
(add-hook 'cmake-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'asm-mode-hook (lambda () (setq comment-start "# " comment-end "")))

;;; highlight cursor and auto-fill when over 80 chars in certain modes
(add-hook 'prog-mode-hook #'highlight-80+-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook
          (lambda () (set-fill-column 80)))
(add-hook 'text-mode-hook #'highlight-80+-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook
          (lambda () (set-fill-column 80)))

;;; ...but not others
(add-hook 'LaTeX-mode-hook (lambda ()
                             (auto-fill-mode -1)
                             (highlight-80+-mode -1)))
(add-hook 'markdown-mode-hook (lambda ()
                                (auto-fill-mode -1)
                                (highlight-80+-mode -1)))

;;; perl
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 2)

(defun perl-repl-get-real-str ()
  (buffer-substring-no-properties
   comint-last-output-start
   (process-mark (get-buffer-process (current-buffer)))))

(defun perl-repl-fix-both-early-and-late-insertions (str)
  (goto-char (point-max))
  ;; the noerror argument shouldn't matter, but provided anyway
  (re-search-backward (regexp-quote perl-repl-prompt) nil t)
  (let ((inhibit-read-only t))
    (when (and (> (point) comint-last-output-start)
               (not (char-equal (char-before) (str2char "\n"))))
      (insert "\n")
      (delete-region (point) (+ (point) (length perl-repl-prompt)))
      (goto-char (1- (process-mark (get-buffer-process (current-buffer)))))
      (let ((next-char (char-after))
            (insert-string (concat "\n" perl-repl-prompt)))
        (insert insert-string)
        (delete-char 1)
        (backward-char (length insert-string))
        (insert (char-to-string next-char)))))
  (goto-char (point-max)))

(defun perl-repl-fix-late-insertions (str)
  (let ((real-str (perl-repl-get-real-str)))
    (when (and (string-match-p (concat "\\`" (regexp-quote perl-repl-prompt))
                               real-str)
               (not (string-equal str ""))
               (not (string-equal str perl-repl-prompt)))
      (let ((inhibit-read-only t))
        (backward-char (length real-str))
        (insert (buffer-substring-no-properties
                 (+ (point) (length perl-repl-prompt))
                 (point-max)))
        (forward-char (length perl-repl-prompt))
        (delete-region (point) (point-max))
        (goto-char (point-max))))))

(defun perl-repl-fix-early-insertions (str)
  (let ((real-str (perl-repl-get-real-str)))
    (backward-char (length perl-repl-prompt))
    (when (and (string-equal
                (buffer-substring-no-properties (point) (point-max))
                perl-repl-prompt)
               (not (string-equal real-str perl-repl-prompt)))
      (while (char-equal (char-before) (str2char "\n"))
        (delete-char -1))
      (insert "\n"))
    (goto-char (point-max))))

(defvar perl-repl-prompt "$ ")

(defvar perl-repl-prompt-regexp (concat "^" (regexp-quote perl-repl-prompt)))

(defvar perl-repl-output-filter-functions
      '(perl-repl-fix-both-early-and-late-insertions
        perl-repl-fix-late-insertions
        perl-repl-fix-early-insertions
        ansi-color-process-output
        comint-postoutput-scroll-to-bottom
        comint-watch-for-password-prompt))

(define-derived-mode perl-repl-mode comint-mode "re.pl"
  "Docs

\\<cperl-mode-map>"
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
  (pop-to-buffer
   (make-comint-in-buffer
    "re.pl" nil (concat init-home-folder-dir
                        "init-scripts/perl-repl-helper.sh")))
  (perl-repl-mode))

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

(make-variable-buffer-local 'comment-region-function)
(make-variable-buffer-local 'comment-insert-comment-function)
(defun add-star-comment-region ()
  (setq comment-region-function #'c-comment-region-stars)
  (setq comment-insert-comment-function #'c-comment-end-of-line))
(add-hook 'c-mode-hook #'add-star-comment-region)
(add-hook 'c++-mode-hook #'add-star-comment-region)
(add-hook 'java-mode-hook #'add-star-comment-region)
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
(c-add-style "csharp-mode-style" csharp-cc-style)
(add-hook 'csharp-mode-hook (lambda () (c-set-style "csharp-mode-style")))
(eval-after-load "csharp-mode"
  '(progn
     (c-lang-defconst c-type-list-kwds csharp
                      (cons "else" (c-lang-const c-type-list-kwds csharp)))
     (font-lock-add-keywords
      'csharp-mode
      '(("else" . font-lock-keyword-face)))))

;;; shell
(defun setup-sh-indentation ()
  (setq sh-basic-offset 2)
  (setq sh-indentation 2))
(add-hook 'sh-mode-hook 'setup-sh-indentation)

;;; lisp and related
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of
Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;;; just turn on paredit for scratch buffer
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(defun setup-paredit-with-repl (mode-map)
  (let ((prev-m-n (lookup-key mode-map (kbd "M-n")))
        (prev-m-p (lookup-key mode-map (kbd "M-p"))))
    (paredit-mode)
    (fix-lisp-keybindings)
    (define-key paredit-mode-map (kbd "M-n") prev-m-n)
    (define-key paredit-mode-map (kbd "M-p") prev-m-p)))
(add-hook 'slime-repl-mode-hook
          (lambda () (setup-paredit-with-repl slime-repl-mode-map)))
(add-hook 'cider-repl-mode-hook
          (lambda () (setup-paredit-with-repl cider-repl-mode-map)))
(add-hook 'lisp-interaction-mode-hook 'fix-lisp-keybindings)
(add-hook 'emacs-lisp-mode-hook 'fix-lisp-keybindings)
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'slime-mode-hook 'fix-lisp-keybindings)

;;; start scratch buffer in paredit mode
(with-current-buffer (get-buffer "*scratch*")
  (enable-paredit-mode)
  (fix-lisp-keybindings)
  (eldoc-mode))

(eval-after-load "slime"
  '(define-key slime-autodoc-mode-map (kbd "SPC")
     (lambda (arg) (interactive "p")
       (if (use-region-p)
           (progn
             (delete-region (region-beginning) (region-end))
             (insert " "))
         (slime-autodoc-space arg)))))

;;; scons
;;; use python-mode for scons files
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))
(add-hook 'js-mode-hook
          (lambda () (setq-local electric-indent-chars nil)))

;;; js/css/html
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.gyp\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;;; syntax highlighting
(global-font-lock-mode 1)               ; turn on syntax highlighting
(setq font-lock-maximum-decoration t)   ; turn it ALL the way on

;;; add code folding with hs-minor-mode
(add-hook 'prog-mode-hook #'hs-minor-mode) ; add to all programming modes

;;; pretty sure this has to be done with a hook but i forget why
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;;; slime
(load-my-init-script "slime-setup")

;;; clojure
(load-my-init-script "cider-setup")

;;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;; TODO: figure out why markdown-mode.el chooses to cripple gfm-mode
;; (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
;; (add-to-list 'auto-mode-alist '("readme\\.md\\'" . gfm-mode))
;; (add-to-list 'auto-mode-alist '("Readme\\.md\\'" . gfm-mode))

;;; coffeescript
(eval-after-load "coffee-mode"
  '(define-key coffee-mode-map (kbd "M-;")
     'coffeescript-comment-do-what-i-really-mean))
(add-hook 'coffee-mode-hook (lambda () (setq coffee-tab-width 2)))

;;; latex
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))
