;;; configuration for various language modes

;;; indentation silliness
(setq-default indent-tabs-mode nil)     ;; use spaces not tabs
(setq tab-width 2)                      ; 4-spacers get at me

;;; format comments like a normal person
(add-hook 'c-mode-hook (lambda () (setq comment-start "// " comment-end   "")))
(add-hook 'fundamental-mode-hook (lambda ()
                                   (setq comment-start "- " comment-end "")))
;;; TODO: this doesn't work! figure out why
(add-hook 'r-mode-hook (lambda () (setq comment-start "# " comment-end   "")))
(add-hook 'ess-mode-hook (lambda () (setq comment-start "# " comment-end   "")))
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; " comment-end "")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; "
                                                 comment-end "")))
(add-hook 'cmake-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'asm-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq comment-start "% " comment-end "")
                             (auto-fill-mode -1)
                             (highlight-80+-mode -1)))
(add-hook 'org-mode-hook (lambda ()
                             (setq comment-start "% " comment-end "")
                             (auto-fill-mode -1)
                             (highlight-80+-mode -1)))
(add-hook 'markdown-mode-hook (lambda ()
                                (auto-fill-mode -1)
                                (highlight-80+-mode -1)))
(add-hook 'html-mode-hook (lambda ()
                            (setq comment-start "<!--" comment-end "-->")))

;;; highlight cursor and auto-fill when over 80 chars in certain modes
(add-hook 'prog-mode-hook #'highlight-80+-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'(lambda ()
                              (set-fill-column 80)))
(add-hook 'text-mode-hook #'highlight-80+-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'(lambda ()
                              (set-fill-column 80)))


;;; perl
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 2)
;;; TODO: fix this
;; (eval-after-load "cperl-mode"
;;   '(define-key cperl-mode-map (kbd "C-c C-k") 'smart-compile))

;;; c/c++/java
(setq-default c-basic-offset 2) ;; cc-mode uses this instead of tab-width
(setq c-hanging-semi&comma-criteria nil) ; stop auto-inserting newlines after ;
                                        ; semicolons i don't like that
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
(c-set-offset 'innamespace 0)
;;; add modes for specific filetypes
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.txx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))

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
(add-hook 'slime-mode-hook 'fix-lisp-keybindings)
(add-hook 'lisp-interaction-mode-hook 'fix-lisp-keybindings)
(add-hook 'emacs-lisp-mode-hook 'fix-lisp-keybindings)
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'fix-lisp-keybindings)

;;; start scratch buffer in paredit mode
(with-current-buffer (get-buffer "*scratch*")
  (enable-paredit-mode)
  (fix-lisp-keybindings)
  (eldoc-mode))



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

;;; syntax highlighting
(global-font-lock-mode 1)               ; turn on syntax highlighting
(setq font-lock-maximum-decoration t)   ; turn it ALL the way on

;;; add code folding with hs-minor-mode
(add-hook 'prog-mode-hook #'hs-minor-mode) ; add to all programming modes

;;; pretty sure this has to be done with a hook but i forget why
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;;; clojure
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-log-messages t)
(global-company-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'enable-paredit-mode)
(add-hook 'cider-mode-hook 'subword-mode)
(defalias 'cider 'cider-jack-in)

;;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("readme\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("Readme\\.md\\'" . gfm-mode))

;;; coffeescript
(eval-after-load "coffee-mode"
  '(define-key coffee-mode-map (kbd "M-;")
     'coffeescript-comment-do-what-i-really-mean))

;;; latex
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))