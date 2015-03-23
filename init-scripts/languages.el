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



;;; perl
(fset 'perl-mode 'cperl-mode)
(setq cperl-indent-level 2)

;;; c/c++/java
(setq-default c-basic-offset 2) ;; cc-mode uses this instead of tab-width
(setq c-hanging-semi&comma-criteria nil) ; stop auto-inserting newlines after ;
                                        ; semicolons i don't like that
(setq c-default-style nil)
;;; doesn't work, hence camel-case-{left,right}-word in functions.el
(subword-mode)

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

;;; scons
;;; use python-mode for scons files
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))
