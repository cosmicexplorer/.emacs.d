;;; -*- lexical-binding: t -*-

;;; setup done for large packages which need a kick in the pants to get going

;;; ein
(require 'ein)
(eval-after-load 'ein
  '(progn
     (setq ein:use-auto-complete-superpack t)
     (require 'smartrep)
     (setq ein:use-smartrep t)))

;;; helm
(helm-mode t)
(add-hook 'eshell-mode-hook
          ;; add pcomplete with helm support to eshell
          (lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))
(setq helm-c-source-swoop-match-functions
      (remove 'helm-fuzzy-match helm-c-source-swoop-match-functions))
(setq helm-c-source-swoop-search-functions
      (remove 'helm-fuzzy-match helm-c-source-swoop-search-functions))

;;; undo-tree
(global-undo-tree-mode)                 ; put it EVERYWHERE
;;; persist across file saves
(setq undo-tree-auto-save-history t)
;;; show diffs in undo tree visualizer
(setq undo-tree-visualizer-diff t)
;;; large undos oh man
(setq undo-outer-limit 50000000)

;;; smart-tab
(global-smart-tab-mode 1)               ; put it EVERYWHERE

;; julia/R from ESS
;;; because it's not detecting this variable correctly on windows fsr
(setq ess-lisp-directory (concat init-home-folder-dir "ESS/lisp"))

(make-submodule "ESS" "make" nil)

;;; now let's load it
(when (file-directory-p (concat init-home-folder-dir "/ESS/lisp"))
  (add-to-list 'load-path (concat init-home-folder-dir "/ESS/lisp"))
  (require 'ess-site)
  (when (executable-find "julia-basic")
    (setq inferior-julia-program-name (executable-find "julia-basic"))
    (add-to-list 'ess-tracebug-search-path "/usr/share/julia/base/"))
  (ess-toggle-underscore nil))

;;; magit
(setq magit-last-seen-setup-instructions "1.4.0")
;; automatically revert unmodified (saved) buffers that magit changes through
;; e.g. pull, merge
(setq magit-auto-revert-mode t)
;;; this is pretty brittle to internal changes in magit, hopefully it stays
;;; relatively stable
(defvar magit-header-section-args '((diffstat) (headers)))
(defun magit-tab-dwim ()
  (interactive)
  (let* ((sec (magit-section-ident (magit-current-section)))
         (sec-diff
          (if (cl-find-if
               (lambda (item)
                 (equal item '(diffstat)))
               sec)
              (apply #'remove-multiple-in-seq
                     (cons sec magit-header-section-args))
            sec)))
    (if (= (length sec) (length sec-diff))
        (call-interactively #'magit-section-toggle)
      (push-mark)
      (magit-section-goto (magit-get-section sec-diff)))))

;;; parenthesis matching and more
;;; turn pair parens on
(electric-pair-mode t)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}))
;;; match parens when cursor on top
(show-paren-mode t)

;;; rainbow delimiters!
(global-rainbow-delimiters-mode)

(ido-mode t)
;;; makes searching fuzzier
(setq ido-enable-flex-matching t)

;; when opening a file the cursor will be at the last saved position
(setq save-place-file (concat init-home-folder-dir "saveplace"))
(setq-default save-place t)
(setq save-place t)

;;;;; ibuffer stuff
;;;; re: http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
;; you can add different groups too, not just home, in case you ever want to
(setq ibuffer-saved-filter-groups
      `(("home"
         ("eshell" (mode . eshell-mode))
         ("dired" (mode . dired-mode))
         ("readme" (filename . "README"))
         ("TODO" (filename . "TODO"))
         ("TODO" (filename . "todo"))
         ("json" (filename . "\\.json\\'"))
         ("gyp" (filename . "\\.gyp\\'"))
         ("clojure" (mode . clojure-mode))
         ("javascript" (mode . js2-mode))
         ("emacs-config" (or (filename . "\\.emacs\\.d")
                             (filename . "emacs-config")
                             (filename . "\\.emacs")
                             (filename . ,user-init-file)))
         ("doc-view" (mode . doc-view-mode))
         ("web" (or (mode . html-mode)
                    (mode . css-mode)
                    (mode . html-helper-mode)
                    (mode . nxhtml-mode)
                    (mode . php-mode)))
         ("subversion" (name . "\*svn"))
         ("magit" (name . "\*magit\*"))
         ("git" (name . "\*git"))
         ("perl" (mode . cperl-mode))
         ("lua" (mode . lua-mode))
         ("helm" (or (name . "helm")
                     (name . "Helm")))
         ("makefile" (or (filename . "\\Makefile\\'")
                         (filename . "\\makefile\\'")))
         ;; because just detecting julia-mode doesn't work fsr
         ("julia" (filename . "\\.jl\\'"))
         ("r" (or (filename . "\\.R\\'")
                  (filename . "\\.r\\'")))
         ("LaTeX" (filename . "\\.tex\\'"))
         ("BibTeX" (filename . "\\.bib\\'"))
         ("cmake" (mode . cmake-mode))
         ("text" (filename . "\\.txt\\'"))
         ("cxx header" (filename . "\\.h\\'"))
         ("c" (mode . c-mode))
         ("c++" (mode . c++-mode))
         ("c#" (mode . csharp-mode))
         ("java" (mode . java-mode))
         ("python" (mode . python-mode))
         ("markdown" (mode . markdown-mode))
         ;; emacs-config filter mostly blocks this but it's whatever
         ("emacs-lisp" (mode . emacs-lisp-mode))
         ("lisp" (mode . lisp-mode))
         ("go" (mode . go-mode))
         ("perl" (mode . perl-mode))
         ("haskell" (mode . haskell-mode))
         ("fortran" (mode . fortran-mode))
         ("ada" (mode . ada-mode))
         ("ruby" (mode . ruby-mode))
         ("hex" (mode . hexl-mode))
         ("qmake" (mode . qmake-mode))
         ("org" (mode . org-mode))
         ("shell script" (mode . sh-mode))
         ("coffeescript" (mode . coffee-mode))
         ("literate coffeescript" (or (filename . "\\.litcoffee\\'")
                                      (filename . "\\.coffee\\.\\'")))
         ("less" (mode . less-css-mode))
         ("genbank" (filename . "\\.gb\\'"))
         ("fasta" (or (filename . "\\.fna\\'")
                      (filename . "\\.fasta\\'")))
         ("latex package" (filename . "\\.sty\\'"))
         ("gitignore" (filename . "\\.gitignore\\'"))
         ("clang-format" (filename . "\\.clang-format\\'"))
         ("syscall table" (filename . "\\.tbl\\'"))
         ("emacs-system"
          (or (name . "\*eshell\*")
              (name . "\*scratch\*")
              (name . "\*Messages\*")
              (name . "\*Compile-Log\*")
              (name . "\*ESS\*")
              (name . "\*compilation\*")
              (name . "\*Backtrace\*")
              (name . "\*Shell Command Output\*")))
         ("documentation"
          (or
           (name . "help")
           (name . "Help")
           (name . "Apropos")
           (name . "apropos")
           (name . "Info")
           (name . "info")
           (name . "doc")
           (name . "Doc")
           (mode . Man-mode)))
         ("yaml" (filename . "\\.yaml\\'"))
         ("default" (name . "")))))

;;; more ibuffer stuff
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t) ;; automatically updates buffer list
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-expert t) ;; only prompt when modified buffer is killed
(setq ibuffer-show-empty-filter-groups nil) ;; only show full filter groups
(defadvice ibuffer-toggle-filter-group (after center activate)
  (recenter))

;;; personal color theme initialization
(eval-after-load "color-theme-danny"
  '(progn
     (color-theme-initialize)
     (color-theme-danny)))

;;; flycheck lol
(eval-after-load 'flycheck
  '(flycheck-package-setup))

;;; npm executables
(defvar npm-bin-dir nil
  "Location of npm binary files.")
(when (executable-find "npm")
  (cd init-home-folder-dir)
  (call-process "npm" nil nil nil "install")
  (setq npm-bin-dir
        ;; destroy trailing newline
        (let ((str (shell-command-to-string "npm bin")))
          (substring str 0 (1- (length str)))))
  (eval-after-load "web-beautify"
    `(progn
       (setq web-beautify-html-program (concat ,npm-bin-dir "/html-beautify"))
       (setq web-beautify-css-program (concat ,npm-bin-dir "/css-beautify"))
       (setq web-beautify-js-program (concat ,npm-bin-dir "/js-beautify"))))
  (eval-after-load "coffee-mode"
    `(progn
       (setq coffee-command (concat ,npm-bin-dir "/coffee"))
       (setq js2coffee-command (concat ,npm-bin-dir "/js2coffee")))))

;;; company
(add-hook 'after-load-init-hook #'global-company-mode)

;;; ggtags is dumb lol
(defadvice ggtags-find-tag-dwim (before advice-ggtags-set-mark activate)
  (push-mark))
(defadvice ggtags-find-definition (before advice-ggtags-set-more-mark activate)
  (push-mark))

;;; web-beautify-js makes my cursor move around
(defadvice web-beautify-js (after dont-move activate) (recenter))

;;; dired async stuff
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;;; GIT GUTTER IS GR8
(global-git-gutter-mode)

;;; SKEWER DIS ISH
(eval-after-load 'skewer-mode
  '(progn
     (skewer-setup)
     (add-hook 'skewer-repl-mode-hook
               (lambda () (setq comint-process-echoes nil)))))

(eval-after-load 'org-mode '(require 'org-plot))
