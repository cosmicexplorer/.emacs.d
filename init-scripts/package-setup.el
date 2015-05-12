;;; setup done for large packages which need a kick in the pants to get going

;;; ein
(require 'ein)
(setq ein:use-auto-complete-superpack t)
(require 'smartrep)
(setq ein:use-smartrep t)

;;; helm
(helm-mode t)
(add-hook 'eshell-mode-hook
          ;; add pcomplete with helm support to eshell
          (lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

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
(when (executable-find "git")
  (let ((ess-git-folder (concat init-home-folder-dir "ESS/.git")))
    (unless (file-directory-p ess-git-folder)
      (let ((ess-update-buf-name "*ESS-create-errors*")
            (prev-wd default-directory))
        (cd init-home-folder-dir)
        (unwind-protect
            (let ((ess-submodule-out-buf
                   (get-buffer-create ess-update-buf-name)))
              (unless (zerop (call-process "git" nil ess-submodule-out-buf nil
                                           "submodule" "init"))
                (throw 'ess-failure "init failed"))
              (unless (zerop (call-process "git" nil ess-submodule-out-buf nil
                                           "submodule" "update"))
                (throw 'ess-failure "update failed")))
          (cd prev-wd))
        (kill-buffer ess-update-buf-name)))))
(when (executable-find "make")
  (let ((ess-make-output-buf (get-buffer-create "*ESS-make-errors*")))
    (set-process-sentinel
     (start-process "make-ess" (buffer-name ess-make-output-buf)
                    "make" "-C"
                    (expand-file-name (concat init-home-folder-dir "ESS")))

     (lambda (proc ev)
       (unless (string= ev "finished\n")
         (when (process-live-p proc) (kill-process proc))
         (switch-to-buffer (process-buffer proc))
         (message ev))
       (kill-buffer (process-buffer proc))))))
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
(setq magit-auto-revert-mode nil)
;; automatically revert unmodified (saved) buffers that magit changes through
;; e.g. pull, merge

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
      '(("home"
         ("eshell" (mode . eshell-mode))
         ("dired" (mode . dired-mode))
         ("readme" (filename . "README"))
         ("TODO" (filename . "TODO"))
         ("TODO" (filename . "todo"))
         ("json" (filename . "\\.json\\'"))
         ("gyp" (filename . "\\.gyp\\'"))
         ("javascript" (mode . js2-mode))
         ("emacs-config" (or (filename . ".emacs.d")
                             (filename . "emacs-config")
                             (filename . ".emacs")))
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
         ("helm" (or (name . "helm")
                     (name . "Helm")))
         ("makefile" (or (filename . "\\Makefile\\'")
                         (filename . "\\makefile\\'")))
         ("dired" (mode . dired-mode))
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
           (name . "Doc")))
         ("yaml" (filename . "\\.yaml\\'"))
         ("man pages" (mode . Man-mode))
         ("default" (name . "")))))

;;; more ibuffer stuff
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t) ;; automatically updates buffer list
             (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-expert t) ;; only prompt when modified buffer is killed
(setq ibuffer-show-empty-filter-groups nil) ;; only show full filter groups

;;; personal color theme initialization
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-danny)))

;;; flycheck lol
(eval-after-load 'flycheck
  '(flycheck-package-setup))
