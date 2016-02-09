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

(make-submodule "ESS" "make" nil "1 min")

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

;;; fix magit's inane keybinding changes
(defun magit-reset-push-destination (remote-branch &optional current-branch)
  (interactive (list (magit-read-remote-branch
                      "remote branch to push to" nil
                      (let ((remotes (magit-list-remotes)))
                        (when remotes
                          (concat (car remotes) "/"
                                  (magit-get-current-branch)))))))
  (magit-git-push
   (or current-branch (magit-get-current-branch))
   remote-branch '("-u")))

(defun magit-delete-remote-branch (remote-branch)
  (interactive (list (magit-read-remote-branch "remote branch to delete")))
  (run-hooks 'magit-credential-hook)
  (-let [(remote . target) (magit-split-branch-name remote-branch)]
    (magit-run-git-async "push" "-v" remote "--delete" target)))

(defun magit-add-action-to-popup (action popup &optional test after)
  (let ((actions (plist-get popup :actions))
        (real-test (or test #'equal)))
    (unless (find action actions :test real-test)
      (plist-put
       popup :actions
       (cond ((null after) (append actions (list action)))
             ((eq after t) (cons action actions))
             (t (loop with found = nil
                      for el in actions
                      with results = nil
                      do (progn
                           (push el results)
                           (when (funcall real-test after
                                          (if (listp el) (car el) el))
                             (push action results)
                             (setq found t)))
                      finally (return
                               (if found (reverse results)
                                 (error "No match found for %S" after))))))))))

(with-eval-after-load 'magit-remote
  (magit-define-popup-action 'magit-push-popup ?u "I PUSH WHERE I WANT"
    #'magit-reset-push-destination ?p)
  (magit-define-popup-action 'magit-push-popup ?P "just fuckin push it lol"
    #'magit-push-current-to-upstream ?u)
  (magit-define-popup-action 'magit-pull-popup ?F "just fuckin pull it lol"
    #'magit-pull-from-upstream ?u)
  (magit-add-action-to-popup "DESTRUCTION" magit-push-popup)
  (magit-define-popup-action 'magit-push-popup ?d "DESTROY IT"
    #'magit-delete-remote-branch))

(defun my-magit-reset-hard (commit)
  (interactive (list (my-magit-read-branch-or-commit "Reset head to")))
  (magit-reset-head commit))

(defun my-magit-reset-soft (commit)
  (interactive (list (my-magit-read-branch-or-commit "Soft reset to")))
  (magit-reset-soft commit))

(defun my-magit-reset-hard (commit)
  (interactive (list (my-magit-read-branch-or-commit "Hard reset to")))
  (magit-reset-hard commit))

(magit-define-popup magit-reset-popup
  "Popup console for reset commands."
  'magit-commands
  :man-page "git-reset"
  :actions '((?R "Hard Reset" my-magit-reset-hard)
             (?S "Soft Reset" my-magit-reset-soft)
             (?M "Mixed Reset" my-magit-reset-head)))

(magit-add-action-to-popup
 '(?R "Reset" magit-reset-popup) magit-dispatch-popup nil ?!)

(define-key magit-status-mode-map (kbd "R") #'magit-reset-popup)
(define-key magit-branch-section-map (kbd "R") #'magit-reset-popup)
(define-key magit-file-section-map (kbd "R") #'magit-reset-popup)

(defun my-magit-clean-all ()
  (interactive)
  (magit-run-git-async "clean" "-x" "-f" "-d"))

(magit-define-popup magit-clean-popup
  "Popup console for clean commands."
  'magit-commands
  :man-page "git-clean"
  :actions '((?C "Clean All (be careful!)" my-magit-clean-all)))

(magit-add-action-to-popup
 '(?C "Clean" magit-clean-popup) magit-dispatch-popup nil ?R)

(define-key magit-mode-map (kbd "C") #'magit-clean-popup)

(defun my-magit-diff-paths (a b)
  (interactive (list (completing-read
                      "First file: " (magit-list-files) nil t)
                     (completing-read
                      "Second file: " (magit-list-files) nil t)))
  (magit-diff-paths a b))

(magit-define-popup-action 'magit-diff-popup ?p "Diff paths"
  #'my-magit-diff-paths)

(defconst git-gutter-fringe-hack-hooks git-gutter:update-hooks)
(defvar git-gutter-fringe-hack-not-modes '(minibuffer-inactive-mode))
(define-minor-mode git-gutter-fringe-hack-mode
  "hack to make git gutter work"
  :group 'git-gutter
  :init-value nil
  :global nil
  :lighter git-gutter:lighter
  (unless (apply #'derived-mode-p git-gutter-fringe-hack-not-modes)
    (if git-gutter-fringe-hack-mode
        (progn
          (loop for hook in git-gutter-fringe-hack-hooks
                do (add-hook hook #'git-gutter t t))
          (git-gutter))
      (loop for hook in git-gutter-fringe-hack-hooks
            do (remove-hook hook #'git-gutter t))
      (git-gutter:clear))))
(defun git-gutter-fringe-hack-turn-on ()
  (git-gutter-fringe-hack-mode +1))
(define-global-minor-mode global-git-gutter-fringe-hack-mode
  git-gutter-fringe-hack-mode git-gutter-fringe-hack-turn-on
  :group 'git-gutter)
(global-git-gutter-fringe-hack-mode)

;;; make git-gutter run on magit actions
(defadvice magit-run-git (around run-git-gutter activate)
  (let ((prev-mod-files (magit-modified-files)))
    ad-do-it
    (loop
     for file in (union prev-mod-files (magit-modified-files)
                        :test #'equal)
     for buf = (get-file-buffer file) when buf
     do (with-current-buffer buf (git-gutter)))))

;;; parenthesis matching and more
;;; turn pair parens on
(electric-pair-mode t)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}))
;;; match parens when cursor on top
(show-paren-mode t)

;;; rainbow delimiters!
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(defvar rainbow-delims-modes '(LaTeX-mode-hook))
(loop for mode-hook in rainbow-delims-modes
      do (add-hook mode-hook #'rainbow-delimiters-mode))

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
         ("MATLAB" (mode . matlab-mode))
         ("Octave" (mode . octave-mode))
         ("OCaml" (mode . tuareg-mode))
         ("lexer" (name . "\\.l\\'"))
         ("grammar" (or (name . "\\.y\\'")
                        (name . "\\.jison\\'")))
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
(eval-after-load 'sourcemap
  '(progn
     (defun coffee-goto-sourcemap-and-delete (props)
       (sourcemap-goto-corresponding-point props)
       (delete-file (plist-get props :sourcemap)))
     (eval-after-load 'coffee-mode
       '(add-hook 'coffee-after-compile-hook
                  #'coffee-goto-sourcemap-and-delete))))
(eval-after-load 'coffee-mode
  '(defadvice coffee-indent-shift-left (around shift-negative activate)
     (let ((start (ad-get-arg 0))
           (end (ad-get-arg 1))
           (count (ad-get-arg 2)))
       (cond
        ((listp count)
         (setq ad-return-value
               (coffee-indent-shift-right start end 1)))
        ((and count (negativep count))
              (setq ad-return-value
                    (coffee-indent-shift-right start end (- count))))
        (t ad-do-it)))))

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

;;; SKEWER DIS ISH
(eval-after-load 'skewer-mode
  '(progn
     (skewer-setup)
     (add-hook 'skewer-repl-mode-hook
               (lambda () (setq comint-process-echoes nil)))))

(eval-after-load 'org-mode '(require 'org-plot))

(eval-after-load 'ag
  '(progn
     (defadvice ag/search (after remember-stuff activate)
       (with-current-buffer ad-return-value
         (setq ag-args (ad-get-args 0))))
     (defadvice ag/search (around same-window activate)
       (let ((cur-win (selected-window)))
         ad-do-it
         (let ((res-buf ad-return-value))
           (quit-windows-on res-buf)
           (with-selected-window cur-win
             (display-buffer-same-window res-buf nil)))))))

;;; have to do it this way because setting mark in an 'around advice screws
;;; around with `thing-at-point'
(defmacro setup-marker-around-helm-command (cmd)
  `(defadvice ,cmd
       (before ,(intern (concat "mark-stuff-" (symbol-name cmd))) activate)
     (let ((mk (make-marker)))
       (set-marker mk (point))
       (push mk global-mark-ring))))
(setup-marker-around-helm-command helm-swoop)
(setup-marker-around-helm-command helm-multi-swoop-all)

(defun turn-off-linum ()
  (interactive)
  (linum-mode -1))

(add-hook 'eww-mode-hook #'turn-off-linum)

(add-to-list 'dired-compress-files-alist
             '("\\.tgz\\'" . "tar -c %i | gzip -c9 > %o"))
