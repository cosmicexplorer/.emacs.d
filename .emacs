;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014

;;; i've changed quite a few files besides just this one and if you wish to
;;; upgrade the associated external packages
;;; you'll have to re-add those changes for the whole frail system to work
;;; alternatively you can just email me that an update occurred in some package
;;; and i'll merge and push the changes
;;;;; specific sections are demarcated by five semicolons, like this line
;;; do a global search through all such marks to go through all major sections

;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;;; globally useful things
;; stop the intro to emacs buffer
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(define-key help-map "a" 'apropos)      ; get useful help for once
(menu-bar-mode 0) ;; remove menu bar for another line of space
(tool-bar-mode 0)
(setq scroll-step 1)
(setq scroll-conservatively 10000)
;;; point in buffer not preserved between scrolls due to internal emacs logic
;; (point must always be in frame), but save-point and goto-saved-point work
(scroll-bar-mode 0)
(set-face-attribute 'default nil :height 100)
(transient-mark-mode 0)                 ; turn that off lol
(setq shift-select-mode t)
(setq scroll-preserve-screen-position t)
;;; indentation silliness
(add-hook 'after-change-major-mode-hook  ; show whitespace
          '(lambda ()
             (setq show-trailing-whitespace t)))
(setq-default indent-tabs-mode nil)     ;; use spaces not tabs
(setq tab-width 4)
(setq-default c-basic-offset 2) ;; cc-mode uses this instead of tab-width
;; Remove trailing whitespace from a line
(setq-default nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; fix input issues in xterm (can't hold down shift and up arrow to
;; highlight stuff)
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "[4~" [end]))

;; starts emacs in server form so i can use emacsclient to add files
;; but only if server isn't already started
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;; FONTS
(add-to-list 'default-frame-alist '(font . "Telegrama 10"))
(set-face-attribute 'default t :font "Telegrama 10")
(set-frame-font "Telegrama 10")

;;; so i can sudo edit files with C-x C-f /sudo::/path/to/file
(require 'tramp)

;;; have normal delete/selection (type over selected text to delete)
(delete-selection-mode 1)

;; do backups well and put them into a separate folder
(setq backup-directory-alist `(("." . "~/.emacs.d/autosaved-files")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
;; do the same thing for undo-tree history
(setq undo-tree-history-directory-alist `(("."
                                           . "~/.emacs.d/undo-tree-history")))

;;; highlight cursor when over 80 chars
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'highlight-80+)
(add-hook 'prog-mode-hook #'highlight-80+-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'(lambda ()
                              (set-fill-column 80)))
(add-hook 'text-mode-hook #'highlight-80+-mode)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'(lambda ()
                              (set-fill-column 80)))

(require 'misc-cmds)

;;;;; setup specific modes for specific filetypes
;; setup slime
;; setup load-path and autoloads
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(add-to-list 'slime-contribs 'slime-repl)
(load (expand-file-name "~/.emacs.d/quicklisp/slime-helper.el")) ;; quicklisp
(add-hook 'emacs-lisp-mode-hook 'fix-lisp-keybindings)
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'fix-lisp-keybindings)

;; paredit
(add-to-list 'load-path "~/.emacs.d/paredit")
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of
Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; haskell mode
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;; random per-language editing things
;; format comments like a normal person
(add-hook 'c-mode-hook (lambda () (setq comment-start "// " comment-end   "")))
(add-hook 'fundamental-mode-hook (lambda ()
                                   (setq comment-start "- " comment-end "")))
(add-hook 'r-mode-hook (lambda () (setq comment-start "# " comment-end   "")))
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; " comment-end "")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; "
                                                 comment-end "")))
(add-hook 'cmake-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'asm-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq comment-start "% " comment-end "")
                             (auto-fill-mode -1)
                             (highlight-80+-mode -1)))
(add-hook 'html-mode-hook (lambda ()
                            (setq comment-start "<!--" comment-end "-->")))

(setq c-hanging-semi&comma-criteria nil) ; stop inserting newlines after
                                        ; semicolons i don't like that
(setq c-default-style "gnu")
(subword-mode)                          ; turn camel-case on
(setq auto-mode-alist                   ; use python-mode for scons files
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(add-to-list 'load-path "~/.emacs.d/js-beautify-emacs")
(require 'js-beautify)
(add-hook 'js-mode-hook 'js-beautify-init)
(add-hook 'html-mode-hook 'js-beautify-init)
(add-hook 'css-mode-hook 'js-beautify-init)

;;; cause otherwise this doens't work in graphical
(global-set-key (kbd "<C-return>") 'newline-and-indent)

;;;;; FIX DUMB SEMICOLONS WITH CC-MODE AND USE CLANG-FORMAT FOR EVERYTHING
(defun add-keybinding-to-mode-maps (keys-pressed func-to-call-quoted
                                                 &rest mode-maps-list)
  "Adds function to a given list of mode maps upon pressing a given key."
  (interactive)
  (loop for mode-map in mode-maps-list
        do (define-key mode-map (kbd keys-pressed) func-to-call-quoted)))
(setq c-electric-flag nil)
(add-hook
 'c-initialization-hook '(lambda ()
                           (add-keybinding-to-mode-maps
                            "RET" 'newline-and-indent-fix-cc-mode
                            c-mode-map
                            c++-mode-map
                            java-mode-map)))
(c-set-offset 'innamespace 0)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(add-hook 'js-mode-hook '(lambda ()
                           (add-keybinding-to-mode-maps
                            "C-j" 'javascript-newline-and-indent-ctrl-j-override
                            js-mode-map)))
(add-hook 'js-mode-hook '(lambda ()
                           (add-keybinding-to-mode-maps
                            "<C-return>"
                            'javascript-newline-and-indent-ctrl-j-override
                            js-mode-map)))
(add-hook 'html-mode-hook '(lambda ()
                             (add-keybinding-to-mode-maps
                              "RET" 'html-mode-newline-and-indent-js-beautify
                              html-mode-map)))
(add-hook 'css-mode-hook '(lambda ()
                            (add-keybinding-to-mode-maps
                             "RET" 'css-mode-newline-and-indent-js-beautify
                             css-mode-map)))

;;; syntax highlighting
(global-font-lock-mode 1)               ; turn on syntax highlighting
(setq font-lock-maximum-decoration t)   ; turn it ALL the way on

;;;;; load utilities
;; load w3m web browser
(add-to-list 'load-path "~/.emacs.d/w3m")
(require 'w3m-load)
(setq w3m-use-cookies t)
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;;; add code folding with hs-minor-mode
(add-hook 'prog-mode-hook #'hs-minor-mode) ; add to all programming modes

(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
(require 'multiple-cursors)

;;; for code formatting
(load "~/.emacs.d/lisp/clang-format.el")

;; for like real scrolling
(xterm-mouse-mode)
;; for interacting with clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; add helm stuff
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(helm-mode t)
;; add pcomplete with helm support to eshell
(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map
                [remap eshell-pcomplete]
                'helm-esh-pcomplete)))

;; autoload eshell at start so helm plays nice
(add-hook 'emacs-startup-hook #'(lambda ()
                                  (let ((default-directory (getenv "HOME")))
                                    (command-execute 'eshell)
                                    (bury-buffer))))
(add-to-list 'load-path "~/.emacs.d/helm-swoop")
(require 'helm-swoop)

;; adds appropriate areas to load path
;; in this case for undo-tree, smart-tab, revbufs, others
(add-to-list 'load-path "~/.emacs.d/lisp")
;; allow for save buffer reversion when files are being edited by external tools
(require 'revbufs)

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; adds smart-compile functionality
(require 'smart-compile)

;; adds evil vim layer, but doesn't turn it on
(add-to-list 'load-path "~/.emacs.d/evil/")
(require 'evil)

;; php-mode
(add-to-list 'load-path "~/.emacs.d/php-mode")
(require 'php-mode)
;; go-mode
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode)
;; dired-xattr
(add-to-list 'load-path "~/.emacs.d/dired-xattr")
(require 'dired-xattr)

;; adds undo-tree functionality
(require 'undo-tree)
(global-undo-tree-mode)
;; persist across file saves
(setq undo-tree-auto-save-history t)
;; show diffs in undo tree visualizer
(setq undo-tree-visualizer-diff t)
;;; large undos oh man
(setq undo-outer-limit 50000000)

;; adds smart-tab (tab completion) functionality
(require 'smart-tab)
(global-smart-tab-mode 1)

;; adds dired-x functionality
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))

;; adds julia functionality
(load "~/.emacs.d/ESS/lisp/ess-site")
(setq inferior-julia-program-name "/usr/bin/julia-basic")
(add-to-list 'ess-tracebug-search-path "/usr/share/julia/base/")
(ess-toggle-underscore nil)     ;; underscore means underscore

;; adds haskell functionality
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
                                        ;(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list 'load-path "~/.emacs.d/ghc-4.1.5")
(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;; unfold org at startup
(setq-default org-startup-folded "showeverything")

;;; see docs for funcs n stuff
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;; clojure stuff
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq nrepl-log-messages t)
(global-company-mode)
(add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'subword-mode)

;; same for scratch buffer
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)

;; show line numbers
(global-linum-mode)
(add-hook 'find-file-hook               ; otherwise docview is extremely slow
          '(lambda ()
             (when (or                  ; because regexes are parsed weirdly
                                        ; here and this works
                    (string-match "\.pdf$" buffer-file-name)
                    (string-match "\.ps$" buffer-file-name)
                    (string-match "\.dvi$" buffer-file-name)
                    (string-match "\.doc.*$" buffer-file-name)
                    (string-match "\.ppt.*$" buffer-file-name)
                    (string-match "\.xls.*$" buffer-file-name)
                    (string-match "\.od.*$" buffer-file-name))
               (linum-mode 0))))
(add-hook 'change-major-mode-hook       ; otherwise docview is extremely slow
          '(lambda ()
             (when (eq (with-current-buffer
                           (current-buffer) major-mode) 'doc-view-mode)
               (linum-mode 0))))

;; make them relative

(add-to-list 'load-path "~/.emacs.d/linum-relative")
(require 'linum-relative)
(setq linum-format 'linum-relative)

;; add magit
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/magit")
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "/path/to/magit/")))
(require 'magit)

;; parenthesis matching and more
;; turn pair parens on
(electric-pair-mode t)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}))
;; match parens when cursor on top
(show-paren-mode t)
;;; rainbow delimiters!
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;;; allow for ido usage for better C-x b buffer search n stuff
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)       ; makes
                                        ; searching fuzzier

;; when opening a file the cursor will be at the last saved position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; qmake-mode
(load "~/.emacs.d/lisp/qmake.el")

;;; browsing kill ring, cause why not
(require 'browse-kill-ring)

;;;;; ibuffer stuff
;;;; re: http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
;; you can add different groups too, not just home, in case you ever want to
;; (lol)
(setq ibuffer-saved-filter-groups
      '(("home"
         ("javascript" (mode . js-mode))
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
         ("helm" (or (name . "helm")
                     (name . "Helm")))
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
         ("makefile" (or (filename . "\\Makefile\\'")
                         (filename . "\\makefile\\'")))
         ("readme" (or (filename . "\\README\\'")
                       (filename . "\\readme\\'")))
         ("dired" (mode . dired-mode))
         ("julia" (filename . "\\.jl\\'")) ;; because just detecting julia-mode
         ;; doesn't work fsr
         ("r" (or (filename . "\\.R\\'")
                  (filename . "\\.r\\'")))
         ("cmake" (mode . cmake-mode))
         ("text" (filename . "\\.txt\\'"))
         ("cxx header" (filename . "\\.h\\'"))
         ("c" (mode . c-mode))
         ("c++" (mode . c++-mode))
         ("java" (mode . java-mode))
         ("python" (mode . python-mode))
         ("markdown" (mode . markdown-mode))
         ("emacs-lisp" (mode . emacs-lisp-mode)) ;; emacs-config filter mostly
         ;; blocks this but it's whatever
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
         ("emacs-system" (or (name . "\*eshell\*")
                             (name . "\*scratch\*")
                             (name . "\*Messages\*")
                             (name . "\*Compile-Log\*")
                             (name . "\*ESS\*")
                             (name . "\*compilation\*")
                             (name . "\*Backtrace\*")))
         ("default" (name . "")))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode t) ;; automatically updates buffer list
             (ibuffer-switch-to-saved-filter-groups "home")))

(setq ibuffer-expert t) ;; only prompt when modified buffer is killed
(setq ibuffer-show-empty-filter-groups nil) ;; only show full filter groups

;;; add modes for specific filetypes
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;;; personal color theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(add-to-list 'load-path "~/.emacs.d/color-themes")
(require 'color-theme-danny)
(color-theme-danny)

(defalias 'cider 'cider-jack-in)

;;; start scratch buffer in paredit mode
(with-current-buffer (get-buffer "*scratch*")
  (enable-paredit-mode))

(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)

(setq LaTeX-command-style '(("" "%(PDF)%(latex) -file-line-error %S%(PDFout)")))

;;;;; keybindings
;; FIND CURRENT KEYBINDINGS WITH C-h b !!!!!
;; use C-h k to look at the current key sequence being entered!
;; this is useful when creating new keybindings
;;; C-x C-z to suspend!

;;; reset quit key combination to close
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x C-c C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c C-f") 'delete-frame)

;; TODO: make this work!
;(add-hook 'org-mode-hook
;          (define-key org-mode-map (kbd "<tab>") 'smart-tab))

(global-set-key (kbd "C-c C-s") 'save-point)
(global-set-key (kbd "C-c C-a") 'goto-saved-point)

;; C-Spc to start selection (set mark) in terminal!

;; the below can also be applied over multiple lines with
;; C-u [number] M-x helm-swoop RET
                                        ; find regexp in file, more
(global-set-key (kbd "C-x o") 'helm-swoop)         ; interactively than above
(global-set-key (kbd "C-x f") 'helm-multi-swoop-all) ; find regexp in ALL open
                                        ; buffers
(global-set-key (kbd "C-x j") 'helm-multi-swoop)                 ; find regexp
                                        ; in SOME open buffers
(global-set-key (kbd "C-x b") 'helm-buffers-list) ; find among open buffers

;; after killing C-x o with helm,
;; let's make sure we do have buffer switching in the event of non-graphical
;; terminal-only editing
(global-set-key (kbd "C-x /") 'other-window)

;;; split-window management
;; open and close
(global-set-key (kbd "C-x <down>") 'split-window-below)
(global-set-key (kbd "C-x <right>") 'split-window-right)
(global-set-key (kbd "C-x <left>") 'split-window-horizontally)
(global-set-key (kbd "C-x <up>") 'split-window-vertically)
(global-set-key (kbd "C-x C-<down>") 'split-window-below)
(global-set-key (kbd "C-x C-<right>") 'split-window-right)
(global-set-key (kbd "C-x C-<left>") 'split-window-horizontally)
(global-set-key (kbd "C-x C-<up>") 'split-window-vertically)
(global-set-key (kbd "C-x e") 'delete-other-windows) ;; "expand"
(global-set-key (kbd "C-x p") 'delete-window) ;; "poof"
;; adjusting pane size
(global-set-key (kbd "C-x <home>") 'enlarge-window) ;; increase window height
(global-set-key (kbd "C-x <end>") 'shrink-window) ;; decrease window height
(global-set-key (kbd "C-x <prior>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <next>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x RET") 'shrink-window-if-larger-than-buffer) ;; shrink
;; window to fit content
(global-set-key (kbd "C-x !") 'balance-windows) ;; make all windows same height
;;; move among panes in a way that isn't totally fucked
(setq windmove-wrap-around t)
;; original meta
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)

;; visualize undo-tree
(global-set-key (kbd "C-x t") 'undo-tree-visualize)

;; use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill current buffer and close pane
(global-set-key (kbd "C-x C-k") 'close-and-kill-this-pane)

;; smart-compile stuff!!!!
(global-set-key (kbd "C-c C-k") 'smart-compile)

;; translate stuff into hex (and back??)
(global-set-key (kbd "C-x h") 'hexl-mode)

;; delete all of everything
;; (global-set-key (kbd "C-x d") 'erase-buffer)

;; find file somewhere below given directory with dired
(global-set-key (kbd "C-x C-r") 'find-name-dired) ; "recursive"

;; make C-z undo instead of FUCKING UP MY ENTIRE LIFE by suspending
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)

;; kill all active dired buffers at once
(global-set-key (kbd "C-x M-d") 'kill-dired-buffers)

;; open new file with given filename from minibuffer, or blank filename (cross
;; your fingers)
(global-set-key (kbd "C-x C-n") 'open-new-file)

;; go to normal mode; i.e. quickly format everything pretty
(global-set-key (kbd "C-x n") 'normal-mode)

;; bind the key that doesn't do dired but should go to dired
(global-set-key (kbd "C-x C-d") 'dired)

;; multiple cursors fun!!!
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-n") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-x C-a") 'mc/mark-all-like-this)

;; gofmt!!!
(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'go-fmt-file)))

;; remember that M-= gets word counts!

;; search all open buffers for regexp
(global-set-key (kbd "C-c M-r") 'search-all-buffers)

;; make <backtab> force tab
(global-set-key (kbd "<backtab>") 'force-insert-tab)
(global-set-key (kbd "C-c t") 'force-insert-tab) ; for modes like markdown-mode
                                        ; where S-tab overridden
(define-key c-mode-map (kbd "C-j") 'newline-and-indent-ctrl-j)
(define-key c-mode-map (kbd "<C-return>") 'newline-and-indent-ctrl-j)
(define-key c++-mode-map (kbd "C-j") 'newline-and-indent-ctrl-j)
(define-key c++-mode-map (kbd "<C-return>") 'newline-and-indent-ctrl-j)

;; open file with wildcards
;;(global-set-key (kbd "C-c o") 'open-file-with-wildcards)
;; doesn't work lol

;; toggle letter casing from ALLCAPS to InitialCase to alllowercase
(global-set-key (kbd "C-x M-c") 'toggle-letter-case)

;;; recognize camel case
(global-set-key (kbd "C-<right>") 'camel-case-right-word)
(global-set-key (kbd "C-<left>") 'camel-case-left-word)

;;; shortcut for magit, finally
(global-set-key (kbd "C-x g") 'magit-status)

;;; just destroy unused files
(global-set-key (kbd "C-x d") 'kill-buffer-and-move-file-to-trash)

;;; opposite of yank-pop
(global-set-key (kbd "C-M-y") 'yank-push)

;;;;; my own functions! used throughout this file
;;; some of these are mine, some are heavily adapated from emacswiki, some are
;;; copy/paste from emacswiki
;;; if you wrote something and want me to put your name by it (which would be
;;; hilarious cause that means someone else is actually using this file) email
;;; me and i'll add accreditation immediately

;; allow for backtab to force '\t'
(defun force-insert-tab ()
  "insert tab character"
  (interactive)
  (insert "\t"))

;; from http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name))
                 (not
                  (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defcustom search-all-buffers-ignored-files
  (list (rx-to-string
         '(and
           bos
           (or
            ".bash_history"
            "TAGS")
           eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun newline-continue-comment ()
  (interactive)
  (if (point-in-comment)
      (comment-indent-new-line)
    (newline-and-indent)))

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.      With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
                     current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory
                                                        (buffer-file-name b))))
                        search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))
   regexp))

;; kill current buffer and close pane
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it
also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;; kill all active dired buffers at once
(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;; open new file (i wrote this!!!)
(defun open-new-file ()
  (interactive)
  (switch-to-buffer ;; switches to operating buffer
   (generate-new-buffer ;; creates new buffer with given name
    (generate-new-buffer-name
     (read-string "new filename: " ;; reads from minibuffer, with given default
                  ;; value
                  nil nil "*newbuf*")))) ;; with default title *newbuf*
  (normal-mode))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; stolen from the ergo emacs guy
;; (http://ergoemacs.org/emacs/modernization_upcase-word.html)
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq p1 (car bds) p2 (cdr bds))))
    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all
lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all
caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init
caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))
    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

;;; get and update the current number of lines within the buffer
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)
(defvar integer-buffer-line-count nil)
(make-variable-buffer-local 'integer-buffer-line-count)

(setq-default mode-line-format
              '("  " mode-line-modified
                (list 'line-number-mode "  ")
                (:eval (when line-number-mode
                         (let ((str "L%l"))
                           (when (and (not (buffer-modified-p))
                                      my-mode-line-buffer-line-count)
                             (setq str (concat str "/"
                                               my-mode-line-buffer-line-count)))
                           str)))
                "  %p"
                (list 'column-number-mode "      C%c")
                "  " mode-line-buffer-identification
                "  " mode-line-modes))

;;; not sure why this is here, but ok
(defun output-lines-in-buffer ()
  (setq integer-buffer-line-count (count-lines (point-min) (point-max)))
  (setq my-mode-line-buffer-line-count (int-to-string
                                        integer-buffer-line-count)))


(add-hook 'find-file-hook 'output-lines-in-buffer)
(add-hook 'after-save-hook 'output-lines-in-buffer)
(add-hook 'after-revert-hook 'output-lines-in-buffer)
(add-hook 'dired-after-readin-hook 'output-lines-in-buffer)

;; function to indent whole buffer
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun newline-and-indent-fix-cc-mode ()
  "cc-mode's indentation procedures upon adding a new bracket or paren are
annoying. This fixes that."
  (interactive)
  (insert-char 97)
  (insert-char 59)
  (clang-format-line)                   ; clang-formats previous line
  (delete-backward-char 2))

(defun count-num-lines-in-buffer ()
  (count-lines (point-min) (point-max)))

(defun get-lines-in-buffer-str ()
  (interactive)
  (int-to-string integer-buffer-line-count))

(defun get-current-line-as-string ()
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun move-point-to-beginning-of-line ()
  (interactive)
  (goto-char (line-beginning-position)))

(defun move-point-to-end-of-line ()
  (interactive)
  (goto-char (line-end-position)))

(defun newline-and-indent-ctrl-j ()
  (interactive)
  (let ((chars-from-end-of-line (- (line-end-position) (point))))
    (newline-and-indent)
    (insert-char 97)                    ; insert a
    (insert-char 59)                    ; insert semicolon
    (move-point-to-beginning-of-line)
    (c-indent-command)
    (move-point-to-end-of-line)
    (backward-char chars-from-end-of-line)
    (if (/= chars-from-end-of-line 0)
        (progn
          (newline-and-indent)
          (move-point-to-beginning-of-line)
          (backward-char)))
    ;; removes a and semicolon
    (delete-backward-char 2)))

(defun string-is-capitalized-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]*\\'" str)))

(defun char-is-capitalized-p (char)
  (let ((case-fold-search nil)
        (str-from-char (string char)))
    (string-match-p "\\`[A-Z]*\\'" str-from-char)))

(defun camel-case-right-word ()
  (interactive "^")                     ; highlights region if shifted
  (let ((cur-point (point))
        fin-point
        cap-letter-index)
    (right-word)
    (setq fin-point (point)
          cap-letter-index (- fin-point cur-point))
    ;; check if string is all caps; if so, skip
    (if (not (string-is-capitalized-p (buffer-substring cur-point fin-point)))
        (progn
          (loop for letter-index from 1 upto (- fin-point cur-point)
                while (= cap-letter-index (- fin-point cur-point))
                do (if (char-is-capitalized-p
                        (char-after (+ cur-point letter-index)))
                       (setq cap-letter-index letter-index)))
          (goto-char (+ cur-point cap-letter-index))))))

(defun camel-case-left-word ()
  (interactive "^")                     ; highlights region if shifted
  (let ((cur-point (point))
        fin-point
        cap-letter-index)
    (right-word -1)              ; used because left-word is screwy, same effect
    (setq fin-point (point)
          cap-letter-index (- fin-point cur-point))
    ;; check if string is all caps; if so, skip
    (if (not (string-is-capitalized-p (buffer-substring cur-point fin-point)))
        (progn
          (loop for letter-index from -1 downto (- fin-point cur-point)
                while (= cap-letter-index (- fin-point cur-point))
                do (if (char-is-capitalized-p
                        (char-after (+ cur-point letter-index)))
                       (setq cap-letter-index letter-index)))
          (goto-char (+ cur-point cap-letter-index))))))

(defun kill-selected-region-default (&optional lines)
  "When selection highlighted, C-k stores all characters in the kill ring,
instead of just the final line."
  (interactive "p")     ; gets beg and end from emacs as args
  (message (number-to-string lines))
  (if (use-region-p)    ; if region selected
      (kill-region (region-beginning) (region-end))
    (if (= lines 1)
        (kill-line)
      (kill-line lines))))
(global-set-key (kbd "C-k") 'kill-selected-region-default)

(add-hook 'slime-mode-hook 'fix-lisp-keybindings)
;; get useful keybindings for lisp editing
(defun fix-lisp-keybindings ()
  "Changes about three million personalized keybindings for lisp editing with
SLIME and Paredit. Not for the faint of heart."
  (interactive)
  (define-key paredit-mode-map (kbd "C-M-<left>") 'windmove-left)
  (define-key paredit-mode-map (kbd "C-M-<right>") 'windmove-right)
  (define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward) ; remove key
                                        ; here (slurp-forward)
  (define-key paredit-mode-map (kbd "C-c <right>") 'paredit-forward)
  (define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward) ; remove key
                                        ; here (slurp-backward)
  (define-key paredit-mode-map (kbd "C-c <left>") 'paredit-backward)
  (define-key paredit-mode-map (kbd "M-a") nil) ; kill this, it's a global but
                                        ; it's annoying and i don't use it
  (define-key paredit-mode-map (kbd "M-a M-a") 'paredit-add-parens-in-front)
  (define-key paredit-mode-map (kbd "M-a M-s") 'paredit-remove-function-wrapper)
  (global-set-key (kbd "RET") 'newline-and-indent) ; set as global because
                                        ; define-key doesn't work, not sure why
  (define-key paredit-mode-map (kbd "M-a M-<right>")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-c <up>")
    'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-a M-<left>")
    'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-a C-M-<right>")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-c <down>")
    'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-a C-M-<left>")
    'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-x C-l") 'mc/edit-lines) ; so that
                                        ; multiple-cursors can use these
  (define-key paredit-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key paredit-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-M-n") 'mc/unmark-next-like-this)
  (define-key paredit-mode-map (kbd "C-M-p") 'mc/unmark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key paredit-mode-map (kbd "C-M-y") 'paredit-yank-push)
  (define-key paredit-mode-map (kbd "DEL") 'paredit-backspace-delete-highlight))

;; create parens and add adjacent two elements to sexp created by parens
(defun paredit-add-parens-in-front ()
  ;; add to this later; slurp all sexps until closing paren would be very
  ;; helpful i think
  (interactive)
  (let ((sel-beg nil) (sel-end nil))
    (if (use-region-p)
        (setq sel-beg (region-beginning) sel-end (region-end))
      (progn
        (paredit-forward)
        (setq sel-end (point))
        (paredit-backward)
        (setq sel-beg (point))))
    (message (number-to-string sel-beg))
    (goto-char sel-beg)
    (paredit-open-parenthesis) ; adds closing paren too thanks to electric pair
                                        ; (or maybe it's paredit itself)
                                        ;       (paredit-forward-slurp-sexp)
    ))
(defun paredit-backspace-delete-highlight ()
  "Makes it so that backspace deletes all highlighted text in paredit mode.
Breaks the rules a little bit, but makes me a lot less insane."
  (interactive)
  (if (use-region-p)                    ; if a region is selected
      (delete-region (region-beginning) (region-end))
    (paredit-backward-delete)))
(defun paredit-remove-function-wrapper ()
  ;; this one is very imperative, not so lispy
  ;; it's really useful though so hopefully history will forgive me
  "Removes all arguments to the left of point within sexp, and removes enclosing
parentheses. CURRENTLY BROKEN"
  (interactive)
  (let ((sel-beg nil) (sel-end nil)) ; set beginning and end of selection
    (if (use-region-p)
        (setq sel-beg (region-beginning) sel-end (region-end))
      (progn
        (paredit-forward)
        (setq sel-end (point))
        (paredit-backward)
        (setq sel-beg (point))))
    (goto-char sel-beg)
    (let ((paren-counter 0))
      (loop until (and (char-equal (preceding-char) 40) ; 40 is open parenthesis
                       (= paren-counter 0))
            do (progn
                 (if (char-equal (preceding-char) 41) ; 41 is closed parenthesis
                     (incf paren-counter))
                 (if (char-equal (preceding-char) 40)   ; open paren
                     (decf paren-counter))
                 (paredit-backward-delete)
                 (decf sel-end))))
    (goto-char sel-end)
    (let ((paren-counter 0))
      (loop until (and (char-equal (following-char) 41) ; closed paren
                       (= paren-counter 0))
            do (progn
                 (if (char-equal (following-char) 40)   ; open paren
                     (incf paren-counter))
                 (if (char-equal (following-char) 41)   ; closed paren
                     (decf paren-counter))
                 (paredit-forward-delete))))
    (paredit-splice-sexp)))

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-compile))
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))
;;; cause i can never figure out how to just get to the REPL lol
(defalias 'haskell-repl (symbol-function 'haskell-process-do-info))

(defun replace-char-in-range (from-char to-char beg end)
  (let ((was-char-replaced nil))
    (loop for index from beg upto end
          do (when (< index (point-max))
               (if
                   (char-equal (char-after index) from-char)
                   (save-excursion
                     (setq was-char-replaced t)
                     (goto-char index)
                     (delete-char 1)
                     (insert-char to-char)))))
    was-char-replaced))

(defun remove-newlines-from-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (loop for line-index from 1 upto (count-num-lines-in-buffer)
          do (progn
               (if (string-equal (get-current-line-as-string) "")
                   (delete-forward-char 1)
                 (forward-line 1))))))

(defun kill-buffer-and-move-file-to-trash ()
  "Doesn't delete the file, just moves it to /tmp so that it goes away."
  (interactive)
  (set-buffer-modified-p nil)           ; pretends buffer not modified
  (message
   (shell-command-to-string
    (concat "mv " (buffer-file-name) " /tmp/")))
  (kill-this-buffer))

(defun yank-push ()
  (interactive)
  (loop for i from 1 to 2               ; twice
        do (if (= (length kill-ring-yank-pointer) 0)
               nil
             (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))))
  (yank-pop))

(defun paredit-yank-push ()
  (interactive)
  (loop for i from 1 to 2               ; twice
        do (if (= (length kill-ring-yank-pointer) 0)
               nil
             (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))))
  (paredit-yank-pop))

(let ((saved-point 1))
  (defun save-point ()
    "Saves point in a single-value register. Use with goto-saved-point. Defaults
 to position 1."
    (interactive)
    (setq saved-point (point))
    (message (concat "Point saved (" (number-to-string saved-point) ")")))
  (defun goto-saved-point ()
    "Moves point to a point previously saved with save-point. By default, moves
 to position 1."
    (interactive)
    (goto-char saved-point)
    (recenter)
    (message
     (concat "Moved to saved point (" (number-to-string saved-point) ")"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(asm-comment-char 35)
 '(color-theme-directory ("/home/cosmicexplorer/.emacs.d/color-themes/"))
 '(fill-column 80)
 '(number-of-extra-newlines-to-preserve-js-mode 0)
 '(org-support-shift-select (quote always))
 '(org-support-shift-select (quote always))
 '(server-delete-tty t)
 '(yank-pop-change-selection t))
(put 'erase-buffer 'disabled nil)


;; TODO: integrate schmidt's emacs into this one

;; (global-set-key "e["      'backward-sexp)
;; (global-set-key "b"       'delete-backward-char)
;; (global-set-key "eb"     'backward-kill-word)
;; (global-set-key "e;"      'backward-to-word)
;; (global-set-key "e,"      'beginning-of-window)
;; (global-set-key "er"      'call-last-kbd-macro)
;; (global-set-key "ej"      'carriage-return-newline)
;; (global-set-key "C-x,"    'compile)
;; (global-set-key "C-x/"    'point-to-register)
;; (global-set-key "C-xj"    'register-to-point)
;; (global-set-key "n"       'cr-indent-relative)
;; (global-set-key "C-xC-b" 'electric-buffer-list)
;; (global-set-key "C-xC-h" 'electric-command-history)
;; (global-set-key "e."      'end-of-window)
;; (global-set-key "e?"      'find-tag)
;; (global-set-key "e]"      'forward-sexp)
;; (global-set-key "e'"      'forward-to-word)
;; (global-set-key "eg"      'goto-line)
;; (global-set-key "eh"      'help-command)
;; (global-set-key "ei"      'indent-point-relative)
;; (global-set-key "eC-b"   'insert-buffer)
;; (global-set-key "C-r"     'isearch-backward-regexp)
;; (global-set-key "C-s"     'isearch-forward-regexp)
;; (global-set-key "e$"      'ispell-word)
;; (global-set-key "ee"     'keyboard-quit)
;; (global-set-key "e^"      'kill-eol-rejoin-lines)
;; (global-set-key "eC-d"   'kill-to-eof)
;; (global-set-key "eC-t"   'kill-to-filestart)
;; (global-set-key "eC-u"   'line-to-top-of-screen)
;; (global-set-key "C-xl"   'load-file)
;; (global-set-key "C-xr"    'mh-rmail)
;; (global-set-key "C-xm"    'mh-smail)
;; (global-set-key "C-e"     'move-to-eoln)
;; (global-set-key "r"       'newline-indent)
;; (global-set-key "C-xb"    'previous-buffer)
;; (global-set-key "C-xa"    'mml-attach-file)
;; (global-set-key "C-xn"    'next-buffer)
;; (global-set-key "eC-q"   'quoted-insert)
;; (global-set-key "C-xf"    'save-all-buffers-then-exit)
;; (global-set-key "en"      'scroll-down-one)
;; (global-set-key "C-x}"    'scroll-left)
;; (global-set-key "C-x{"    'scroll-right)
;; (global-set-key "ep"      'scroll-up-one)
;; (global-set-key "C-q"     'static-cursor-insert-space)
;; (global-set-key "C-xv"    'switch-to-buffer-other-window)
;; (global-set-key "C-xc"    'switch-to-buffer)
;; (global-set-key "C-xt"    'tags-search)
;; (global-set-key "eC-a"   'toggle-auto-fill-mode)
;; (global-set-key "eC-c"   'toggle-c-mode)
;; (global-set-key "eo"      'toggle-overlay-mode)
;; (global-set-key "ez"      'tags-loop-continue)
;; (global-set-key "C-x="    'what-line)
;; (global-set-key "e="      'what-cursor-position)
;; (global-set-key "C-xC-f" 'wild-find-file)
;; (global-set-key "C-xC-v" 'wild-find-file-other-window)
;; (global-set-key "C-xC-y" 'strip-and-save)
;; 
;; (setq line-number-mode t)
;; (setq enable-local-variables t)
;; (setq auto-fill-default t) ;; make new buffers be in auto fill mode by default
;; (setq auto-save-interval 600)     ; half as often as default
;; (setq-default indent-tabs-mode nil)
;; (setq gc-cons-threshold 2000000)
;; (setq lpr-switches '("-Pps"))
;; (setq text-mode-hook 'turn-on-auto-fill)
;; (setq default-truncate-lines nil)
;; (setq visible-bell t)
;; (setq require-final-newline t)
;; (setq version-control t)
;; (setq abbrev-all-caps t)
;; (put 'eval-expression 'disabled t)
;; (setq trim-versions-without-asking t)
;; (setq mode-line-inverse-video t)
;; (setq-default nuke-trailing-whitespace-p t)
;; (setq default-tab-width 8)
;; (setq explicit-shell-file-name "/pkg/local/bin/tcsh");
;; 
;; (autoload 'html-helper-mode "html-helper-mode" "HTML rules!" t)
;; (autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
;; (autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
;; (autoload 'ispell-region "ispell" "Check spelling of region" t)
;; (autoload 'ispell-buffer "ispell" "Check spelling of buffer" t)
;; (autoload 'webster "webster" "look up a word in Webster's 7th edition" t)
;; 
;; (fmakunbound 'c-mode)
;; (makunbound 'c-mode-map)
;; (fmakunbound 'c++-mode)
;; (makunbound 'c++-mode-map)
;; 
;; (setq load-path (cons (expand-file-name "~/lib/lisp") load-path))
;; (autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
;; (autoload 'c-mode   "cc-mode" "C Editing Mode" t)
;; (setq auto-mode-alist
;; (append '(("\.[Chily]$" . c++-mode)
;; ("\.html$" . html-helper-mode)
;; ("\.cc$" . c++-mode)
;; ("\.cpp$" . c++-mode)
;; ("\.inl$" . c++-mode)
;; ("\.idl$" . c++-mode)
;; ("\.c$" . c-mode)   ; to edit C code
;; )auto-mode-alist))
;; (setq auto-mode-alist (cons '("\.Z$" . compress-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\.gz$" . zip-mode) auto-mode-alist))
;; ;;(setq auto-mode-alist (cons '("\.[cly]$" . load-c-mode) auto-mode-alist))
;; 
;; ;(setq mh-progs "/home/cs/faculty/schmidt/bin/SunOS5"
;; ;      mh-lib "/home/cs/faculty/schmidt/lib/SunOS5"
;; ;(setq mh-progs "/pkg/mh/bin"
;; ;      mh-lib "/pkg/mh/lib"
;; ;      mh-user-path "~/personal/mhbox/"
;; ;      mh-draft-folder "drafter"
;; ;      mh-use-mhl t
;; ;      mhl-formfile t
;; ;      mail-default-reply-to "schmidt@cse.wustl.edu")
;; ;(fset 'rmail 'mh-rmail)
;; ;(fset 'smail 'mh-smail)
;; 
;; ;; Load the sendmail.el library which contains definitions of mail-text
;; ;;
;; (require 'sendmail)
;; 
;; ;; Utility function to create a From field
;; ;;
;; (defun mail-from ()
;; "Move point to end of From-field.  Create a From field if none."
;; (interactive)
;; (expand-abbrev)
;; (or (mail-position-on-field "From" t)
;; (progn
;; (goto-char (point-min))
;; (insert "From: " user-full-name " <"user-mail-address">n")
;; (mail-text))))
;; 
;; ;; Add mail-from so that it gets called whenever a message is composed
;; ;;
;; (add-hook 'mh-letter-mode-hook 'mail-from)
;; (require 'mh-e)
;; (define-key mh-folder-mode-map "C-d" (lambda(range)
;; (interactive
;; (list (mh-interactive-range "Delete")))
;; (mh-pipe-msg "sa-learn --spam " t)
;; (mh-iterate-on-range () range
;; (mh-delete-a-msg nil))
;; (if (looking-at
;; mh-scan-deleted-msg-regexp)
;; (mh-next-msg))))
;; ;; MH-E setup
;; 
;; (defun compress-mode ()
;; "Deal with .Z compressed files"
;; (interactive)
;; (shell-command-on-region (point-min) (point-max) "compress -d" t)
;; (goto-char 1)
;; (not-modified)
;; )
;; 
;; (defun zip-mode ()
;; "Deal with .gz compressed files"
;; (interactive)
;; (shell-command-on-region (point-min) (point-max) "gzip -d" t)
;; (goto-char 1)
;; (not-modified)
;; )
;; 
;; (defun scroll-down-one (lines) "scrolls the screen down one line, point remains
;; in same location"
;; (interactive "p")
;; (scroll-down lines))
;; 
;; (defun scroll-up-one (lines) "scrolls the screen up one line, point remains
;; in same location"
;; (interactive "p")
;; (scroll-up lines))
;; 
;; (defun print-current-buffer () "prints current buffer to the Imagen printer"
;; (interactive)
;; (shell-command-on-region (point-min) (point-max)
;; (concat "lpr -Plp " (buffer-name)) nil)
;; (message (concat "File " (buffer-name) " is currently imprinting , sire!")))
;; 
;; (defun toggle-overlay-mode () "turns on overwrite mode, turns off abbrevs and
;; vice-versa"
;; (interactive)
;; (cond
;; ((eq overwrite-mode t)
;; (overwrite-mode (- 1))
;; (abbrev-mode (+ 1))
;; (line-status))
;; ((eq overwrite-mode nil)
;; (overwrite-mode (+ 1))
;; (abbrev-mode (- 1))
;; (line-status))))
;; 
;; ;(defun proc-up-shell () "starts me up with the C-shell running in GNU"
;; ;   (get-buffer-create (concat "*" "shell" "*"))
;; ;   (shell))
;; 
;; ;(defun switch-to-shell-buffer () "moves from current buffer to shell buffer"
;; ;   (interactive)
;; ;   (switch-to-buffer "*shell*"))
;; 
;; (defun static-cursor-insert-space () "inserts a space, but does not move cursor"
;; (interactive)
;; (insert-string " ")
;; (forward-char (- 1)))
;; 
;; (defun kill-eol-rejoin-lines () "Kills end of line and then appends
;; previous line to end of current line"
;; (interactive)
;; (kill-line)
;; (just-one-space))
;; 
;; (defun load-display-time () "load the display time upon invocation!"
;; (setq display-time-day-and-date t)
;; (display-time))
;; 
;; (defun save-modified-buffers () "saves all modified file buffers w/o query"
;; (interactive)
;; (save-some-buffers t))
;; 
;; (defun save-all-buffers-then-exit () "saves buffers without question and exits"
;; (interactive)
;; (save-modified-buffers)
;; (kill-emacs))
;; 
;; (defun line-to-top-of-screen () "moves line to top of screen"
;; (interactive)
;; (recenter 0))
;; 
;; (defun open-line (arg)
;; "Insert a newline and leave point before it.
;; With arg, inserts that many newlines."
;; (interactive "*p")
;; (let ((flag (and (bolp) (not (bobp)))))
;; (if flag (forward-char -1))
;; (while (> arg 0)
;; (save-excursion
;; (insert ?n)
;; (if fill-prefix (insert fill-prefix)))
;; (setq arg (1- arg)))
;; (if flag (forward-char 1))))
;; 
;; (defun move-to-eoln () "Moves to end of text on line"
;; (interactive)
;; (end-of-line)
;; (delete-horizontal-space))
;; 
;; 
;; (defun kill-backwards-line () "Kills the entire line backwards"
;; (interactive)
;; (beginning-of-line)
;; (kill-line))
;; 
;; (defun newline-indent ()
;; (interactive)
;; (next-line (+ 1))
;; (back-to-indentation))
;; 
;; (defun carriage-return-newline () "performs the function formerly reserved
;; for newline"
;; (interactive)
;; (insert "n"))
;; 
;; (defun cr-indent-relative () "Indents at beginning column location of
;; previous line"
;; (interactive)
;; (point-to-register 106)
;; (back-to-indentation)
;; (setq opoint (current-column))
;; (register-to-point 106)
;; (insert "n")
;; (indent-to opoint))
;; 
;; (defun indent-point-relative () "Indents directly under beginning of previous
;; line"
;; (interactive)
;; (delete-horizontal-space)
;; (indent-relative))
;; 
;; (defun back-space-tab () "Provides a nice back-tabbing capability, does NOT
;; alter text on line."
;; (interactive)
;; (forward-char (- 3)))
;; 
;; (defun forward-tab () "Tabs forward three spaces without moving text, only
;; point"
;; (interactive)
;; (forward-char (+ 3)))
;; 
;; (defun mark-beginning-of-buffer ()
;; "Set mark at the beginning of the buffer."
;; (interactive)
;; (push-mark (point-min)))
;; 
;; (defun mark-end-of-buffer ()
;; "Set mark at the end of the buffer."
;; (interactive)
;; (push-mark (point-max)))
;; 
;; (defun kill-to-eof () "Kills buffer to the end of file"
;; (interactive)
;; (mark-end-of-buffer)
;; (kill-region (mark) (point)))
;; 
;; (defun kill-to-filestart () "Kills buffer to the start of file"
;; (interactive)
;; (mark-beginning-of-buffer)
;; (kill-region (mark) (point)))
;; 
;; (defun beginning-of-window () "Put dot at the beginning of this window"
;; (interactive)
;; (goto-char (window-start)))
;; 
;; (defun end-of-window () "Put dot at the end of this window"
;; (interactive)
;; (goto-char (window-start))
;; (vertical-motion (- (window-height) 2))
;; (end-of-line))
;; 
;; (defun line-status () "prints neat info about current line"
;; (save-excursion (set-buffer (other-buffer))))
;; 
;; (defun toggle-abbrev-mode () "Turns ON abbrevs if currently OFF and vice versa"
;; (interactive)
;; (cond
;; ((eq abbrev-mode t)
;; (abbrev-mode (- 1))
;; (line-status))
;; ((eq abbrev-mode nil)
;; (abbrev-mode (+ 1))
;; (line-status))))
;; 
;; (defun toggle-auto-fill-mode () "Turns ON auto-fill if currently OFF and vice versa"
;; (interactive)
;; (auto-fill-mode nil)
;; (line-status))
;; 
;; (defun toggle-fundamental-mode () "Toggle the fundamental mode"
;; (interactive)
;; (fundamental-mode)
;; (line-status))
;; 
;; ; stuff I only use with X (hilit19 mode and colored paren mode)
;; (cond
;; ((eq window-system 'x)
;; (defvar hilit-background-mode 'light
;; "'mono inhibits color, 'dark or 'light is the background brightness")
;; (load-library "hilit19")
;; (load-library "paren"))
;; )
;; 
;; ;(defun toggle-c-mode () "Toggle my C mode"
;; ;  (interactive)
;; ;  (load-file "~/.c-mode.el")
;; ;  (c-mode)
;; ;  (line-status))
;; 
;; (defun forward-to-word (arg)
;; "Move forward until encountering the beginning of a word.
;; With argument, do this that many times."
;; (interactive "p")
;; (or (re-search-forward (if (> arg 0) "\W" "\W") nil t arg)
;; (goto-char (if (> arg 0) (point-max) (point-min)))))
;; 
;; (defun skip-forward-identifier () "moves point to end of identifier"
;; (interactive)
;; (forward-word (+ 1))
;; (skip-chars-forward "._a-zA-Z0-9-"))
;; 
;; (defun skip-backward-identifier () "moves point to end of identifier"
;; (interactive)
;; (forward-word (- 1))
;; (skip-chars-backward "._a-zA-Z0-9-"))
;; 
;; 
;; (defun backward-to-word (arg)
;; "Move backward until encountering the end of a word.
;; With argument, do this that many times."
;; (interactive "p")
;; (forward-to-word (- arg)))
;; 
;; 
;; ;(defun load-c-mode () "load my own C mode!"
;; ;  (load-file "~/.c-mode.el")
;; ;  (c-mode)
;; ;)
;; 
;; ;----------------------------------------------------------------------
;; 
;; ;(defun load-ada-mode () "load the ada mode abbrevs"
;; ;   (load-file "~/.ada.el")
;; ;   (ada-mode)
;; ;  (setq comment-start "--( ")
;; ;  (setq comment-end " )")
;; ;  (setq comment-column 45)
;; ;  (setq comment-start-skip "--( ")
;; ;   (setq abbrev-file-name "~/.ada_defs")
;; ;   (quietly-read-abbrev-file "~/.ada_defs")
;; ;   (abbrev-mode 1))
;; 
;; ;(defun load-pascal-mode () "load the pascal mode abbrevs 'n' stuff"
;; ;   (load-file "~/.pascal.el")
;; ;   (pascal-mode)
;; ;   (setq abbrev-file-name "~/.pascal_defs")
;; ;   (quietly-read-abbrev-file "~/.pascal_defs")
;; ;   (abbrev-mode 1))
;; 
;; ;(defun mousetags ()
;; ;  (interactive)
;; ;  (load-file "~/.mouse-tags.el")
;; ;  (visit-tags-table "./TAGS")
;; ;  (message "Mouse tags loaded")) ;; type c-x c-e before the ;; here
;; 
;; ;----------------------------------------
;; ; Crazy stuff that Owen gave me!
;; ;----------------------------------------
;; 
;; (defun buffer-acceptable (buffer)
;; ; Decide whether a buffer is acceptable
;; (not (null (buffer-file-name buffer))))
;; 
;; (defun select-ok-buffer (buff-list)
;; ; Select the first acceptable buffer from a list
;; (if (null buff-list)
;; nil
;; (if (buffer-acceptable (car buff-list))
;; (car buff-list)
;; (select-ok-buffer (cdr buff-list)))))
;; 
;; (defun next-buffer ()
;; ; Function to switch to the next buffer in the buffer list
;; (interactive)
;; (let ((new-buf (select-ok-buffer (reverse (buffer-list)))))
;; (if new-buf
;; (switch-to-buffer new-buf))))
;; 
;; (defun previous-buffer ()
;; ; Function to switch to the previous buffer in the buffer list
;; (interactive)
;; (let ((new-buf (select-ok-buffer (cdr (buffer-list)))))
;; (if new-buf
;; (progn
;; (bury-buffer (current-buffer))
;; (switch-to-buffer new-buf)))))
;; 
;; (defun wild-to-regexp (wild-pattern)
;; ; Convert a wildcard pattern with * to a regexp
;; (let ((result (regexp-quote wild-pattern))
;; (next-posn nil))
;; 
;; ; Convert each "*" to ".*"
;; (while (setq next-posn (string-match "\\*" result))
;; (setq result (concat (substring result 0 next-posn)
;; ".*"
;; (substring result (+ next-posn 2)
;; (length result)))))
;; 
;; ; Make sure we only match full expressions
;; (concat "^" result "$")))
;; 
;; (defun wild-find-file (filename)
;; ; Read a set of files from a wildcard specification
;; (interactive "FWildcard find file: ")
;; (let* ((dir       (file-name-directory filename))
;; (pattern   (wild-to-regexp (file-name-nondirectory filename)))
;; (file-list (directory-files dir t pattern)))
;; (if (null file-list)
;; (find-file filename)
;; (mapcar 'find-file file-list))))
;; 
;; (defun wild-find-file-other-window (filename)
;; ; Read a set of files from a wildcard specification
;; (interactive "FWildcard find file in other window: ")
;; (let* ((dir       (file-name-directory filename))
;; (pattern   (wild-to-regexp (file-name-nondirectory filename)))
;; (file-list (directory-files dir t pattern)))
;; (if (null file-list)
;; (find-file-other-window filename)
;; (mapcar 'find-file-other-window file-list))))
;; 
;; ;;;;;;;;
;; ;;;;;;;; Save a file, after untabify'ing and stripping trailing spaces.
;; ;;;;;;;;
;; (defun strip-and-save ()
;; "untabify, strip trailing spaces, and save file"
;; (interactive)
;; (untabify (point-min) (point-max))
;; (goto-char (point-min))
;; (while (re-search-forward " +$" nil t)
;; (replace-match "" nil nil))
;; (goto-char (point-min))
;; (save-buffer))
;; 
;; ; ChangeLog mode
;; (add-hook 'change-log-mode-hook
;; (function (lambda ()
;; (setq fill-prefix "  "))))
;; ;                    two spaces here: -> ^^
;; (add-hook 'change-log-mode-hook 'auto-fill-mode)
;; 
;; ;---------------------------------------------------------------------------
;; ; Things to do everytime we start up!
;; ;---------------------------------------------------------------------------
;; 
;; (load-display-time)
;; 
;; (put 'downcase-region 'disabled nil)
;; (custom-set-variables
;; ;; custom-set-variables was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(safe-local-variable-values (quote ((TeX-master . "proposal") (add-log-time-format lambda nil (progn (setq tz (getenv "TZ")) (set-time-zone-rule "UTC") (setq time (format-time-string "%a %b %e %H:%M:%S %Z %Y" (current-time))) (set-time-zone-rule tz) time))))))
;; (custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
