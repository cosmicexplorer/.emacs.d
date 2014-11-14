;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014

;;; i've changed quite a few files besides just this one and if you wish to
;;; upgrade the associated external packages
;;; you'll have to re-add those changes for the whole frail system to work
;;; alternatively you can just email me that an update occurred in some package
;;; and i'll merge and push the changes
;;;;; specific sections are demarcated by five semicolons, like this line
;;; do a global search through all such marks to go through all major sections

;; TODO: add send keystroke to window so i can pause soundcloud from without

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

;;; TODO: why isn't this doing anything???
;;; might need cedet installed/configured
;; (hs-minor-mode) ;; C-c @ C-c for folding up code blocks!!!
;; (add-hook 'prog-mode-hook #'hs-minor-mode) ; add to all programming modes

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

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
(add-to-list 'load-path "~/.emacs.d/color-themes")
(require 'color-theme-danny)
(color-theme-danny)

(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)

;;;;; keybindings
;; FIND CURRENT KEYBINDINGS WITH C-h b !!!!!
;; use C-h k to look at the current key sequence being entered!
;; this is useful when creating new keybindings
;;; C-x C-z to suspend!

;;; reset quit key combination to close
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x C-c C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c C-f") 'delete-frame)

(add-hook 'org-mode-hook
          (local-set-key (kbd "<tab>") 'smart-tab))

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

;;; TODO: make this functional
;; (defun yank-push ()
;;   (interactive)
;;   (browse-kill-ring-forward 2)
;;   (yank-pop))

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
 '(asm-comment-char 35)
 '(color-theme-directory ("/home/cosmicexplorer/.emacs.d/color-themes/"))
 '(fill-column 80)
 '(number-of-extra-newlines-to-preserve-js-mode 0)
 '(org-support-shift-select (quote always))
 '(org-support-shift-select (quote always))
 '(server-delete-tty t))
(put 'erase-buffer 'disabled nil)
