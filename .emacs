;;; -* lexical-binding: t -*-

;;; .....let's begin
(package-initialize)

(defgroup my-customizations nil "all `defcustom' forms in my own init scripts")

;;; add wherever emacs was invoked from to path
;;; done at top so we know we're not changing any directories
(defvar emacs-start-command (car command-line-args)
  "Command used to start emacs.")

;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014-2015

;;; IF YOU ARE HAVING CRASHES UPON OPENING A PARTICULAR FILE, TRY DELETING THAT
;;; FILE'S UNDO-TREE-HISTORY in ~/.emacs.d/undo-tree-history/!!!!!!!!!!!

;;; let's not hardcode everything like back in 9th grade
(defvar init-home-folder-dir (file-truename user-emacs-directory)
  "Location of this home directory.")

;;; CUSTOM VARS
;;; the below variables should be defcustoms, but i don't enjoy that setup
;;; because when you click "apply and save" on a "customize" prompt, it saves
;;; the choice directly to your .emacs file. after every such operation, i would
;;; have to then move the choice of defcustom from this .emacs file to a
;;; separate file if i wanted it to be gitignored. as a result, I am creating
;;; essentially my own more low-level version of defcustoms so that a single
;;; file can be modified without affecting version control. this also allows for
;;; storing personal information such as directory structure, or irc nicks,
;;; which is not really great for sharing on github. this "custom-vars.el" file
;;; is gitignored so that it may vary easily. meaningful defaults are provided
;;; below with each variable. it should be noted that this file is especially
;;; useful to store variables such as `org-agenda-files'.
(defvar warning-words-file nil
  "Path to file defining words to highlight specially. An example file would
contain:

todo
fixme
hack
broken
deprecated

Set in custom-vars.el, and used in init-scripts/interface.el.")
(defvar sbcl-special-command nil
  "Sometimes required because some versions of sbcl are difficult to wire up
correctly. Set in custom-vars.el")
(defvar save-visited-files t
  "Whether or not to restore all files that were visited during the previous
session. Used later in this file.")
(defvar saved-files (file-truename (concat init-home-folder-dir "saved-files"))
  "File path to save visited-files. Used later in this file.")
(defvar save-eshell-history t
  "Whether or not to save eshell history to disk. Used in
init-scripts/interface.el.")
(defvar save-shell-history t
  "Whether or not to save shell history to disk. Used in
init-scripts/interface.el.")
(defvar save-nonvisiting-files t
  "Whether or not to persist all buffers not visiting files to disk. Used in
init-scripts/interface.el.")
(defvar save-tramp-bufs t
  "Whether or not to revisit tramp buffers opened in a previous session. Used in
init-scripts/interface.el.")
(defvar submodule-makes-to-ignore nil
  "List of submodule makes to ignore compilation for.")
(defvar dont-ask-about-git nil
  "If git not installed, don't worry about it.")

;;; TODO: make shorthand for val being matched within BODY-FORMS of pcase

(defvar after-load-init-hook nil
  "Hook to run whatever after loading packages n functions n whatever.")
(defvar use-omnisharp t "C#!!!!!!!")
(defvar use-https t)

(defcustom my-files-to-open-xdg nil
  "files to open on startup"
  :type '(list string))

(defgroup my-errors nil
  "`defcustom' group for error handling in my own emacs lisp code.")
(define-error 'my-errors "Errors in my own emacs lisp code.")
(define-error
  'my-init-error "Error in my personal emacs initialization." 'my-errors)

;;; load custom values for these variables (this file is .gitignored)
(let ((custom-var-file
       (concat
        (if load-file-name
            (file-name-directory (file-truename load-file-name))
          default-directory)
        "custom-vars.el"))
      (msg-string
       "Make a custom-vars.el! Only if you want, though.
Check out your .emacs."))
  (unless (file-exists-p custom-var-file)
    (with-temp-buffer
      (insert (concat "(with-current-buffer \"*scratch*\"
  (insert \"" msg-string "\")
  (newline))"))
      (write-region nil nil custom-var-file)))
  (load-file custom-var-file))

(unless (file-exists-p init-home-folder-dir)
  (throw 'no-home-holder "no emacs home directory found (check your .emacs)!"))

;;; added up here cause a lot of packages depend on it being defined without
;;; defining it themselves
(eval-when-compile (require 'cl))
(require 'json)

;; starts emacs in server form so i can use emacsclient to add files
;; but only if server isn't already started
(require 'server)
(when (fboundp 'server-running-p)
  (if (not (eq (server-running-p) t))
      (server-start)
    (setq save-visited-files nil)))

(defun load-my-script (fname &optional dir)
  (load-file (expand-file-name
              (format "%s/%s.el" (or dir ".") fname)
              init-home-folder-dir)))

;;; load the packages i like
(load-my-script "packages" "init-scripts")

;;; load elisp
;;; should be /after/ byte-recompilation
(load-my-script "requires" "init-scripts")

;;; load all my cool functions!!!
(load-my-script "functions" "utils")

;;; for compatibility between different operating environments
(load-my-script "compat" "init-scripts")

;;; enforce my strong opinions on the default emacs ui
(load-my-script "interface" "init-scripts")
;;; do some additional work to setup packages
(load-my-script "package-setup" "init-scripts")
;;; load (programming) language-specific settings
(load-my-script "languages" "init-scripts")
;;; cause what else is emacs for
(load-my-script "keybindings" "init-scripts")

(add-hook 'after-init-hook (z (load-my-script "visuals" "init-scripts")))

(cl-mapc #'open-if-not-already my-files-to-open-xdg)

;;; byte-compile everything: slow on first startup, but /significantly/ faster
;;; during normal usage
(async-start
 (lambda ()
   (byte-recompile-directory user-emacs-directory 0))
 'ignore)

;;; sometimes fails on 'require call

;;; save visited files to buffer
(when save-visited-files
  (add-hook 'after-init-hook #'reread-visited-files-from-disk)
  (add-hook 'kill-emacs-hook #'save-visiting-files-to-buffer))

(add-hook 'after-init-hook #'clean-init-screen t)
(add-hook 'after-init-hook #'redisplay)
(add-hook 'after-init-hook #'redisplay t)

;;; let's do it
(add-hook
 'after-init-hook
 (z
  (progn
    (when check-internet-connection
      (setup-internet-connection-check 'check))
    (when monitor-internet-connection
      (setup-internet-connection-check 'monitor)))))

;;; deprecated
(unless (null after-load-init-hook)
  (user-error "%s" "`after-load-init-hook' is deprecated!"))

(add-hook 'after-init-hook #'garbage-collect)
(add-hook 'after-init-hook #'garbage-collect t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(TeX-newline-function (quote reindent-then-newline-and-indent))
 '(ag-default-search-fn (quote ag-regexp))
 '(ag-highlight-search t)
 '(asm-comment-char 35)
 '(async-shell-command-buffer (quote new-buffer))
 '(auto-revert-verbose nil)
 '(book-txt-view-buffer-contents (quote fill))
 '(book-txt-view-font-size 12)
 '(browse-url-browser-function (quote browse-url-chrome))
 '(cloc-use-3rd-gen nil)
 '(coffee-args-compile (quote ("-c" "-b" "--no-header" "-m")))
 '(coffee-indent-like-python-mode t)
 '(coffee-switch-to-compile-buffer t)
 '(coffee-tab-width 2)
 '(comint-prompt-read-only t)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(cperl-hairy nil)
 '(cperl-invalid-face (quote cperl-no-trailing-whitespace-face))
 '(create-lockfiles nil)
 '(dabbrev-case-replace nil)
 '(default-text-height 90)
 '(dired-auto-revert-buffer t)
 '(dired-clean-up-buffers-too nil)
 '(dired-guess-shell-alist-user
   (quote
    (("\\.zip\\'" "unzip")
     ("\\.tar\\.xz\\'" "tar xpf" "sudo pacman -U")
     ("\\.exe\\'" "wine")
     ("\\.\\(?:jpe?g\\|png\\|gif\\|bmp\\)\\'" "display" "gimp")
     ("\\.\\(?:pdf\\|ps\\)\\'" "evince")
     ("\\.coffee\\'" "coffee")
     ("\\.pl\\'" "perl")
     ("\\.jar\\'" "java -jar")
     ("^[^\\.]+\\'"
      (concat user-emacs-directory "exec-file.sh"))
     ("\\.bash\\'" "bash")
     ("\\.zsh\\'" "zsh")
     ("\\.sh\\'" "sh")
     ("\\.py\\'" "python" "python2" "python3")
     ("\\..+x\\'" "libreoffice")
     ("\\.html\\'" "google-chrome-stable" "firefox")
     ("\\.vi\\'" "labview")
     ("\\.R\\'" "Rscript")
     ("\\.svg\\'" "inkscape")
     ("\\.\\(?:mp3\\|wav\\|wmv\\)\\'" "cvlc --play-and-exit")
     ("\\.*" "xdg-open"))))
 '(dired-listing-switches "-lavFh")
 '(dired-no-confirm t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(display-buffer-alist
   (quote
    (("\\*Async Shell Command\\*.*" display-buf-no-win-save-shell-command-buf))))
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(edebug-eval-macro-args t)
 '(edebug-save-windows nil)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(enable-recursive-minibuffers t)
 '(ess-S-assign "_")
 '(ess-default-style (quote OWN))
 '(ess-own-style-list
   (quote
    ((ess-indent-offset . 4)
     (ess-offset-arguments . open-delim)
     (ess-offset-arguments-newline . prev-line)
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-align-nested-calls "ifelse")
     (ess-align-arguments-in-calls "function[[:space:]]*(")
     (ess-align-continuations-in-calls . t)
     (ess-align-blocks control-flow)
     (ess-indent-from-lhs arguments fun-decl-opening)
     (ess-indent-from-chain-start . t)
     (ess-indent-with-fancy-comments . t))))
 '(fill-column 80)
 '(git-gutter:update-hooks
   (quote
    (after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook text-scale-mode-hook magit-revert-buffer-hook magit-status-refresh-hook magit-run-git-hook)))
 '(git-gutter:update-interval 1)
 '(git-gutter:window-width 0)
 '(grep-command "gr ")
 '(grep-highlight-matches (quote auto))
 '(grep-use-null-device nil)
 '(gud-key-prefix [3 134217737])
 '(haskell-check-command "hlint")
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(helm-ag--preview-highlight-matches (quote any))
 '(helm-ag--preview-max-matches 500)
 '(helm-ag-do-display-preview nil)
 '(helm-ag-insert-at-point (quote symbol))
 '(helm-ag-use-agignore t)
 '(helm-ag-use-emacs-lisp-regexp nil)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (tmm-menubar)
     (load-file . ido))))
 '(helm-ff-fuzzy-matching nil)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-swoop-pre-input-function (lambda nil (thing-at-point (quote symbol))))
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(hl-paren-background-colors (quote ("light goldenrod")))
 '(hl-paren-colors (quote ("chocolate" "magenta" "tomato" "yellow")))
 '(initial-buffer-choice t)
 '(js2-global-externs (quote ("history" "getComputedStyle")))
 '(js2-include-node-externs t)
 '(kill-buffer-trash-alist
   (quote
    ((markdown-mode lambda
                    (f)
                    (concat
                     (file-name-sans-extension f)
                     ".html"))
     (coffee-mode lambda
                  (f)
                  (concat
                   (file-name-sans-extension f)
                   ".js"))
     (java-mode lambda
                (f)
                (concat
                 (file-name-sans-extension f)
                 ".class"))
     (csharp-mode lambda
                  (f)
                  (file-name-sans-extension f))
     (c++-mode lambda
               (f)
               (file-name-sans-extension f))
     (c-mode lambda
             (f)
             (file-name-sans-extension f)))))
 '(latex-engine "lualatex")
 '(linum-relative-current-symbol ">")
 '(linum-relative-format "%4s")
 '(linum-relative-plusp-offset 0)
 '(lua-indent-level 2)
 '(magit-auto-revert-mode t)
 '(magit-display-buffer-function (quote magit-display-buffer-traditional))
 '(magit-no-confirm
   (quote
    (kill-process stage-all-changes unstage-all-changes)))
 '(magit-push-always-verify nil)
 '(magit-revert-buffers 5 t)
 '(markdown-export-async t)
 '(markdown-gfm-additional-languages nil)
 '(markdown-indent-on-enter nil)
 '(markdown-list-indent-width 4)
 '(markdown-live-preview-delete-export (quote delete-on-destroy))
 '(markdown-live-preview-do-sync nil)
 '(multi-isearch-search nil)
 '(my-isearch-search-fun (quote do-normal-isearch))
 '(my-loc-lib-do-on-result (quote (find-file)))
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files nil)
 '(org-catch-invisible-edits (quote smart))
 '(org-confirm-babel-evaluate nil)
 '(org-confirm-elisp-link-function (quote y-or-n-p))
 '(org-confirm-shell-link-function (quote y-or-n-p))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-from-is-user-regexp nil)
 '(org-src-fontify-natively t)
 '(org-startup-folded t)
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (markdown-mode jq-mode vimrc-mode polymode intero shm nhexl-mode web-mode f3 scrooge projectile thrift cuda-mode visual-fill-column realgud mmm-mode pdf-tools font-lock-studio shut-up git-gutter-fringe yaml-mode sourcemap wgrep wgrep-ag wgrep-helm ag pacmacs slime-company enh-ruby-mode robe tuareg solarized-theme color-theme-solarized highlight-parentheses racket-mode sage-shell-mode gnuplot-mode gnuplot sml-mode skewer-mode csv-mode git-gutter matlab-mode speech-tagger lua-mode ensime scala-mode company-ghc company-ghci ghc epresent helm-gtags ggtags xterm-color web-beautify w3m smartrep rainbow-mode rainbow-delimiters paredit omnisharp misc-cmds minimap literate-coffee-mode linum-relative less-css-mode js2-mode helm-swoop go-mode flycheck-package evil espuds ein company color-theme cloc cider better-defaults auctex 2048-game magit multiple-cursors)))
 '(perl6-indent-offset 2)
 '(rainbow-ansi-colors t)
 '(rainbow-html-colors t)
 '(rainbow-latex-colors t)
 '(rainbow-r-colors t)
 '(rainbow-x-colors t)
 '(safe-local-variable-values
   (quote
    ((markdown-enable-math . t)
     (c-offsets-alist
      (inexpr-class . +)
      (inexpr-statement . +)
      (lambda-intro-cont . +)
      (inlambda . c-lineup-inexpr-block)
      (template-args-cont c-lineup-template-args +)
      (incomposition . +)
      (inmodule . +)
      (innamespace . +)
      (inextern-lang . +)
      (composition-close . 0)
      (module-close . 0)
      (namespace-close . 0)
      (extern-lang-close . 0)
      (composition-open . 0)
      (module-open . 0)
      (namespace-open . 0)
      (extern-lang-open . 0)
      (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
      (objc-method-args-cont . c-lineup-ObjC-method-args)
      (objc-method-intro .
                         [0])
      (friend . 0)
      (cpp-define-intro c-lineup-cpp-define +)
      (cpp-macro-cont . +)
      (cpp-macro .
                 [0])
      (inclass . +)
      (stream-op . c-lineup-streamop)
      (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
      (arglist-cont c-lineup-gcc-asm-reg 0)
      (arglist-intro . +)
      (catch-clause . 0)
      (else-clause . 0)
      (do-while-closure . 0)
      (label . 2)
      (access-label . -)
      (substatement-label . 2)
      (substatement . +)
      (statement-case-open . 0)
      (statement-case-intro . +)
      (statement-block-intro . +)
      (statement-cont . +)
      (statement . 0)
      (brace-entry-open . 0)
      (brace-list-entry . 0)
      (brace-list-intro . +)
      (brace-list-close . 0)
      (brace-list-open . 0)
      (block-close . 0)
      (inher-cont . c-lineup-multi-inher)
      (inher-intro . +)
      (member-init-cont . c-lineup-multi-inher)
      (member-init-intro . +)
      (annotation-var-cont . +)
      (annotation-top-cont . 0)
      (topmost-intro-cont . c-lineup-topmost-intro-cont)
      (topmost-intro . 0)
      (knr-argdecl . 0)
      (func-decl-cont . +)
      (inline-close . 0)
      (inline-open . +)
      (class-close . 0)
      (class-open . 0)
      (defun-block-intro . +)
      (defun-close . 0)
      (defun-open . 0)
      (string . c-lineup-dont-change)
      (arglist-close . c-lineup-arglist)
      (substatement-open . 0)
      (case-label . 0)
      (block-open . 0)
      (c . 1)
      (comment-intro . 0)
      (knr-argdecl-intro . -))
     (c-cleanup-list scope-operator brace-else-brace brace-elseif-brace brace-catch-brace empty-defun-braces list-close-comma defun-close-semi)
     (c-hanging-semi&comma-criteria c-semi&comma-no-newlines-before-nonblanks)
     (c-hanging-colons-alist
      (member-init-intro before)
      (inher-intro)
      (case-label after)
      (label after)
      (access-label after))
     (c-hanging-braces-alist
      (substatement-open after)
      (brace-list-open after)
      (brace-entry-open)
      (defun-open after)
      (class-open after)
      (inline-open after)
      (block-open after)
      (block-close . c-snug-do-while)
      (statement-case-open after)
      (substatement after))
     (c-comment-only-line-offset . 0)
     (c-tab-always-indent . t)
     (f3-before-args "-not" "(" "-ipath" "*.git*" "-or" "-ipath" "*.pants.d*" ")")
     (js2-basic-offset . 4)
     (no-gfm)
     (Syntax . ANSI-Common-Lisp)
     (Base . 10)
     (no-gfm . t)
     (major-mode . sh-mode)
     (TeX-master . "proposal")
     (add-log-time-format lambda nil
                          (progn
                            (setq tz
                                  (getenv "TZ"))
                            (setq time
                                  (format-time-string "%a %b %e %H:%M:%S %Z %Y"
                                                      (current-time)))
                            (set-time-zone-rule tz)
                            time))
     (destroy-whitespace)
     (nil)
     (flycheck-mode))))
 '(search-default-mode (quote char-fold-to-regexp))
 '(smart-tab-completion-functions-alist
   (quote
    ((lisp-mode . slime-complete-symbol)
     (text-mode . dabbrev-completion))))
 '(smart-tab-default-functions-alist
   (quote
    ((org-mode . org-cycle)
     (markdown-mode . toggle-subtree-markdown))))
 '(smart-tab-disabled-major-modes nil)
 '(smart-tab-using-hippie-expand t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(woman-fill-frame nil)
 '(woman-imenu t)
 '(xmllint-pretty-level 2)
 '(yank-pop-change-selection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-no-trailing-whitespace-face ((t (:underline nil))))
 '(font-latex-script-char-face ((t (:foreground "burlywood"))))
 '(italic ((t (:foreground "magenta" :slant italic))))
 '(underline ((t (:foreground "yellow" :underline t))))
 '(woman-unknown ((t (:background "#333333" :foreground "#ff0000")))))
