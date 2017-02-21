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
(defvar do-ssh-agent-command-on-start t
  "Whether or not to run an ssh-agent command on starting up emacs.")
(defvar id-rsa-path (let ((id-rsa-path (concat (getenv "HOME") "/.ssh/id_rsa")))
                      (if (file-exists-p id-rsa-path) id-rsa-path nil))
  "Path to desired id_rsa file for ssh. Used in init-scripts/interface.el to
stop ssh from prompting you every time you run git.")
(defvar ssh-pass nil
  "Default password to use for ssh-agent. Keep this secure.")
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
(defvar erc-nick nil
  "Nick to use for erc.")
(defvar erc-port 6667
  "Default port to use for erc.")
(defvar erc-server-pass-alist nil
  "Alist of passwords and ports for servers to connect with erc.")
(defvar submodule-makes-to-ignore nil
  "List of submodule makes to ignore compilation for.")
(defvar dont-ask-about-git nil
  "If git not installed, don't worry about it.")

;;; used in user customizations
(defmacro with-internet-connection (&rest body)
  "Perform BODY only if we can grab a url in a short period of time."
  `(progn
     (unless (featurep 'url-queue) (require 'url-queue))
     ;; url-queue-retrieve used because of built-in timeout
     (url-queue-retrieve
      ;; arbitrary url, chosen because github's uptime is ridiculous (remember
      ;; when all of china ddos'd them? incredible) and they probably aren't
      ;; tracking my browsing info
      "https://github.com"
      (lambda (status)
        (let ((err (plist-get status :error)))
          (unless err ,@body)))
      nil t t)))

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.  FILE is normally a feature name, but it
can also be a file name, in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file ,`'(progn ,@body))))

(defvar after-load-init-hook nil
  "Hook to run whatever after loading packages n functions n whatever.")
(defvar use-omnisharp t "C#!!!!!!!")
(defvar use-https t)

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
    (setq do-ssh-agent-command-on-start nil)
    (setq save-visited-files nil)))

(defun load-my-init-script (file-name)
  "Loads script located in init-scripts directory."
  (load-file (concat init-home-folder-dir "init-scripts/" file-name ".el")))

(add-hook 'after-load-init-hook
          (lambda () (load-my-init-script "visuals")))

;;; load the packages i like
(load-my-init-script "packages")

;;; load elisp
;;; should be /after/ byte-recompilation
(load-my-init-script "requires")

;;; for compatibility between different operating environments
(load-my-init-script "compat")
;;; enforce my strong opinions on the default emacs ui
(load-my-init-script "interface")
;;; do some additional work to setup packages
(load-my-init-script "package-setup")
;;; load (programming) language-specific settings
(load-my-init-script "languages")
;;; cause what else is emacs for
(load-my-init-script "keybindings")

;;; byte-compile everything: slow on first startup, but /significantly/ faster
;;; during normal usage
(async-start
 (lambda ()
   (byte-recompile-directory user-emacs-directory 0))
 'ignore)

;;; sometimes fails on 'require call
(load-file (expand-file-name
            (concat user-emacs-directory "lisp/smart-compile.el")))

(when do-ssh-agent-command-on-start (setup-ssh-agent))

;;; let's do it
(run-hooks 'after-load-init-hook)
;;; reload org from submodule
(require 'org-loaddefs)
(require 'org)
(require 'ox)
(require 'ox-latex)
(require 'ox-html)
(require 'ox-publish)

;;; save visited files to buffer
(when save-visited-files
  (add-hook 'after-init-hook #'reread-visited-files-from-disk)
  (add-hook 'kill-emacs-hook #'save-visiting-files-to-buffer))

;;; if everything loaded correctly, clear that last message
(message "")
(setq init-loaded-fully t)
(put 'upcase-region 'disabled nil)

(setq prolog-program-name (if (executable-find "swipl") "swipl" "prolog"))

(switch-to-buffer "*scratch*")
(delete-other-windows)

(setq visible-bell nil)

(garbage-collect)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(TeX-newline-function (quote reindent-then-newline-and-indent))
 '(ag-highlight-search t)
 '(asm-comment-char 35)
 '(async-shell-command-buffer (quote new-buffer))
 '(auto-revert-verbose nil)
 '(browse-url-browser-function (quote browse-url-chrome))
 '(cloc-use-3rd-gen nil)
 '(coffee-args-compile (quote ("-c" "-b" "--no-header" "-m")))
 '(coffee-indent-like-python-mode t)
 '(coffee-switch-to-compile-buffer t)
 '(coffee-tab-width 2)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode)))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(cperl-hairy t)
 '(cperl-invalid-face (quote cperl-no-trailing-whitespace-face))
 '(dabbrev-case-replace nil)
 '(dired-auto-revert-buffer t)
 '(dired-clean-up-buffers-too nil)
 '(dired-guess-shell-alist-user
   (quote
    (("\\.tar\\.xz\\'" "tar xpf" "sudo pacman -U")
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
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-fill-mode t)
 '(erc-highlight-nicknames-mode t)
 '(erc-irccontrols-mode t)
 '(erc-join-buffer (quote bury))
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode nil)
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-prompt (quote get-erc-prompt))
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-stamp-mode t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(fill-column 80)
 '(git-gutter:update-hooks
   (quote
    (after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook text-scale-mode-hook magit-revert-buffer-hook magit-status-refresh-hook magit-run-git-hook)))
 '(git-gutter:update-interval 1)
 '(git-gutter:window-width 0)
 '(grep-command (concat "\"" init-home-folder-dir "switch-grep.sh\" "))
 '(grep-highlight-matches (quote auto))
 '(grep-use-null-device nil)
 '(gud-key-prefix [3 134217737])
 '(haskell-check-command "hlint")
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(helm-ag--preview-highlight-matches (quote any))
 '(helm-ag--preview-max-matches 500)
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
 '(js2-global-externs (quote ("history")))
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
 '(linum-relative-plusp-offset 1)
 '(lua-indent-level 2)
 '(magit-display-buffer-function (quote magit-display-buffer-traditional))
 '(magit-no-confirm
   (quote
    (kill-process stage-all-changes unstage-all-changes)))
 '(magit-push-always-verify nil)
 '(magit-revert-buffers 5 t)
 '(markdown-export-async t)
 '(markdown-gfm-additional-languages nil)
 '(markdown-list-indent-width 4)
 '(markdown-live-preview-delete-export (quote delete-on-destroy))
 '(markdown-live-preview-do-sync nil)
 '(multi-isearch-search nil)
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
    (polymode intero shm nhexl-mode web-mode f3 scrooge projectile thrift cuda-mode visual-fill-column realgud mmm-mode pdf-tools font-lock-studio shut-up git-gutter-fringe yaml-mode sourcemap helm-ag wgrep wgrep-ag wgrep-helm ag pacmacs slime-company enh-ruby-mode robe tuareg solarized-theme color-theme-solarized highlight-parentheses racket-mode sage-shell-mode gnuplot-mode gnuplot sml-mode skewer-mode csv-mode git-gutter matlab-mode speech-tagger lua-mode ensime scala-mode2 company-ghc company-ghci ghc epresent helm-gtags ggtags xterm-color web-beautify w3m smartrep rainbow-mode rainbow-delimiters paredit omnisharp misc-cmds minimap literate-coffee-mode linum-relative less-css-mode js2-mode helm-swoop go-mode flycheck-package evil espuds ein company color-theme cloc cider better-defaults auctex 2048-game)))
 '(perl6-indent-offset 2)
 '(rainbow-ansi-colors t)
 '(rainbow-html-colors t)
 '(rainbow-latex-colors t)
 '(rainbow-r-colors t)
 '(rainbow-x-colors t)
 '(safe-local-variable-values
   (quote
    ((c-offsets-alist
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
 '(yank-pop-change-selection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-no-trailing-whitespace-face ((t (:underline nil))))
 '(italic ((t (:foreground "magenta" :slant italic))))
 '(underline ((t (:foreground "yellow" :underline t))))
 '(woman-unknown ((t (:background "#333333" :foreground "#ff0000")))))
