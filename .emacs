;;; -* lexical-binding: t -*-


;;;;; (1) Define `defcustom' base groups for other init code to attach to.

(defgroup my-customizations nil "all `defcustom' forms in my own init scripts.")

(defgroup my-errors nil
  "`defcustom' group for error handling in my own emacs lisp code."
  :group 'my-customizations)

(define-error 'my-errors "Errors in my own emacs lisp code.")


;;;;; (2) Define convenience functions to access and load files in `init-scripts/'.
;;;;;     TODO: If we rewrite all `init-scripts/' files to "(provide 'xxx)", we can avoid having
;;;;;     special load methods for that code!

(defconst init-home-folder-dir (file-truename user-emacs-directory)
  "The absolute and canonical path to the directory containing .emacs and `init-scripts/'.")

(defun home-dir-path (relative-path)
  "Wrapper for `expand-file-name' acting on RELATIVE-PATH within `init-home-folder-dir'.

The constructed path is not checked to exist, but is probably expected to exist."
  (expand-file-name relative-path init-home-folder-dir))

(defun ensure-single-trailing-slash (dir-path)
  "Remove any trailing slashes from DIR-PATH to insert a single trailing slash.

This ensures we can concatenate any other path component to its right side to get a valid path."
  (cl-assert (not (string-empty-p dir-path)) t
             "Empty strings are rejected to avoid producing a filesystem root '/' by accident.")
  (replace-regexp-in-string "/*\\'" "/" dir-path))

(cl-defun home-dir-resolve (fname &key (prefix nil) (suffix nil))
  "Concatenate PREFIX to FNAME to SUFFIX.

Uses `ensure-single-trailing-slash' to treat PREFIX, if provided."
  (let ((prefix (if (stringp prefix)
                    (ensure-single-trailing-slash prefix)
                  ""))
        (suffix (if (stringp suffix)
                    suffix
                  "")))
    (format "%s%s%s" prefix fname suffix)))

(defun resolve-init-scripts-script (fname)
  "Resolve FNAME to a `.el' file within the `init-scripts/' subdir."
  (let ((relative-path (home-dir-resolve fname :prefix "init-scripts" :suffix ".el")))
    (home-dir-path relative-path)))


;;;;; (3) Define locations for backup of various emacs state.

(defconst backup-base (home-dir-path (ensure-single-trailing-slash "backup-files"))
  "Directory to store emacs backups in.")

(defconst undo-tree-history-base (home-dir-path (ensure-single-trailing-slash "undo-tree-history"))
  "Directory to store `undo-tree' history persistently.")


;;;;; (4) Load init-scripts one by one, in the mysterious correct order.
;;;;;     TODO: rewrite all of `init-scripts/' as packages, and simply load then via `require'!

;;; Run `package-initialize' and configure `package-archives' for elpa and melpa.
(load-file (resolve-init-scripts-script "packages"))

;;; Bring our own as well as installed m?elpa packages into scope.
;;; This adds `utils/', `integrations/' and `lisp/' to the `load-path'.
(load-file (resolve-init-scripts-script "requires"))

;;; Some quick checks for compatibility between different operating environments.
(load-file (resolve-init-scripts-script "compat"))

;;; Configure package variables and `defadvice' some functions. Make sure that customizable settings
;;; go in `.emacs' or `danny-theme'!
(load-file (resolve-init-scripts-script "package-setup"))

;;; Configure everything I don't like about the emacs API and built-in packages.
(load-file (resolve-init-scripts-script "interface"))

;;; Configure hooks and workaround logic for specific languages.
(load-file (resolve-init-scripts-script "languages"))

;;; REBIND ALL THE THINGS!!!! EVERYTHING IS OFF THE DEFAULT!!!! MY KEYS ARE MINE!!!!
(load-file (resolve-init-scripts-script "keybindings"))

;;; Non-Custom configuration (and overrides) of how emacs displays things.
(load-file (resolve-init-scripts-script "visuals"))


;;;;; (5) Setup tasks that rely on state that was built up in the prior section.
;;;;;     TODO: make a megafunction that does this all and call it here instead?

;;; Seems like this would be a good time to GC, not that the GC ever really bothers me...
(add-hook 'after-init-hook #'garbage-collect)

;;; Make process buffers stop whining when I quit emacs.
(setup-buffer-save-prompts)

;;; Setup the emacs server!
(double-checked-server-init)

;;; load submodules!!!! this is a mildly complex function that interacts with git and the
;;; filesystem, but it seems mostly reliable somehow.
(setup-submodules-load)

;;; Remove any buffers e.g. for files that don't exist, or many process buffers. This reduces the
;;; chance that such a buffer will prompt you when you exit this emacs session!
;;; See `setup-buffer-save-prompts' for more background on this prompting problem.
(advice-add 'save-buffers-kill-emacs :before #'clean-nonvisiting-buffers)


;;;;; (6) Custom settings, which is hand- and machine-edited. This one is sparse so that the
;;;;;     customization in `danny-theme' can decouple customization entries from our init scripts.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '(("" . "/home/cosmicexplorer/.emacs.d/backup-files/")))
 '(copyright-query t)
 '(default-justification 'left)
 '(global-undo-tree-mode t)
 '(grep-command "gr ")
 '(grep-highlight-matches 'auto)
 '(grep-use-null-device nil)
 '(gud-key-prefix [3 134217737])
 '(haskell-check-command "hlint")
 '(haskell-process-type 'cabal-repl)
 '(haskell-tags-on-save t)
 '(helm-ag--preview-highlight-matches 'any)
 '(helm-ag--preview-max-matches 500)
 '(helm-ag-do-display-preview nil)
 '(helm-ag-insert-at-point 'symbol)
 '(helm-ag-use-agignore t)
 '(helm-ag-use-emacs-lisp-regexp nil)
 '(helm-completing-read-handlers-alist
   '((describe-function . helm-completing-read-symbols)
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
     (load-file . ido)))
 '(helm-ff-fuzzy-matching nil)
 '(helm-follow-mode-persistent t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-rg-default-directory 'git-root)
 '(helm-swoop-pre-input-function (lambda nil (thing-at-point 'symbol)))
 '(highlight-80+-columns 100)
 '(highlight-parentheses-background-colors '("light goldenrod"))
 '(highlight-parentheses-colors '("chocolate" "magenta" "tomato" "yellow"))
 '(hippie-expand-try-functions-list
   '(try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol))
 '(ido-enable-flex-matching t)
 '(ido-mode 'both nil (ido))
 '(inhibit-startup-buffer-menu t)
 '(inhibit-startup-echo-area-message nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(js2-global-externs '("history" "getComputedStyle"))
 '(js2-include-node-externs t)
 '(kill-buffer-trash-alist
   '((markdown-mode lambda
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
             (file-name-sans-extension f))))
 '(latex-engine "lualatex")
 '(linum-relative-current-symbol ">")
 '(linum-relative-format "%4s")
 '(linum-relative-plusp-offset 0)
 '(lua-indent-level 2)
 '(magit-auto-revert-mode t)
 '(magit-display-buffer-function 'magit-display-buffer-traditional)
 '(magit-no-confirm '(kill-process stage-all-changes unstage-all-changes))
 '(magit-push-always-verify nil)
 '(magit-remote-add-set-remote.pushDefault 'ask)
 '(magit-revert-buffers 5 t)
 '(markdown-export-async t)
 '(markdown-gfm-additional-languages nil)
 '(markdown-indent-on-enter nil)
 '(markdown-list-indent-width 2)
 '(markdown-live-preview-delete-export 'delete-on-destroy)
 '(markdown-live-preview-do-sync nil)
 '(minibuffer-depth-indicate-mode t)
 '(minibuffer-eldef-shorten-default t)
 '(minibuffer-electric-default-mode t)
 '(minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
 '(multi-isearch-search nil)
 '(my-isearch-search-fun 'do-normal-isearch)
 '(my-loc-lib-do-on-result '(find-file))
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files nil)
 '(org-catch-invisible-edits 'smart)
 '(org-confirm-babel-evaluate nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-highlight-latex-and-related '(native latex script entities))
 '(org-link-elisp-confirm-function 'y-or-n-p)
 '(org-link-from-user-regexp nil)
 '(org-link-shell-confirm-function 'y-or-n-p)
 '(org-src-fontify-natively t)
 '(org-startup-folded t)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(grip-mode lsp-mode markdown-mode matlab-mode multiple-cursors nix-mode pkgbuild-mode typescript-mode w3m web-mode tidal auctex swift-mode uuid idris-mode 0blayout 2048-game ag aggressive-fill-paragraph all-the-icons-dired all-the-icons-gnus all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich bart-mode better-defaults cider cl-lib cloc cmake-font-lock cmake-mode color-theme color-theme-approximate color-theme-modern company company-ghci company-nixos-options csv-mode cuda-mode dhall-mode dired-sidebar diredfl diredful dockerfile-mode dynamic-fonts ein emoji-fontset enh-ruby-mode epresent espuds ess-R-data-view ess-r-insert-obj ess-smart-equals ess-smart-underscore ess-view ess-view-data evil f3 faceup flycheck-package flycheck-rust font-lock-profiler font-lock-studio fontawesome fontify-face ggtags git-gutter git-gutter-fringe gnuplot gnuplot-mode go-mode graphql-mode groovy-mode helm-R helm-ag helm-gtags helm-nixos-options helm-rg helm-swoop helpful highlight-parentheses highlight-quoted highlight-refontification highlight-stages ibuffer-sidebar info-buffer info-colors info-rename-buffer inform jq-mode js2-mode kotlin-mode less-css-mode linum-relative lisp-extra-font-lock lisp-local literate-coffee-mode lua-mode magic-latex-buffer magit-popup mediawiki minibuffer-line minimap mmm-mode modern-cpp-font-lock modern-fringes modern-sh morlock nhexl-mode niceify-info nim-mode nix-buffer nix-env-install nix-sandbox nix-update nixpkgs-fmt ob-coffeescript ob-rust org org-agenda-property org-beautify-theme org-edna org-pdftools org-pretty-tags org-radiobutton org-random-todo org-randomnote org-ref org-sync org-table-comment org-transform-tree-table org-translate org-tree-slide org-treeusage orgit orgnav origami-predef ox-gfm pabbrev pacmacs paredit pcre2el pdf-tools php-mode poly-R polymode preproc-font-lock pretty-sha-path projectile propfont-mixed proportional protobuf-mode python-info racer rainbow-delimiters rainbow-mode robe rust-mode sage-shell-mode sass-mode scala-mode scrooge shm shut-up simple-call-tree skewer-mode slime-company smart-compile smart-tab smartrep sml-mode solarized-theme sourcemap speech-tagger strace-mode sysctl thrift toml-mode udev-mode undo-tree unicode-fonts unicode-math-input unicode-progress-reporter unicode-whitespace use-package use-ttf vimrc-mode visual-fill-column web-beautify wgrep wgrep-ag wgrep-helm xterm-color yaml-mode yaml-mode))
 '(perl6-indent-offset 2)
 '(python-indent-def-block-scale 1)
 '(racer-command-timeout 0.001)
 '(racer-eldoc-timeout 0.001)
 '(rainbow-ansi-colors t)
 '(rainbow-html-colors t)
 '(rainbow-latex-colors t)
 '(rainbow-r-colors t)
 '(rainbow-x-colors t)
 '(read-buffer-completion-ignore-case t)
 '(read-file-name-completion-ignore-case t)
 '(rust-indent-offset 2)
 '(safe-local-variable-values
   '((highlight-80+-columns . 99)
     (diff-add-log-use-relative-names . t)
     (comment-end)
     (markdown-list-indent-width . 4)
     (highlight-80+-columns . 80)
     (highlight-80+-columns . 88)
     (org-todo-keyword-faces quote
                             (("IDEA" . turqoise)
                              ("RESEARCH" . yellow)
                              ("WIP" . "sky blue")
                              ("WAIT-FOR-REVIEW" . org-warning)
                              ("CANCELED" :foreground "blue" :weight bold)))
     (org-todo-keywords quote
                        ((sequence "IDEA" "IN PROGRESS" "GET IT IN" "DONE" "BLOCKED" "|" "DONE" "DELEGATED")))
     (org-todo-keywords quote
                        ((sequence "IDEA" "WIP" "DONE" "BLOCKED" "|" "DONE" "DELEGATED")))
     (org-todo-keywords quote
                        ((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED")))
     (whitespace-check-buffer-indent)
     (highlight-80+-columns . 100)
     (highlight-stages-mode)
     (highlight-sexp-mode)
     (highlight-80+-mode)
     (TeX-auto-untabify . t)
     (comment-start . //)
     (f3-default-directory . /home/cosmicexplorer/projects/active/ping-pong)
     (f3-before-args "-not" "(" "-ipath" "*.git/*" "-or" "-ipath" "*.pants.d/*" "-or" "-iname" "*.pyc" ")")
     (f3-default-directory . project)
     (c-file-offsets
      (block-close . 0)
      (brace-list-close . 0)
      (brace-list-entry . 0)
      (brace-list-intro . +)
      (case-label . 0)
      (class-close . 0)
      (defun-block-intro . +)
      (defun-close . 0)
      (defun-open . 0)
      (else-clause . 0)
      (inclass . +)
      (label . 0)
      (statement . 0)
      (statement-block-intro . +)
      (statement-case-intro . +)
      (statement-cont . +)
      (substatement . +)
      (topmost-intro . 0))
     (markdown-list-indent-width . 2)
     (markdown-enable-math . t)
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
     (flycheck-mode)))
 '(undo-tree-history-directory-alist
   '(("" . "/home/cosmicexplorer/.emacs.d/undo-tree-history/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "YOFo" :family "Telegrama"))))
 '(bold-italic ((t (:family "Telegrama Italic"))))
 '(italic ((t (:family "Telegrama Italic"))))
 '(variable-pitch ((t (:family "Ancho")))))
