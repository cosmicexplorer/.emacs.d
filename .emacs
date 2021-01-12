;;; -* lexical-binding: t -*-

(require 'package)
(require 'cl-lib)

(package-initialize)

(defgroup my-customizations nil "all `defcustom' forms in my own init scripts")

(defconst init-home-folder-dir (file-truename user-emacs-directory))

(setq backup-directory-alist `(("." . ,(concat init-home-folder-dir "backup-files/")))
      undo-tree-history-directory-alist `(("." . ,(concat init-home-folder-dir "undo-tree-history/"))))

(defgroup my-errors nil
  "`defcustom' group for error handling in my own emacs lisp code."
  :group 'my-customizations)

(define-error 'my-errors "Errors in my own emacs lisp code.")
(define-error
  'my-init-error "Error in my personal emacs initialization." 'my-errors)

;; starts emacs in server form so i can use emacsclient to add files
;; but only if server isn't already started
(require 'server)
(when (fboundp 'server-running-p)
  (unless (server-running-p)
    (server-start)))

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

;;; Interact with package variables outside of `defcustom's.
(load-my-script "package-setup" "init-scripts")

;;; enforce my strong opinions on the default emacs ui
(load-my-script "interface" "init-scripts")
;;; do some additional work to setup packages
(load-my-script "package-setup" "init-scripts")
;;; load (programming) language-specific settings
(load-my-script "languages" "init-scripts")
;;; cause what else is emacs for
(load-my-script "keybindings" "init-scripts")

;;; make it look nice
(load-my-script "visuals" "init-scripts")

;;; load submodules!!!!
(setup-submodules-load)

;;; This just seems like a nice idea.
(add-hook 'after-init-hook #'garbage-collect)

(advice-add 'save-buffers-kill-emacs :before #'clean-nonvisiting-buffers)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 '(TeX-engine 'luatex)
 '(TeX-newline-function 'reindent-then-newline-and-indent)
 '(adaptive-fill-mode t)
 '(ag-default-search-fn 'ag-regexp)
 '(ag-highlight-search t)
 '(asm-comment-char 35)
 '(async-shell-command-buffer 'new-buffer)
 '(auto-revert-verbose nil)
 '(book-txt-view-buffer-contents 'fill)
 '(book-txt-view-font-size 12)
 '(browse-url-browser-function 'browse-url-firefox)
 '(browse-url-firefox-program "firefox-nightly")
 '(cloc-use-3rd-gen nil)
 '(coffee-args-compile '("-c" "--bare" "--no-header"))
 '(coffee-command
   "/Users/dmcclanahan/tools/coffeescript/invoke-coffeescript.zsh")
 '(coffee-indent-like-python-mode t)
 '(coffee-switch-to-compile-buffer t)
 '(coffee-tab-width 2)
 '(comint-prompt-read-only t)
 '(company-dabbrev-code-modes
   '(prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode))
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case t)
 '(company-tooltip-align-annotations t)
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(completion-auto-help 'lazy)
 '(completion-cycle-threshold t)
 '(completion-pcm-word-delimiters "-_./:|       ")
 '(completions-format 'horizontal)
 '(cperl-hairy nil)
 '(cperl-invalid-face 'cperl-no-trailing-whitespace-face)
 '(create-lockfiles nil)
 '(dabbrev-case-replace nil)
 '(desktop-save-mode t)
 '(dired-auto-revert-buffer t)
 '(dired-clean-up-buffers-too nil)
 '(dired-guess-shell-alist-user
   '(("\\.zip\\'" "unzip")
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
     ("\\.html\\'" "chromium" "firefox")
     ("\\.vi\\'" "labview")
     ("\\.R\\'" "Rscript")
     ("\\.svg\\'" "inkscape")
     ("\\.\\(?:mp3\\|wav\\|wmv\\)\\'" "cvlc --play-and-exit")
     ("\\.*" "xdg-open")))
 '(dired-listing-switches "-lavFh")
 '(dired-no-confirm t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(display-buffer-alist
   '(("\\*Async Shell Command\\*.*" display-buf-no-win-save-shell-command-buf)))
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
 '(echo-keystrokes 0.1)
 '(edebug-eval-macro-args t)
 '(edebug-save-windows nil)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-pair-mode t)
 '(electric-pair-pairs
   '((40 . 41)
     (123 . 125)
     (34 . 34)
     ((nth 0 electric-quote-chars)
      nth 1 electric-quote-chars)
     ((nth 2 electric-quote-chars)
      nth 3 electric-quote-chars)))
 '(electric-pair-skip-self 'electric-pair-default-skip-self)
 '(electric-pair-text-pairs
   '((34 . 34)
     ((nth 0 electric-quote-chars)
      nth 1 electric-quote-chars)
     ((nth 2 electric-quote-chars)
      nth 3 electric-quote-chars)))
 '(enable-recursive-minibuffers t)
 '(ensime-eldoc-hints 'all)
 '(ensime-startup-notification nil)
 '(ensime-startup-snapshot-notification nil)
 '(ess-S-assign "_")
 '(ess-own-style-list
   '((ess-indent-offset . 4)
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
     (ess-indent-with-fancy-comments . t)))
 '(ess-style 'OWN)
 '(eval-expression-print-level nil)
 '(f3-before-args
   '("-not" "(" "-ipath" "*.git/*" "-or" "-ipath" "*.pants.d/*" "-or" "-name" "*.pyc" ")"))
 '(fill-column 100)
 '(git-gutter:update-hooks
   '(after-save-hook after-revert-hook find-file-hook after-change-major-mode-hook text-scale-mode-hook magit-revert-buffer-hook magit-status-refresh-hook magit-run-git-hook))
 '(git-gutter:update-interval 1)
 '(git-gutter:window-width 0)
 '(global-company-mode t)
 '(global-linum-mode nil)
 '(global-smart-tab-mode t)
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
 '(magit-remote-add-set-remote\.pushDefault 'ask)
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
 '(package-selected-packages (
                               0blayout
                              2048-game
                              ag
                              aggressive-fill-paragraph
                              all-the-icons-dired
                              all-the-icons-gnus
                              all-the-icons-ibuffer
                              all-the-icons-ivy
                              all-the-icons-ivy-rich
                              auctex
                              bart-mode
                              bazel-mode
                              better-defaults
                              cider
                              cl-lib
                              cl-lib-highlight
                              cloc
                              cmake-font-lock
                              cmake-mode
                              color-theme
                              color-theme-approximate
                              color-theme-modern
                              color-theme-solarized
                              company
                              company-ghc
                              company-ghci
                              company-nixos-options
                              csv-mode
                              cuda-mode
                              dhall-mode
                              dired-sidebar
                              diredfl
                              diredful
                              dockerfile-mode
                              dynamic-fonts
                              ein
                              emoji-fontset
                              enh-ruby-mode
                              epresent
                              espuds
                              ess-R-data-view
                              ess-r-insert-obj
                              ess-smart-equals
                              ess-smart-underscore
                              ess-view
                              ess-view-data
                              evil
                              f3
                              faceup
                              flycheck-package
                              flycheck-rust
                              font-lock-profiler
                              font-lock-studio
                              fontawesome
                              fontify-face
                              ggtags
                              ghc
                              git-gutter
                              git-gutter-fringe
                              gnuplot
                              gnuplot-mode
                              go-mode
                              graphql-mode
                              grip-mode
                              groovy-mode
                              helm-R
                              helm-ag
                              helm-gtags
                              helm-nixos-options
                              helm-rg
                              helm-swoop
                              helpful
                              highlight-parentheses
                              highlight-quoted
                              highlight-refontification
                              highlight-stages
                              ibuffer-sidebar
                              info-buffer
                              info-colors
                              info-rename-buffer
                              inform
                              intero
                              jq-mode
                              js2-mode
                              kotlin-mode
                              less-css-mode
                              linum-relative
                              lisp-extra-font-lock
                              lisp-local
                              literate-coffee-mode
                              lsp-mode
                              lua-mode
                              magic-latex-buffer
                              magit-popup
                              markdown-mode
                              matlab-mode
                              mediawiki
                              minibuffer-line
                              minimap
                              mmm-mode
                              modern-cpp-font-lock
                              modern-fringes
                              modern-sh
                              morlock
                              multiple-cursors
                              nhexl-mode
                              niceify-info
                              nim-mode
                              nix-buffer
                              nix-env-install
                              nix-mode
                              nix-sandbox
                              nix-update
                              nixpkgs-fmt
                              ob-coffeescript
                              ob-rust
                              org
                              org-agenda-property
                              org-beautify-theme
                              org-edna
                              org-pdftools
                              org-pretty-tags
                              org-radiobutton
                              org-random-todo
                              org-randomnote
                              org-ref
                              org-sync
                              org-table-comment
                              org-transform-tree-table
                              org-translate
                              org-tree-slide
                              org-treeusage
                              orgit
                              orgnav
                              origami-predef
                              ox-gfm
                              pabbrev
                              pacmacs
                              paredit
                              pcre2el
                              pdf-tools
                              perl6-mode
                              php-mode
                              pkgbuild-mode
                              poly-R
                              polymode
                              preproc-font-lock
                              pretty-sha-path
                              projectile
                              propfont-mixed
                              proportional
                              protobuf-mode
                              python-info
                              racer
                              rainbow-delimiters
                              rainbow-mode
                              robe
                              rust-mode
                              rustic
                              sage-shell-mode
                              sass-mode
                              scala-mode
                              scrooge
                              shm
                              shut-up
                              simple-call-tree
                              skewer-mode
                              slime-company
                              smart-compile
                              smart-tab
                              smartrep
                              sml-mode
                              solarized-theme
                              sourcemap
                              speech-tagger
                              strace-mode
                              sysctl
                              thrift
                              toml-mode
                              typescript-mode
                              udev-mode
                              undo-tree
                              unicode-fonts
                              unicode-math-input
                              unicode-progress-reporter
                              unicode-whitespace
                              use-package
                              use-ttf
                              vimrc-mode
                              visual-fill-column
                              w3m
                              web-beautify
                              web-mode
                              wgrep
                              wgrep-ag
                              wgrep-helm
                              xterm-color
                              yaml-mode
                              yaml-mode))
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
   '((markdown-list-indent-width . 4)
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
 '(save-place-file "(expand-file-name \"saveplace\" init-home-folder-dir)")
 '(save-place-mode nil)
 '(search-default-mode 'char-fold-to-regexp)
 '(show-paren-mode t)
 '(smart-tab-completion-functions-alist
   '((lisp-mode . slime-complete-symbol)
     (text-mode . dabbrev-completion)))
 '(smart-tab-default-functions-alist
   '((org-mode . org-cycle)
     (markdown-mode . toggle-subtree-markdown)))
 '(smart-tab-disabled-major-modes nil)
 '(smart-tab-using-hippie-expand t)
 '(tool-bar-mode nil)
 '(undo-outer-limit 5000000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-visualizer-diff t)
 '(warning-suppress-types '((undo discard-info)))
 '(woman-fill-frame nil)
 '(woman-imenu t)
 '(xmllint-pretty-level 2)
 '(yank-pop-change-selection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "green" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "YOFo" :family "Telegrama"))))
 '(bold-italic ((t (:family "Telegrama Italic"))))
 '(danny-buffer-progress ((t (:box (:line-width (2 . 2) :style pressed-button)))))
 '(danny-modified-string ((t (:foreground "white" :box (:line-width (2 . 2) :style pressed-button)))))
 '(italic ((t (:family "Telegrama Italic"))))
 '(variable-pitch ((t (:family "Ancho")))))
