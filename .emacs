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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pkgbuild-mode yaml-mode wgrep-helm wgrep-ag web-mode web-beautify w3m visual-fill-column vimrc-mode use-ttf use-package unicode-whitespace unicode-progress-reporter unicode-math-input unicode-fonts undo-tree udev-mode typescript-mode toml-mode sysctl strace-mode speech-tagger sourcemap solarized-theme sml-mode smartrep smart-tab smart-compile slime-company skewer-mode simple-call-tree shut-up shm scrooge scala-mode sass-mode sage-shell-mode rustic rust-mode robe rainbow-mode rainbow-delimiters python-info protobuf-mode proportional propfont-mixed pretty-sha-path preproc-font-lock poly-R php-mode pcre2el pacmacs pabbrev origami-predef orgnav orgit org-treeusage org-tree-slide org-translate org-transform-tree-table org-table-comment org-sync org-ref org-randomnote org-random-todo org-radiobutton org-pretty-tags org-pdftools org-edna org-beautify-theme org-agenda-property ob-rust ob-coffeescript nixpkgs-fmt nix-update nix-sandbox nix-mode nix-env-install nix-buffer nim-mode niceify-info nhexl-mode multiple-cursors morlock modern-sh modern-fringes modern-cpp-font-lock mmm-mode minimap minibuffer-line mediawiki matlab-mode magit-popup magic-latex-buffer lua-mode lsp-mode literate-coffee-mode lisp-local lisp-extra-font-lock linum-relative kotlin-mode jq-mode intero inform info-rename-buffer info-colors info-buffer ibuffer-sidebar highlight-stages highlight-refontification highlight-quoted highlight-parentheses helpful helm-swoop helm-nixos-options helm-gtags helm-ag helm-R groovy-mode grip-mode graphql-mode go-mode gnuplot-mode gnuplot git-gutter-fringe ggtags fontify-face fontawesome font-lock-studio font-lock-profiler flycheck-rust flycheck-package faceup f3 evil ess-view-data ess-view ess-smart-underscore ess-smart-equals ess-r-insert-obj ess-R-data-view espuds epresent enh-ruby-mode emoji-fontset ein dynamic-fonts dockerfile-mode diredful diredfl dired-sidebar dhall-mode cuda-mode company-nixos-options company-ghci company-ghc color-theme-modern color-theme-approximate cmake-font-lock cloc cl-lib-highlight cider better-defaults bazel-mode bart-mode auctex all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-ibuffer all-the-icons-gnus all-the-icons-dired aggressive-fill-paragraph ag 2048-game 0blayout)))
