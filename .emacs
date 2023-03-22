;;; -* lexical-binding: t -*-

(require 'cl-lib)


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

;;; Most variables are set in the theme `danny-theme' instead of here. Settings in .emacs should
;;; strictly refer to settings related to initialization, including any references to the
;;; init directory. `package-selected-packages' is modified automatically when a new package is
;;; installed, so we let it stay here as well.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 `(backup-directory-alist '(("" . ,backup-base)))
 `(undo-tree-history-directory-alist '(("" . ,undo-tree-history-base)))
 '(package-selected-packages
   '(graphviz-dot-mode company-coq proof-general morlock org-ref powershell grip-mode lsp-mode matlab-mode pkgbuild-mode tidal auctex swift-mode uuid idris-mode 0blayout 2048-game ag aggressive-fill-paragraph all-the-icons-dired all-the-icons-gnus all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich bart-mode better-defaults cider cl-lib cloc cmake-font-lock cmake-mode color-theme color-theme-approximate color-theme-modern company company-ghci company-nixos-options csv-mode cuda-mode dhall-mode dired-sidebar diredfl diredful dockerfile-mode dynamic-fonts ein emoji-fontset enh-ruby-mode epresent espuds ess-R-data-view ess-r-insert-obj ess-smart-equals ess-smart-underscore ess-view ess-view-data evil f3 faceup flycheck-package flycheck-rust font-lock-profiler font-lock-studio fontawesome fontify-face ggtags git-gutter git-gutter-fringe gnuplot gnuplot-mode go-mode graphql-mode groovy-mode helm-R helm-ag helm-gtags helm-nixos-options helm-rg helm-swoop highlight-quoted highlight-refontification highlight-stages ibuffer-sidebar info-buffer info-colors info-rename-buffer inform jq-mode js2-mode less-css-mode lisp-extra-font-lock lisp-local literate-coffee-mode magic-latex-buffer magit-popup mediawiki minibuffer-line minimap modern-cpp-font-lock modern-sh nhexl-mode niceify-info nim-mode nix-buffer nix-env-install nix-sandbox nix-update nixpkgs-fmt ob-coffeescript ob-rust org org-agenda-property org-beautify-theme org-edna org-pretty-tags org-radiobutton org-random-todo org-randomnote org-sync org-table-comment org-transform-tree-table org-treeusage orgnav origami-predef ox-gfm pabbrev pacmacs paredit poly-R polymode preproc-font-lock pretty-sha-path projectile propfont-mixed proportional python-info racer rainbow-delimiters robe sass-mode scrooge shm shut-up simple-call-tree skewer-mode slime-company smart-compile smart-tab smartrep sml-mode solarized-theme sourcemap speech-tagger strace-mode sysctl thrift toml-mode udev-mode unicode-fonts unicode-progress-reporter unicode-whitespace use-ttf vimrc-mode web-beautify xterm-color)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
