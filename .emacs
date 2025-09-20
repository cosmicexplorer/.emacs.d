;;; -*- lexical-binding: t; -*-

(require 'cl-lib)


;;;;; (1) Define `defcustom' base groups for other init code to attach to.

(defgroup my-customizations nil "all `defcustom' forms in my own init scripts.")

(defgroup my-errors nil
  "`defcustom' group for error handling in my own emacs lisp code."
  :group 'my-customizations)

(define-error 'my-errors "Errors in my own emacs lisp code.")


;;;;; (4) Load init-scripts one by one, in the mysterious correct order.
;;;;;     TODO: rewrite all of `init-scripts/' as packages, and simply load then via `require'!

;;; Run `package-initialize' and configure `package-archives' for elpa and melpa.
(load (locate-user-emacs-file "init-scripts/packages.el"))

;;; Bring our own as well as installed m?elpa packages into scope.
;;; This adds `utils/', `integrations/' and `lisp/' to the `load-path'.
(load (locate-user-emacs-file "init-scripts/requires.el"))

;;; Some quick checks for compatibility between different operating environments.
(require 'local-compat)

;;; Configure package variables and `defadvice' some functions. Make sure that customizable settings
;;; go in `.emacs' or `danny-theme'!
(require 'package-setup)

;;; Configure everything I don't like about the emacs API and built-in packages.
(require 'interface)

;;; Configure hooks and workaround logic for specific languages.
(require 'languages)

;;; REBIND ALL THE THINGS!!!! EVERYTHING IS OFF THE DEFAULT!!!! MY KEYS ARE MINE!!!!
(require 'keybindings)

;;; Non-Custom configuration (and overrides) of how emacs displays things.
(require 'visuals)


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
(require 'functions)
(setup-submodules-load)

;;; Native-compile all the lisp in this directory!
(when (native-comp-available-p)
  (native-compile-async (locate-user-emacs-file "init-scripts") t nil)
  (native-compile-async (locate-user-emacs-file "utils") t nil)
  (native-compile-async (locate-user-emacs-file "integrations") t nil)
  (native-compile-async (locate-user-emacs-file "lisp") t nil))

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

 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
