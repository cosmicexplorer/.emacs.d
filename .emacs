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
 '(helm-completion-style 'emacs))
