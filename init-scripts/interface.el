;;; modifications to default ui

;;; prompts for (yes/no) -> (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;; stop the intro to emacs buffer
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;;; c-h a -> apropos
(define-key help-map "a" 'apropos)      ; get useful help for once

;;; remove toolbars
(menu-bar-mode 0)                       ;  remove menu bar for a line of space
(tool-bar-mode 0)                       ;  and tool bar for graphical

;;; configure scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(scroll-bar-mode 0)
(setq scroll-preserve-screen-position t)

;;; set font size and type
(set-face-attribute 'default nil :height 100)
(when (member "Telegrama" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Telegrama 10"))
  (set-face-attribute 'default t :font "Telegrama 10")
  (set-frame-font "Telegrama 10"))

;;; the mark is stupid as a ui concept even if it's great in scripts
(transient-mark-mode 0)
(setq shift-select-mode t)

;;; i don't want to have to guess which version of my file is the right one
(global-auto-revert-mode)

;; Remove trailing whitespace from a line
(setq-default nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; only show whitespace sometimes
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (unless (or (eq major-mode 'w3m-mode)
                         (eq major-mode 'eshell-mode)
                         (eq major-mode 'ibuffer-mode)
                         (eq major-mode 'undo-tree-visualizer-mode))
               (setq show-trailing-whitespace t))))

;;; have normal delete/selection (type over selected text to delete)
(delete-selection-mode 1)

;; do backups well and put them into a separate folder
(setq backup-directory-alist
      `(("." . (concat init-home-folder-dir "autosaved-files"))))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; do the same thing for undo-tree history
(setq undo-tree-history-directory-alist
      '((".*" . (concat init-home-folder-dir "undo-tree-history"))))
(setq undo-tree-visualizer-timestamps t)

                              (set-fill-column 80)))
