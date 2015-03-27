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
      `(("." . ,(concat init-home-folder-dir "autosaved-files"))))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; do the same thing for undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,(concat init-home-folder-dir "undo-tree-history"))))
(setq undo-tree-visualizer-timestamps t)

;; load w3m web browser
(when (executable-find "w3m")
  (require 'w3m)
  (setq w3m-use-cookies t)
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  (add-hook 'w3m-mode-hook
            '(lambda ()
               (w3m-turnoff-inline-images)
               (w3m-toggle-inline-images))))


;;; unfold org at startup
(setq-default org-startup-folded "showeverything")

;;; see docs for funcs n stuff
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; show line numbers
(global-linum-mode 1)
;; make them relative
(setq linum-format 'linum-relative)

;;; RAINBOW
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)

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
(defun output-lines-in-buffer ()
  (setq integer-buffer-line-count (count-lines (point-min) (point-max)))
  (setq my-mode-line-buffer-line-count (int-to-string
                                        integer-buffer-line-count)))

;;; convenience hooks
(add-hook 'find-file-hook 'output-lines-in-buffer)
(add-hook 'after-save-hook 'output-lines-in-buffer)
(add-hook 'after-revert-hook 'output-lines-in-buffer)
(add-hook 'dired-after-readin-hook 'output-lines-in-buffer)

;;; TODO: make this work
;; (add-hook 'buffer-list-update-hook
;;           #'(lambda ()
;;               (if (and (buffer-file-name)
;;                       (or               ; because regexes are parsed weirdly
;;                                         ; here and this works
;;                        (string-match "^.*\.pdf$" (buffer-file-name))
;;                        (string-match "^.*\.ps$" (buffer-file-name))
;;                        (string-match "^.*\.dvi$" (buffer-file-name))
;;                        (string-match "^.*\.doc.*$" (buffer-file-name))
;;                        (string-match "^.*\.ppt.*$" (buffer-file-name))
;;                        (string-match "^.*\.xls.*$" (buffer-file-name))
;;                        (string-match "^.*\.od.*$" (buffer-file-name)))
;;                       ;; not sure why this is required, but it is
;;                       (not
;;                        (or (string-match "^.*\.tex$" (buffer-file-name))
;;                            (string-match "^.*\.bib$" (buffer-file-name))
;;                            (string-match "^.*\.el$" (buffer-file-name))
;;                            (string-match "^.*\.emacs$" (buffer-file-name)))))
;;                   ;; doesn't work otherwise lol
;;                   (with-current-buffer (buffer-name)
;;                     (linum-mode 0))
;;                 (global-linum-mode 1))))

;;; misc
(load-display-time)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq auto-save-interval 600)     ; half as often as default
(setq gc-cons-threshold 2000000)
(setq lpr-switches '("-Pps"))
(setq text-mode-hook 'turn-on-auto-fill)
(setq default-truncate-lines nil)
(setq visible-bell t)
(setq require-final-newline t)
(setq version-control t)
(setq abbrev-all-caps t)
(put 'eval-expression 'disabled nil)
(autoload 'html-helper-mode "html-helper-mode" "HTML rules!" t)
(autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
(autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
(autoload 'ispell-region "ispell" "Check spelling of region" t)
(autoload 'ispell-buffer "ispell" "Check spelling of buffer" t)
;;; i'm not sure what these do
(setq trim-versions-without-asking t)
(setq mode-line-inverse-video t)

;;; setup syntax highlighting for keywords i care about
;;; this overrides comment highlighting now, thank god
(defvar warning-highlights-keywords
  `((,(regexp-opt '("TODO" "todo" "Todo"
                    "FIXME" "fixme" "Fixme"
                    "FIX" "fix" "Fix"
                    "DEPRECATED" "deprecated" "Deprecated"
                    "XXX" "xxx" "Xxx"
                    "HACK" "hack" "Hack"
                    "IFFY" "iffy" "Iffy"
                    "CHANGED" "changed" "Changed"
                    "OPTIMIZATION" "optimization" "Optimization"
                    "BROKEN" "broken" "Broken"))
     0 font-lock-warning-face t))
  "Keywords to apply extra highlights to.")
(defun warning-highlights-turn-on ()
  "Turn on warning-highlights-mode."
  (font-lock-add-keywords
   nil
   warning-highlights-keywords))
(defun warning-highlights-turn-off ()
  "Turn off warning-highlights-mode."
  (font-lock-remove-keywords
   nil
   `(,@warning-highlights-keywords)))
(define-minor-mode warning-highlights-mode
  "radical"
  :lighter " !!"
  (progn
    (if warning-highlights-mode
        (warning-highlights-turn-on)
      (warning-highlights-turn-off))
    (font-lock-mode 1)))
(add-hook 'prog-mode-hook #'warning-highlights-mode)
