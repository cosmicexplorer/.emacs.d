;;; stuff from this one guy's .emacs i'm in the process of stealing

;; (setq load-path (cons (expand-file-name "~/lib/lisp") load-path))
;; (autoload 'c++-mode "cc-mode" "C++ Editing Mode" t)
;; (autoload 'c-mode   "cc-mode" "C Editing Mode" t)
;; (setq auto-mode-alist
;; (append '(("\.[Chily]$" . c++-mode)
;; ("\.html$" . html-helper-mode)
;; ("\.cc$" . c++-mode)
;; ("\.cpp$" . c++-mode)
;; ("\.inl$" . c++-mode)
;; ("\.idl$" . c++-mode)
;; ("\.c$" . c-mode)   ; to edit C code
;; )auto-mode-alist))
;; (setq auto-mode-alist (cons '("\.Z$" . compress-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\.gz$" . zip-mode) auto-mode-alist))
;; ;;(setq auto-mode-alist (cons '("\.[cly]$" . load-c-mode) auto-mode-alist))
;;
;; ;(setq mh-progs "/home/cs/faculty/schmidt/bin/SunOS5"
;; ;      mh-lib "/home/cs/faculty/schmidt/lib/SunOS5"
;; ;(setq mh-progs "/pkg/mh/bin"
;; ;      mh-lib "/pkg/mh/lib"
;; ;      mh-user-path "~/personal/mhbox/"
;; ;      mh-draft-folder "drafter"
;; ;      mh-use-mhl t
;; ;      mhl-formfile t
;; ;      mail-default-reply-to "schmidt@cse.wustl.edu")
;; ;(fset 'rmail 'mh-rmail)
;; ;(fset 'smail 'mh-smail)
;;
;; ;; Load the sendmail.el library which contains definitions of mail-text
;; ;;
;; (require 'sendmail)
;;
;; ;; Utility function to create a From field
;; ;;
;; (defun mail-from ()
;; "Move point to end of From-field.  Create a From field if none."
;; (interactive)
;; (expand-abbrev)
;; (or (mail-position-on-field "From" t)
;; (progn
;; (goto-char (point-min))
;; (insert "From: " user-full-name " <"user-mail-address">n")
;; (mail-text))))
;;
;; ;; Add mail-from so that it gets called whenever a message is composed
;; ;;
;; (add-hook 'mh-letter-mode-hook 'mail-from)
;; (require 'mh-e)
;; (define-key mh-folder-mode-map "C-d" (lambda(range)
;; (interactive
;; (list (mh-interactive-range "Delete")))
;; (mh-pipe-msg "sa-learn --spam " t)
;; (mh-iterate-on-range () range
;; (mh-delete-a-msg nil))
;; (if (looking-at
;; mh-scan-deleted-msg-regexp)
;; (mh-next-msg))))
;; ;; MH-E setup
;;
;; (defun compress-mode ()
;; "Deal with .Z compressed files"
;; (interactive)
;; (shell-command-on-region (point-min) (point-max) "compress -d" t)
;; (goto-char 1)
;; (not-modified)
;; )
;;
;; (defun zip-mode ()
;; "Deal with .gz compressed files"
;; (interactive)
;; (shell-command-on-region (point-min) (point-max) "gzip -d" t)
;; (goto-char 1)
;; (not-modified)
;; )
;;
;; (defun scroll-down-one (lines) "scrolls the screen down one line, point remains
;; in same location"
;; (interactive "p")
;; (scroll-down lines))
;;
;; (defun scroll-up-one (lines) "scrolls the screen up one line, point remains
;; in same location"
;; (interactive "p")
;; (scroll-up lines))
;;
;; (defun print-current-buffer () "prints current buffer to the Imagen printer"
;; (interactive)
;; (shell-command-on-region (point-min) (point-max)
;; (concat "lpr -Plp " (buffer-name)) nil)
;; (message (concat "File " (buffer-name) " is currently imprinting , sire!")))
;;
;; (defun toggle-overlay-mode () "turns on overwrite mode, turns off abbrevs and
;; vice-versa"
;; (interactive)
;; (cond
;; ((eq overwrite-mode t)
;; (overwrite-mode (- 1))
;; (abbrev-mode (+ 1))
;; (line-status))
;; ((eq overwrite-mode nil)
;; (overwrite-mode (+ 1))
;; (abbrev-mode (- 1))
;; (line-status))))
;;
;; ;(defun proc-up-shell () "starts me up with the C-shell running in GNU"
;; ;   (get-buffer-create (concat "*" "shell" "*"))
;; ;   (shell))
;;
;; ;(defun switch-to-shell-buffer () "moves from current buffer to shell buffer"
;; ;   (interactive)
;; ;   (switch-to-buffer "*shell*"))
;;
;; (defun static-cursor-insert-space () "inserts a space, but does not move cursor"
;; (interactive)
;; (insert-string " ")
;; (forward-char (- 1)))
;;
;; (defun kill-eol-rejoin-lines () "Kills end of line and then appends
;; previous line to end of current line"
;; (interactive)
;; (kill-line)
;; (just-one-space))
;;

;; (global-set-key "e["      'backward-sexp)
;; (global-set-key "b"       'delete-backward-char)
;; (global-set-key "eb"     'backward-kill-word)
;; (global-set-key "e;"      'backward-to-word)
;; (global-set-key "e,"      'beginning-of-window)
;; (global-set-key "er"      'call-last-kbd-macro)
;; (global-set-key "ej"      'carriage-return-newline)
;; (global-set-key "C-x,"    'compile)
;; (global-set-key "C-x/"    'point-to-register)
;; (global-set-key "C-xj"    'register-to-point)
;; (global-set-key "n"       'cr-indent-relative)
;; (global-set-key "C-xC-b" 'electric-buffer-list)
;; (global-set-key "C-xC-h" 'electric-command-history)
;; (global-set-key "e."      'end-of-window)
;; (global-set-key "e?"      'find-tag)
;; (global-set-key "e]"      'forward-sexp)
;; (global-set-key "e'"      'forward-to-word)
;; (global-set-key "eg"      'goto-line)
;; (global-set-key "eh"      'help-command)
;; (global-set-key "ei"      'indent-point-relative)
;; (global-set-key "eC-b"   'insert-buffer)
;; (global-set-key "C-r"     'isearch-backward-regexp)
;; (global-set-key "C-s"     'isearch-forward-regexp)
;; (global-set-key "e$"      'ispell-word)
;; (global-set-key "ee"     'keyboard-quit)
;; (global-set-key "e^"      'kill-eol-rejoin-lines)
;; (global-set-key "eC-d"   'kill-to-eof)
;; (global-set-key "eC-t"   'kill-to-filestart)
;; (global-set-key "eC-u"   'line-to-top-of-screen)
;; (global-set-key "C-xl"   'load-file)
;; (global-set-key "C-xr"    'mh-rmail)
;; (global-set-key "C-xm"    'mh-smail)
;; (global-set-key "C-e"     'move-to-eoln)
;; (global-set-key "r"       'newline-indent)
;; (global-set-key "C-xb"    'previous-buffer)
;; (global-set-key "C-xa"    'mml-attach-file)
;; (global-set-key "C-xn"    'next-buffer)
;; (global-set-key "eC-q"   'quoted-insert)
;; (global-set-key "C-xf"    'save-all-buffers-then-exit)
;; (global-set-key "en"      'scroll-down-one)
;; (global-set-key "C-x}"    'scroll-left)
;; (global-set-key "C-x{"    'scroll-right)
;; (global-set-key "ep"      'scroll-up-one)
;; (global-set-key "C-q"     'static-cursor-insert-space)
;; (global-set-key "C-xv"    'switch-to-buffer-other-window)
;; (global-set-key "C-xc"    'switch-to-buffer)
;; (global-set-key "C-xt"    'tags-search)
;; (global-set-key "eC-a"   'toggle-auto-fill-mode)
;; (global-set-key "eC-c"   'toggle-c-mode)
;; (global-set-key "eo"      'toggle-overlay-mode)
;; (global-set-key "ez"      'tags-loop-continue)
;; (global-set-key "C-x="    'what-line)
;; (global-set-key "e="      'what-cursor-position)
;; (global-set-key "C-xC-f" 'wild-find-file)
;; (global-set-key "C-xC-v" 'wild-find-file-other-window)
;; (global-set-key "C-xC-y" 'strip-and-save)
;;
;; (setq line-number-mode t)
;; (setq enable-local-variables t)
;; (setq auto-fill-default t) ;; make new buffers be in auto fill mode by default


;;
;; (defun save-modified-buffers () "saves all modified file buffers w/o query"
;; (interactive)
;; (save-some-buffers t))
;;
;; (defun save-all-buffers-then-exit () "saves buffers without question and exits"
;; (interactive)
;; (save-modified-buffers)
;; (kill-emacs))
;;
;; (defun line-to-top-of-screen () "moves line to top of screen"
;; (interactive)
;; (recenter 0))
;;
;; (defun open-line (arg)
;; "Insert a newline and leave point before it.
;; With arg, inserts that many newlines."
;; (interactive "*p")
;; (let ((flag (and (bolp) (not (bobp)))))
;; (if flag (forward-char -1))
;; (while (> arg 0)
;; (save-excursion
;; (insert ?n)
;; (if fill-prefix (insert fill-prefix)))
;; (setq arg (1- arg)))
;; (if flag (forward-char 1))))
;;
;; (defun move-to-eoln () "Moves to end of text on line"
;; (interactive)
;; (end-of-line)
;; (delete-horizontal-space))
;;
;;
;; (defun kill-backwards-line () "Kills the entire line backwards"
;; (interactive)
;; (beginning-of-line)
;; (kill-line))
;;
;; (defun newline-indent ()
;; (interactive)
;; (next-line (+ 1))
;; (back-to-indentation))
;;
;; (defun carriage-return-newline () "performs the function formerly reserved
;; for newline"
;; (interactive)
;; (insert "n"))
;;
;; (defun cr-indent-relative () "Indents at beginning column location of
;; previous line"
;; (interactive)
;; (point-to-register 106)
;; (back-to-indentation)
;; (setq opoint (current-column))
;; (register-to-point 106)
;; (insert "n")
;; (indent-to opoint))
;;
;; (defun indent-point-relative () "Indents directly under beginning of previous
;; line"
;; (interactive)
;; (delete-horizontal-space)
;; (indent-relative))
;;
;; (defun back-space-tab () "Provides a nice back-tabbing capability, does NOT
;; alter text on line."
;; (interactive)
;; (forward-char (- 3)))
;;
;; (defun forward-tab () "Tabs forward three spaces without moving text, only
;; point"
;; (interactive)
;; (forward-char (+ 3)))
;;
;; (defun mark-beginning-of-buffer ()
;; "Set mark at the beginning of the buffer."
;; (interactive)
;; (push-mark (point-min)))
;;
;; (defun mark-end-of-buffer ()
;; "Set mark at the end of the buffer."
;; (interactive)
;; (push-mark (point-max)))
;;
;; (defun kill-to-eof () "Kills buffer to the end of file"
;; (interactive)
;; (mark-end-of-buffer)
;; (kill-region (mark) (point)))
;;
;; (defun kill-to-filestart () "Kills buffer to the start of file"
;; (interactive)
;; (mark-beginning-of-buffer)
;; (kill-region (mark) (point)))
;;
;; (defun beginning-of-window () "Put dot at the beginning of this window"
;; (interactive)
;; (goto-char (window-start)))
;;
;; (defun end-of-window () "Put dot at the end of this window"
;; (interactive)
;; (goto-char (window-start))
;; (vertical-motion (- (window-height) 2))
;; (end-of-line))
;;
;; (defun line-status () "prints neat info about current line"
;; (save-excursion (set-buffer (other-buffer))))
;;
;; (defun toggle-abbrev-mode () "Turns ON abbrevs if currently OFF and vice versa"
;; (interactive)
;; (cond
;; ((eq abbrev-mode t)
;; (abbrev-mode (- 1))
;; (line-status))
;; ((eq abbrev-mode nil)
;; (abbrev-mode (+ 1))
;; (line-status))))
;;
;; (defun toggle-auto-fill-mode () "Turns ON auto-fill if currently OFF and vice versa"
;; (interactive)
;; (auto-fill-mode nil)
;; (line-status))
;;
;; (defun toggle-fundamental-mode () "Toggle the fundamental mode"
;; (interactive)
;; (fundamental-mode)
;; (line-status))
;;
;; ; stuff I only use with X (hilit19 mode and colored paren mode)
;; (cond
;; ((eq window-system 'x)
;; (defvar hilit-background-mode 'light
;; "'mono inhibits color, 'dark or 'light is the background brightness")
;; (load-library "hilit19")
;; (load-library "paren"))
;; )
;;
;; ;(defun toggle-c-mode () "Toggle my C mode"
;; ;  (interactive)
;; ;  (load-file "~/.c-mode.el")
;; ;  (c-mode)
;; ;  (line-status))
;;
;; (defun forward-to-word (arg)
;; "Move forward until encountering the beginning of a word.
;; With argument, do this that many times."
;; (interactive "p")
;; (or (re-search-forward (if (> arg 0) "\W" "\W") nil t arg)
;; (goto-char (if (> arg 0) (point-max) (point-min)))))
;;
;; (defun skip-forward-identifier () "moves point to end of identifier"
;; (interactive)
;; (forward-word (+ 1))
;; (skip-chars-forward "._a-zA-Z0-9-"))
;;
;; (defun skip-backward-identifier () "moves point to end of identifier"
;; (interactive)
;; (forward-word (- 1))
;; (skip-chars-backward "._a-zA-Z0-9-"))
;;
;;
;; (defun backward-to-word (arg)
;; "Move backward until encountering the end of a word.
;; With argument, do this that many times."
;; (interactive "p")
;; (forward-to-word (- arg)))
;;
;;
;; ;(defun load-c-mode () "load my own C mode!"
;; ;  (load-file "~/.c-mode.el")
;; ;  (c-mode)
;; ;)
;;
;; ;----------------------------------------------------------------------
;;
;; ;(defun load-ada-mode () "load the ada mode abbrevs"
;; ;   (load-file "~/.ada.el")
;; ;   (ada-mode)
;; ;  (setq comment-start "--( ")
;; ;  (setq comment-end " )")
;; ;  (setq comment-column 45)
;; ;  (setq comment-start-skip "--( ")
;; ;   (setq abbrev-file-name "~/.ada_defs")
;; ;   (quietly-read-abbrev-file "~/.ada_defs")
;; ;   (abbrev-mode 1))
;;
;; ;(defun load-pascal-mode () "load the pascal mode abbrevs 'n' stuff"
;; ;   (load-file "~/.pascal.el")
;; ;   (pascal-mode)
;; ;   (setq abbrev-file-name "~/.pascal_defs")
;; ;   (quietly-read-abbrev-file "~/.pascal_defs")
;; ;   (abbrev-mode 1))
;;
;; ;(defun mousetags ()
;; ;  (interactive)
;; ;  (load-file "~/.mouse-tags.el")
;; ;  (visit-tags-table "./TAGS")
;; ;  (message "Mouse tags loaded")) ;; type c-x c-e before the ;; here
;;
;; ;----------------------------------------
;; ; Crazy stuff that Owen gave me!
;; ;----------------------------------------
;;
;; (defun buffer-acceptable (buffer)
;; ; Decide whether a buffer is acceptable
;; (not (null (buffer-file-name buffer))))
;;
;; (defun select-ok-buffer (buff-list)
;; ; Select the first acceptable buffer from a list
;; (if (null buff-list)
;; nil
;; (if (buffer-acceptable (car buff-list))
;; (car buff-list)
;; (select-ok-buffer (cdr buff-list)))))
;;
;; (defun next-buffer ()
;; ; Function to switch to the next buffer in the buffer list
;; (interactive)
;; (let ((new-buf (select-ok-buffer (reverse (buffer-list)))))
;; (if new-buf
;; (switch-to-buffer new-buf))))
;;
;; (defun previous-buffer ()
;; ; Function to switch to the previous buffer in the buffer list
;; (interactive)
;; (let ((new-buf (select-ok-buffer (cdr (buffer-list)))))
;; (if new-buf
;; (progn
;; (bury-buffer (current-buffer))
;; (switch-to-buffer new-buf)))))
;;
;; (defun wild-to-regexp (wild-pattern)
;; ; Convert a wildcard pattern with * to a regexp
;; (let ((result (regexp-quote wild-pattern))
;; (next-posn nil))
;;
;; ; Convert each "*" to ".*"
;; (while (setq next-posn (string-match "\\*" result))
;; (setq result (concat (substring result 0 next-posn)
;; ".*"
;; (substring result (+ next-posn 2)
;; (length result)))))
;;
;; ; Make sure we only match full expressions
;; (concat "^" result "$")))
;;
;; (defun wild-find-file (filename)
;; ; Read a set of files from a wildcard specification
;; (interactive "FWildcard find file: ")
;; (let* ((dir       (file-name-directory filename))
;; (pattern   (wild-to-regexp (file-name-nondirectory filename)))
;; (file-list (directory-files dir t pattern)))
;; (if (null file-list)
;; (find-file filename)
;; (mapcar 'find-file file-list))))
;;
;; (defun wild-find-file-other-window (filename)
;; ; Read a set of files from a wildcard specification
;; (interactive "FWildcard find file in other window: ")
;; (let* ((dir       (file-name-directory filename))
;; (pattern   (wild-to-regexp (file-name-nondirectory filename)))
;; (file-list (directory-files dir t pattern)))
;; (if (null file-list)
;; (find-file-other-window filename)
;; (mapcar 'find-file-other-window file-list))))
;;
;; ;;;;;;;;
;; ;;;;;;;; Save a file, after untabify'ing and stripping trailing spaces.
;; ;;;;;;;;
;; (defun strip-and-save ()
;; "untabify, strip trailing spaces, and save file"
;; (interactive)
;; (untabify (point-min) (point-max))
;; (goto-char (point-min))
;; (while (re-search-forward " +$" nil t)
;; (replace-match "" nil nil))
;; (goto-char (point-min))
;; (save-buffer))
;;
;; ; ChangeLog mode
;; (add-hook 'change-log-mode-hook
;; (function (lambda ()
;; (setq fill-prefix "  "))))
;; ;                    two spaces here: -> ^^
;; (add-hook 'change-log-mode-hook 'auto-fill-mode)
;;
;; ;---------------------------------------------------------------------------
;; ; Things to do everytime we start up!
;; ;---------------------------------------------------------------------------
;;

(message "dr schmidt is in the house")
