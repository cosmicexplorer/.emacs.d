;;; note that the ~ expansion to home directory does not work when sudo/chrooted and absolute paths are required
;;; i have changed quite a few files besides just this one and if you wish to upgrade them
;;; you'll have to re-add those changes for the whole frail system to work
;;; alternatively you can just email me that an update occurred in some package and i'll add it and push the changes

;;;;; specific sections are demarcated by five semicolons, like this line
;;; do a global search through all such marks to go through all major sections

;;; TODO: add ghc-mode if i ever get into haskell

;;;;; globally useful things
;; stop the intro to emacs buffer
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(menu-bar-mode -1) ;; remove menu bar for another line of space
(setq-default indent-tabs-mode nil)	;; fix indentation issues
;; fix selection issues in xterm (can't hold down shift and up arrow to highlight stuff)
(if (equal "xterm" (tty-type))
		(define-key input-decode-map "\e[1;2A" [S-up]))

;; starts emacs in server form so i can use emacsclient to add files
;; but only if server not already started
(require 'server)
(if (and (fboundp 'server-running-p)
				 (not (server-running-p)))
    (server-start))

;; important for many things
(require 'cl)

;;; so i can sudo edit files with C-x C-f /sudo::/path/to/file
(require 'tramp)

;;; have normal delete/selection
(delete-selection-mode 1)

;; do backups well and put them into a separate folder
(setq backup-directory-alist `(("." . "~/.emacs.d/autosaved-files")))
(setq backup-by-copying t)
(setq delete-old-versions t
			kept-new-versions 6
			kept-old-versions 2
			version-control t)
;; do the same thing for undo-tree history
(setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo-tree-history")))

;;;;; setup specific modes for specific filetypes
;; setup slime
;; setup load-path and autoloads
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)
;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))
(add-to-list 'slime-contribs 'slime-repl)
(load (expand-file-name "~/.emacs.d/quicklisp/slime-helper.el")) ;; add quicklisp!
(add-hook 'emacs-lisp-mode-hook 'fix-lisp-keybindings)
(add-hook 'lisp-mode-hook 'slime-mode)
(add-hook 'lisp-mode-hook 'fix-lisp-keybindings)

;; paredit
(add-to-list 'load-path "~/.emacs.d/paredit")
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
;; create parens and add adjacent two elements to sexp created by parens
(defun paredit-create-sexp-slurp-both-sides ()
  (interactive)
  (paredit-open-parenthesis) ; adds closing paren too thanks to electric pair (or maybe it's paredit itself)
  (paredit-forward-slurp-sexp)
  (paredit-backward-slurp-sexp))        ; front and back added

;; haskell mode
(add-to-list 'load-path "~/.emacs.d/haskell-mode/")
(require 'haskell-mode-autoloads)
(add-to-list 'Info-default-directory-list "~/.emacs.d/haskell-mode/")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;; random per-language editing things
;; format comments like a normal person
(add-hook 'c-mode-hook (lambda () (setq comment-start "// " comment-end   "")))
(add-hook 'r-mode-hook (lambda () (setq comment-start "## " comment-end   "")))
(add-hook 'lisp-mode-hook (lambda () (setq comment-start ";; " comment-end "")))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq comment-start ";; " comment-end "")))
(add-hook 'cmake-mode-hook (lambda () (setq comment-start "# " comment-end "")))
(setq c-hanging-semi&comma-criteria nil) ; stop inserting newlines after semicolons i don't like them
(subword-mode)													 ; turn camel-case on
(setq auto-mode-alist                ; use python-mode for scons files
			(cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
			(cons '("SConscript" . python-mode) auto-mode-alist))

;;;;; load utilities
;; load w3m web browser
(add-to-list 'load-path "~/.emacs.d/w3m")
(require 'w3m-load)
(setq w3m-use-cookies t)
(setq w3m-coding-system 'utf-8
			w3m-file-coding-system 'utf-8
			w3m-file-name-coding-system 'utf-8
			w3m-input-coding-system 'utf-8
			w3m-output-coding-system 'utf-8
			w3m-terminal-coding-system 'utf-8)

(hs-minor-mode) ;; C-c @ C-c for folding up code blocks!!!
(add-hook 'prog-mode-hook #'hs-minor-mode)

(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
(require 'multiple-cursors)

;; for like real scrolling
(xterm-mouse-mode)
;; for interacting with clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; add helm stuff
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(helm-mode t)
;; add pcomplete with helm support to eshell
(add-hook 'eshell-mode-hook
					#'(lambda ()
							(define-key eshell-mode-map
								[remap eshell-pcomplete]
								'helm-esh-pcomplete)))

;; autoload eshell at start so helm plays nice (this doesn't affect load time at all i've checked)
(add-hook 'emacs-startup-hook #'(lambda ()
																	(let ((default-directory (getenv "HOME")))
																		(command-execute 'eshell)
																		(bury-buffer))))
(add-to-list 'load-path "~/.emacs.d/helm-swoop")
(require 'helm-swoop)

;; adds appropriate areas to load path
;; in this case for undo-tree, smart-tab, revbufs, others
(add-to-list 'load-path "~/.emacs.d/lisp")
;; allow for save buffer reversion when files are being edited by external tools
(require 'revbufs)

(autoload 'markdown-mode "markdown-mode"
	"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; adds smart-compile functionality
(require 'smart-compile)

;; sets term to zsh
(setq shell-file-name "zsh")
(setq explicit-shell-file-name "zsh")

;; adds evil vim layer, but doesn't turn it on
(add-to-list 'load-path "~/.emacs.d/evil/")
(require 'evil)

;; php-mode
(add-to-list 'load-path "~/.emacs.d/php-mode")
(require 'php-mode)
;; go-mode
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode)
;; dired-xattr
(add-to-list 'load-path "~/.emacs.d/dired-xattr")
(require 'dired-xattr)
;; yasnippet
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)

;; adds undo-tree functionality
(require 'undo-tree)
(global-undo-tree-mode)
;; persist across file saves
(setq undo-tree-auto-save-history t)
;; show diffs in undo tree visualizer
(setq undo-tree-visualizer-diff t)

;; adds smart-tab (tab completion) functionality
(require 'smart-tab)
(global-smart-tab-mode 1)

;; adds dired-x functionality
(add-hook 'dired-load-hook
					(lambda ()
						(load "dired-x")))

;; adds julia functionality
(load "~/.emacs.d/ESS/lisp/ess-site")
(setq inferior-julia-program-name "/usr/bin/julia-basic")
(add-to-list 'ess-tracebug-search-path "/usr/share/julia/base/")
(ess-toggle-underscore nil)	;; underscore means underscore

;; adds haskell functionality
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; show line numbers
(global-linum-mode)
;; make them relative
(add-to-list 'load-path "~/.emacs.d/linum-relative")
(require 'linum-relative)
(setq linum-format 'linum-relative)

;; add magit
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/magit")
(eval-after-load 'info
	'(progn (info-initialize)
					(add-to-list 'Info-directory-list "/path/to/magit/")))
(require 'magit)

;; parenthesis matching and more
;; turn pair parens on
(electric-pair-mode t)
(add-to-list 'electric-pair-pairs '(?\{ . ?\}))
;; match parens when cursor on top
(show-paren-mode t)
;;; rainbow delimiters!
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; ;; tab correctly
;; (defun generate-tab-stops (&optional width max)
;;   "Return a sequence suitable for `tab-stop-list'."
;;   (let* ((max-column (or max 200))
;; 				 (tab-width (or width tab-width))
;; 				 (count (/ max-column tab-width)))
;;     (number-sequence tab-width (* tab-width count) tab-width)))

;; (setq-default tab-width 2)
;; (setq-default tab-stop-list (generate-tab-stops))

;; set for different modes (usually done in file itself)
;; (setq-default c-basic-offset 2)

;;; allow for ido usage for better C-x b buffer search n stuff
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)				; makes searching fuzzier

;; when opening a file the cursor will be at the last saved position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;; qmake-mode
(load "~/.emacs.d/lisp/qmake.el")

;;;;; ibuffer stuff
;;;; re: http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
;; you can add different groups too, not just home, in case you ever want to (lol)
(setq ibuffer-saved-filter-groups
      '(("home"
				 ("emacs-config" (or (filename . ".emacs.d")
														 (filename . "emacs-config")
														 (filename . ".emacs")))
				 ("doc-view" (mode . doc-view-mode))
				 ("web" (or (mode . html-mode)
										(mode . css-mode)
										(mode . html-helper-mode)
										(mode . nxhtml-mode)
										(mode . php-mode)))
				 ("subversion" (name . "\*svn"))
				 ("magit" (name . "\*magit\*"))
				 ("git" (name . "\*git"))
				 ("helm" (or (name . "*helm\*")
										 (name . "*Helm\*")))
				 ("help" (or (name . "\*Help\*")
										 (name . "\*help\*")
										 (name . "\*Apropos\*")
										 (name . "\*apropos\*")
										 (name . "\*Info\*")
										 (name . "\*info\*")
										 (name . "\*doc\*")))
				 ("makefile" (or (filename . "\\Makefile\\'")
												 (filename . "\\makefile\\'")))
				 ("readme" (or (filename . "\\README\\'")
											 (filename . "\\readme\\'")))
				 ("dired" (mode . dired-mode))
				 ("julia" (filename . "\\.jl\\'")) ;; because just detecting julia-mode doesn't work fsr
				 ("r" (or (filename . "\\.R\\'")
									(filename . "\\.r\\'")))
				 ("cmake" (mode . cmake-mode))
				 ("text" (filename . "\\.txt\\'"))
				 ("c header" (filename . "\\.h\\'"))
				 ("c" (mode . c-mode))
				 ("c++" (mode . c++-mode))
				 ("java" (mode . java-mode))
				 ("python" (mode . python-mode))
				 ("markdown" (mode . markdown-mode))
				 ("emacs-lisp" (mode . emacs-lisp-mode)) ;; emacs-config filter mostly blocks this but it's whatever
				 ("lisp" (mode . lisp-mode))
				 ("go" (mode . go-mode))
				 ("javascript" (mode . js-mode))
				 ("perl" (mode . perl-mode))
				 ("haskell" (mode . haskell-mode))
				 ("fortran" (mode . fortran-mode))
				 ("ada" (mode . ada-mode))
				 ("ruby" (mode . ruby-mode))
				 ("hex" (mode . hexl-mode))
				 ("qmake" (mode . qmake-mode))
				 ("emacs-system" (name . "*\**")) ;; down here cause it was taking my php files fsr
				 )
				)
      )

(add-hook 'ibuffer-mode-hook
					'(lambda ()
						 (ibuffer-auto-mode t) ;; automatically updates buffer list
						 (ibuffer-switch-to-saved-filter-groups "home")))

(setq ibuffer-expert t) ;; only prompt when modified buffer is killed
(setq ibuffer-show-empty-filter-groups nil) ;; only show full filter groups

;;;;; keybindings
;; FIND CURRENT KEYBINDINGS WITH C-h b !!!!!
;; use C-h k to look at the current key sequence being entered!
;; this is useful when creating new keybindings

;; since eval-expression uses just the alt key but unity sucks
;; ESC-: DOES THIS ALREADY

;; the below can also be applied over multiple lines with C-u [number] M-x helm-swoop RET
(global-set-key (kbd "C-x o") 'helm-swoop)				; find regexp in file, more interactively than above
(global-set-key (kbd "C-x f") 'helm-multi-swoop-all) ; find regexp in ALL open buffers
(global-set-key (kbd "C-x j") 'helm-multi-swoop)		 ; find regexp is SOME open buffers
(global-set-key (kbd "C-x b") 'helm-buffers-list) ; find among open buffers

;; after killing C-x o with helm,
;; let's make sure we do have buffer switching in the event of non-graphical terminal-only editing
(global-set-key (kbd "C-x /") 'other-window)

;;; split-window management
;; open and close
(global-set-key (kbd "C-x <down>") 'split-window-below)
(global-set-key (kbd "C-x <right>") 'split-window-right)
(global-set-key (kbd "C-x C-<down>") 'split-window-below)
(global-set-key (kbd "C-x C-<right>") 'split-window-right)
(global-set-key (kbd "C-x e") 'delete-other-windows) ;; "expand"
(global-set-key (kbd "C-x p") 'delete-window) ;; "poof"
;; adjusting pane size
(global-set-key (kbd "C-x <home>") 'enlarge-window) ;; increase window height
(global-set-key (kbd "C-x <end>") 'shrink-window) ;; decrease window height
(global-set-key (kbd "C-x <prior>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <next>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x RET") 'shrink-window-if-larger-than-buffer) ;; shrink window to fit content
(global-set-key (kbd "C-x !") 'balance-windows) ;; make all windows same height
;;; move among panes in a way that isn't totally fucked
(setq windmove-wrap-around t)
;; original meta
(global-set-key (kbd "C-M-<left>")	'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>")	'windmove-up)
(global-set-key (kbd "C-M-<down>")	'windmove-down)
;; terminal in ubuntu doesn't allow above keybindings; probably because M-<left> and M-<right> switch tty
;; which is cool i guess??? very annoying though
(global-set-key (kbd "C-c <left>")	'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")	'windmove-up)
(global-set-key (kbd "C-c <down>")	'windmove-down)

;; visualize undo-tree
(global-set-key (kbd "C-x t") 'undo-tree-visualize)

;; use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill current buffer and close pane
(global-set-key (kbd "C-x C-k") 'close-and-kill-this-pane)

;; smart-compile stuff!!!!
(global-set-key (kbd "C-c C-k") 'smart-compile)

;; translate stuff into hex (and back??)
(global-set-key (kbd "C-x h") 'hexl-mode)
;; add back select all functionality since that was previously bound to C-x h
(global-set-key (kbd "C-x a") 'mark-whole-buffer)

;; find file somewhere below given directory with dired
(global-set-key (kbd "C-x C-r") 'find-name-dired) ; "recursive"

;; make C-z undo instead of FUCKING UP MY ENTIRE LIFE by suspending
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)

;; kill all active dired buffers at once
(global-set-key (kbd "C-x M-d") 'kill-dired-buffers)

;; open new file with given filename from minibuffer, or blank filename (cross your fingers)
(global-set-key (kbd "C-x C-n") 'open-new-file)

;; go to normal mode; i.e. quickly format everything pretty
(global-set-key (kbd "C-x n") 'normal-mode)

;; bind the key that doesn't do dired but should go to dired
(global-set-key (kbd "C-x C-d") 'dired)

;; multiple cursors fun!!!
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-n") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; gofmt!!!
(add-hook 'go-mode-hook
					(lambda () (local-set-key (kbd "C-c f") 'go-fmt-file)))

;; add forward regexp search cause it's pretty useful!
(global-set-key (kbd "C-x M-r") 'search-forward-regexp)
;; don't forget regexp-builder to see interactively how many hits the regexp is getting!

;; indent all lines in a file in case copy/pasting screws up somehow
(global-set-key (kbd "C-c i") 'iwb)

;; remember that M-= gets word counts!

;; search all open buffers for regexp
(global-set-key (kbd "C-c M-r") 'search-all-buffers)

;; make <backtab> force tab
(global-set-key (kbd "<backtab>") 'force-insert-tab)
(global-set-key (kbd "C-c t") 'force-insert-tab) ; for modes like markdown-mode where S-tab overridden

;; open file with wildcards
;;(global-set-key (kbd "C-c o") 'open-file-with-wildcards)
;; doesn't work lol

;; toggle letter casing from ALLCAPS to InitialCase to alllowercase
(global-set-key (kbd "C-x M-c") 'toggle-letter-case)

;;;;; my own functions! used throughout this file
;;; some of these are mine, some are heavily adapated from emacswiki, some are copy/paste from emacswiki
;;; if you wrote something and want me to put your name by it (which would be hilarious cause that means
;;; someone else is actually using this file) email me and i'll add accreditation immediately

;; allow for backtab to force '\t'
(defun force-insert-tab ()
	"insert tab character"
	(interactive)
	(insert "\t"))

;; from http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
	"Refreshes all open buffers from their respective files."
	(interactive)
	(dolist (buf (buffer-list))
		(with-current-buffer buf
			(when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
				(revert-buffer t t t) )))
	(message "Refreshed open files.") )

(defcustom search-all-buffers-ignored-files (list (rx-to-string '(and bos (or ".bash_history" "TAGS") eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(require 'grep)
(defun search-all-buffers (regexp prefix)
  "Searches file-visiting buffers for occurence of REGEXP.	With
prefix > 1 (i.e., if you type C-u \\[search-all-buffers]),
searches all buffers."
  (interactive (list (grep-read-regexp)
										 current-prefix-arg))
  (message "Regexp is %s; prefix is %s" regexp prefix)
  (multi-occur
   (if (member prefix '(4 (4)))
       (buffer-list)
     (remove-if
      (lambda (b) (some (lambda (rx) (string-match rx  (file-name-nondirectory (buffer-file-name b)))) search-all-buffers-ignored-files))
      (remove-if-not 'buffer-file-name (buffer-list))))
   regexp))

;; kill current buffer and close pane
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;; kill all active dired buffers at once
(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
					(when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
						(kill-buffer buffer)))
				(buffer-list)))

;; open new file (i wrote this!!!)
(defun open-new-file ()
  (interactive)
  (switch-to-buffer ;; switches to operating buffer
   (generate-new-buffer ;; creates new buffer with given name
    (generate-new-buffer-name
     (read-string "new filename: " ;; reads from minibuffer, with given default value
									nil nil "*newbuf*")))) ;; with default title *newbuf*
  (normal-mode))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; stolen from the ergo emacs guy (http://ergoemacs.org/emacs/modernization_upcase-word.html)
;; like normally everything he's put up is super underwhelming but this sees some use
(defun toggle-letter-case ()
	"Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
	(interactive)
	(let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
		(if (region-active-p)
				(setq p1 (region-beginning) p2 (region-end))
			(let ((bds (bounds-of-thing-at-point 'word)))
				(setq p1 (car bds) p2 (cdr bds))))
		(when (not (eq last-command this-command))
			(save-excursion
				(goto-char p1)
				(cond
				 ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
				 ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
				 ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
				 ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
				 ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
				 (t (put this-command 'state "all lower")))))
		(cond
		 ((string= "all lower" (get this-command 'state))
			(upcase-initials-region p1 p2) (put this-command 'state "init caps"))
		 ((string= "init caps" (get this-command 'state))
			(upcase-region p1 p2) (put this-command 'state "all caps"))
		 ((string= "all caps" (get this-command 'state))
			(downcase-region p1 p2) (put this-command 'state "all lower")))))

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
													 (when (and (not (buffer-modified-p)) my-mode-line-buffer-line-count)
														 (setq str (concat str "/" my-mode-line-buffer-line-count)))
													 str)))
								"  %p"
								(list 'column-number-mode "	 C%c")
								"  " mode-line-buffer-identification
								"  " mode-line-modes))

(defun my-mode-line-count-lines ()
  (setq integer-buffer-line-count (count-lines (point-min) (point-max)))
  (setq my-mode-line-buffer-line-count (int-to-string integer-buffer-line-count)))

(add-hook 'find-file-hook 'my-mode-line-count-lines)
(add-hook 'after-save-hook 'my-mode-line-count-lines)
(add-hook 'after-revert-hook 'my-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'my-mode-line-count-lines)

;; function to indent whole buffer
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(add-hook 'slime-mode-hook 'smart-tab-mode-off)
(add-hook 'slime-mode-hook 'fix-lisp-keybindings)
;; get useful keybindings for lisp editing
(defun fix-lisp-keybindings ()
  "Adds about three million personalized keybindings for lisp editing with SLIME and Paredit.
Not for the faint of heart."
	(interactive)
  (define-key slime-mode-map (kbd "TAB") 'slime-fuzzy-complete-symbol)
	(define-key paredit-mode-map (kbd "C-M-<left>") 'windmove-left)
	(define-key paredit-mode-map (kbd "C-M-<right>") 'windmove-right)
	(define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward) ; remove key here (slurp-forward)
  (define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward) ; remove key here (slurp-backward)
  (define-key paredit-mode-map (kbd "M-a") nil) ; kill this, it's a global but it's annoying and i don't use it
  (define-key paredit-mode-map (kbd "M-a M-a") 'paredit-create-sexp-slurp-both-sides)
  (global-set-key (kbd "RET") 'newline-and-indent) ; set as global because define-key doesn't work, not sure why
  (define-key paredit-mode-map (kbd "M-a M-<right>") 'paredit-forward-slurp-sexp)
  (define-key paredit-mode-map (kbd "M-a M-<left>") 'paredit-backward-slurp-sexp)
  (define-key paredit-mode-map (kbd "C-M-a C-M-<right>") 'paredit-forward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-M-a C-M-<left>") 'paredit-backward-barf-sexp)
  (define-key paredit-mode-map (kbd "C-x C-l") 'mc/edit-lines) ; so that multiple-cursors can use these
  (define-key paredit-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key paredit-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-M-n") 'mc/unmark-next-like-this)
  (define-key paredit-mode-map (kbd "C-M-p") 'mc/unmark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-c C-a") 'mc/mark-all-like-this)
  (define-key paredit-mode-map (kbd "DEL") 'backspace-delete-highlight-paredit))
(defun backspace-delete-highlight-paredit ()
  "Makes it so that backspace deletes all highlighted text in paredit mode.
Breaks the rules a little bit, but makes me a lot less insane."
  (interactive)
  (if (use-region-p)                    ; if a region is selected
      (delete-region (region-beginning) (region-end))
    (paredit-backward-delete)))

(eval-after-load "haskell-mode"
	'(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-compile))
(eval-after-load "haskell-mode"
	'(progn
		 (define-key haskell-mode-map (kbd "C-x C-d") nil)
		 (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
		 (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
		 (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
		 (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
		 (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
		 (define-key haskell-mode-map (kbd "C-c M-.") nil)
		 (define-key haskell-mode-map (kbd "C-c C-d") nil)))
;;; cause i can never figure out how to just get to the REPL lol
(defalias 'haskell-repl (symbol-function 'haskell-process-do-info))
