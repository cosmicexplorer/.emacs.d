;; NOTE THAT THE ~ EXPANSION TO HOME DIRECTORY DOES NOT WORK WHEN CHROOTED
;; AND ABSOLUTE PATHS ARE REQUIRED

;; start emacs up in a plain old terminal
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;; starts emacs in server form so i can use emacsclient to add files
;; but only if server not already started
(require 'server)
(if (and (fboundp 'server-running-p)
				 (not (server-running-p)))
    (server-start))

;; add multiple cursor stuff
(add-to-list 'load-path "~/.emacs.d/multiple-cursors.el")
(require 'multiple-cursors)

;; attempt to implement vim-like undo scrounged from evil-mode
;; (add-hook 'pre-command-hook #'evil-insert-repeat-hook)
;; (evil-start-undo-step t)
;; (remove-hook 'pre-command-hook #'evil-insert-repeat-hook)
;; (setq evil-insert-repeat-info evil-repeat-info)
;; (evil-set-marker ?^ nil t)
;; (evil-end-undo-step t)
;; (when evil-move-cursor-back
;; 	(evil-move-cursor-back))

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
;; fix sudo pcomplete
(defun pcomplete/sudo ()
	(let ((prec (pcomplete-arg 'last -1)))
		(cond ((string= "sudo" prec)
					 (while (pcomplete-here*
									 (funcall pcomplete-command-completion-function)
									 (pcomplete-arg 'last) t))))))
;; autoload eshell at start so helm plays nice
(add-hook 'emacs-startup-hook #'(lambda ()
																	(let ((default-directory (getenv "HOME")))
																		(command-execute 'eshell)
																		(bury-buffer))))
(add-to-list 'load-path "~/.emacs.d/helm-swoop")
(require 'helm-swoop)
	
;; add appropriate stuff to path
(setenv "PATH" (concat (getenv "PATH") ":/opt/Qt/5.3/gcc_64/bin")) ;; add qmake
(setq exec-path (append exec-path '("/opt/Qt/5.3/gcc_64/bin"))) ;; add qmake

;; add tags n stuff
;;  Jonas.Jarnestrom<at>ki.ericsson.se A smarter
;;  find-tag that automagically reruns etags when it cant find a
;;  requested item and then makes a new try to locate it.
;;  Fri Mar 15 09:52:14 2002    
(defadvice find-tag (around refresh-etags activate)
	"Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
	(let ((extension (file-name-extension (buffer-file-name))))
		(condition-case err
				ad-do-it
			(error (and (buffer-modified-p)
									(not (ding))
									(y-or-n-p "Buffer is modified, save it? ")
									(save-buffer))
						 (er-refresh-etags extension)
						 ad-do-it))))
(defun er-refresh-etags (&optional extension)
	"Run etags on all peer files in current dir and reload them silently."
	(interactive)
	(shell-command (format "etags *.%s" (or extension "el")))
	(let ((tags-revert-without-query t))  ; don't query, revert silently
		(visit-tags-table default-directory nil)))

;; cpplint
(eval-after-load 'flycheck
	'(progn
		 (require 'flycheck-google-cpplint)
		 ;; Add Google C++ Style checker.
		 ;; In default, syntax checked by Clang and Cppcheck.
		 ;;		 (flycheck-add-next-checker 'c/c++-googlelint
		 (flycheck-add-next-checker 'c/c++-clang
																'(warnings-only . c/c++-googlelint))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-c/c++-googlelint-executable "/usr/local/bin/cpplint.py")
 '(flycheck-googlelint-filter "-whitespace,+whitespace/braces")
 '(flycheck-googlelint-linelength "120")
 '(flycheck-googlelint-root "project/src")
 '(flycheck-googlelint-verbose "3")
 '(safe-local-variable-values
	 (quote
		((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
					 (add-hook
						(quote write-contents-functions)
						(lambda nil
							(delete-trailing-whitespace)
							nil))
					 (require
						(quote whitespace))
					 "Sometimes the mode needs to be toggled off and on."
					 (whitespace-mode 0)
					 (whitespace-mode 1))
		 (whitespace-line-column . 80)
		 (whitespace-style face trailing lines-tail)))))


;; turn on flycheck
;; (flycheck-mode) ;; none of that fucking shit for now maybe later when i want to shoot myself

;; https://stackoverflow.com/questions/7494203/how-do-i-m-x-replace-string-across-all-buffers-in-emacs
;; global search and replace
;; M-x ibuffer <RET> t U
;; OR C-x C-b t U

;; M-g g GOTO goes to go to a line number go to line

;; lets me fucking use wildcards if i want to god damn
;; cause ido-mode is great but doesn't allow that in C-x C-f
;; doesn't work, i'll just use eshell
;;(defun open-file-with-wildcards ()
;;	(interactive)
;;	(shell-command
;;	 (concatenate 'string "emacsclient -n "
;;								(read-string "filename wildcard: " nil nil "*")))) ;; reads from minibuf, default *

;; adds appropriate areas to load path
;; in this case for undo-tree and smart-tab
(add-to-list 'load-path "~/.emacs.d/lisp")

;; allow for save buffer reversion when files are being edited by external tools
(require 'revbufs)

(autoload 'markdown-mode "markdown-mode"
	"Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(defun toggle-letter-case ()
	"Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
	(interactive)
	(let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
		(if (region-active-p)
				(setq p1 (region-beginning) p2 (region-end))
			(let ((bds (bounds-of-thing-at-point 'word) ) )
				(setq p1 (car bds) p2 (cdr bds)) ) )

		(when (not (eq last-command this-command))
			(save-excursion
				(goto-char p1)
				(cond
				 ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
				 ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
				 ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
				 ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
				 ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
				 (t (put this-command 'state "all lower") ) ) ) )

		(cond
		 ((string= "all lower" (get this-command 'state))
			(upcase-initials-region p1 p2) (put this-command 'state "init caps"))
		 ((string= "init caps" (get this-command 'state))
			(upcase-region p1 p2) (put this-command 'state "all caps"))
		 ((string= "all caps" (get this-command 'state))
			(downcase-region p1 p2) (put this-command 'state "all lower")) )
		) )

;; adds icicles!!! yeah!!!
;; except i don't want it rn oops lol
;;(add-to-list 'load-path "~/.emacs.d/lisp/icicles")
;;(require 'icicles)
;;(icicle-mode)

;; adds smart-compile functionality
(require 'smart-compile)

;; treat cl files as C files (for now since we're doing OpenCL and not common lisp)
(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))

;; sets shell to zsh, hopefully, when i use it and not the emacs shell
(setq shell-file-name "zsh")
(setq explicit-shell-file-name "zsh")

;; adds evil vim layer
(add-to-list 'load-path "~/.emacs.d/evil/")
(require 'evil)
;; start up with evil on
;; (evil-mode 1) ; left off rn cause i'm not quite feelin it

;; allow for backtab => forced'\t'
(defun force-insert-tab ()
	"insert tab character"
	(interactive)
	(insert "\t"))

;; add el-get functionality
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;; 	(with-current-buffer
;; 			;; (url-retrieve-synchronously
;;			;; "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
;;			(find-file-noselect "~/.emacs.d/el-get/el-get-install.el") ; so that we don't have to use the internet to load up emacs lol
;;	  (goto-char (point-max))
;;	  (eval-print-last-sexp)
;;	  (revert-buffer nil t))							; discard changes to el-get-install.el
;;	(kill-buffer "el-get-install.el"))		; close buffer
;;(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;;(el-get 'sync)

;; above left in case el-get needs to be added again

;;; add stuff that used to be in el-get
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
;; destroy undo tree diff visualizer when undo tree visualizer is killed
;; doesn't work (just go backk to orig buffer and press space to throw all that away)
;; (add-hook 'kill-buffer-hook
;; 					(lambda ()
;; 						(if (equal (buffer-name) "*undo-tree*")
;; 								(kill-buffer "*undo-tree Diff*"))))

;; adds smart-tab (tab completion) functionality
(require 'smart-tab)
(global-smart-tab-mode 1)

;; adds dired-x functionality
(add-hook 'dired-load-hook
					(lambda ()
						(load "dired-x")
						;; Set dired-x global variables here.  For example:
						;; (setq dired-guess-shell-gnutar "gtar")
						;; (setq dired-x-hands-off-my-keys nil)
						))
(add-hook 'dired-mode-hook
					(lambda ()
						;; Set dired-x buffer-local variables here.	 For example:
						;; (dired-omit-mode 1)
						))

;; adds julia functionality
(load "~/.emacs.d/ESS/lisp/ess-site")
(setq inferior-julia-program-name "/usr/bin/julia-basic")
(add-to-list 'ess-tracebug-search-path "/usr/share/julia/base/")

;; adds haskell functionality
																				;(load "/usr/share/emacs-snapshot/site-lisp/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; pick one of the following three indentations
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; show line numbers
(global-linum-mode)
;; make them relative
(add-to-list 'load-path "~/.emacs.d/linum-relative")
(require 'linum-relative)
(setq linum-format 'linum-relative)
;; NOTE THAT I TOTALLY CHANGED LINUM-RELATIVE.EL
;; SO FOR COPYING THIS DEFINITELY CHECK THAT OUT
;; IF YOU WANT TO EMULATE THIS BEHAVIOR

;; from http://www.emacswiki.org/emacs/RevertBuffer
(defun revert-all-buffers ()
	"Refreshes all open buffers from their respective files."
	(interactive)
	(dolist (buf (buffer-list))
		(with-current-buffer buf
			(when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
				(revert-buffer t t t) )))
	(message "Refreshed open files.") )

(require 'cl)
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

;; GHC-mode for haskell
;; http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
(add-to-list 'load-path "~/.cabal/share/ghc-mod-4.1.1")
																				;(require 'ghc)
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
(setq ghc-debug t)

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
;; match parens when cursor on top
(show-paren-mode t)
;; integrate highlight-parentheses with autopair mode
(add-to-list 'load-path "~/.emacs.d/highlight-parentheses")
(require 'highlight-parentheses)
(highlight-parentheses-mode t)

;; tab correctly
(defun generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
				 (tab-width (or width tab-width))
				 (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq-default tab-width 2)
(setq-default tab-stop-list (generate-tab-stops))

;; set for different modes (usually done in file itself)
(setq-default c-basic-offset 2)

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
				 ("header" (filename . "\\.h\\'"))
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

;;; allow for ido usage for better C-x b buffer search n stuff
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)				; makes searching fuzzier

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

;; when opening a file the cursor will be at the last saved position
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)

;;; ibuffer hacks

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
					 "Open ibuffer with cursor pointed to most recent buffer name"
					 (let ((recent-buffer-name (buffer-name)))
						 ad-do-it
						 (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)


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

;; qmake-mode
(load "~/.emacs.d/lisp/qmake.el")

;;;;; keybindings
;; FIND CURRENT KEYBINDINGS WITH C-h b !!!!!

;; since eval-expression uses just the alt key but unity sucks
;;(global-set-key (kbd "C-M-z") 'eval-expression)
;; ESC-: DOES THIS ALREADY

;; (global-set-key (kbd "C-x C-f") 'helm-find-files) ; override normal find with helm find
;; (global-set-key (kbd "C-x C-f") 'helm-find) ; recursive helm find
;; (global-set-key (kbd "C-x C-M-f") 'ido-find-file)	; "uncertain find;" searches up and down recursively
;; defaults to above, where it will be kept
;; the below can also be applied over multiple lines with C-u [number] M-x helm-swoop RET
(global-set-key (kbd "C-x o") 'helm-swoop)				; find regexp in file, more interactively than above
(global-set-key (kbd "C-x f") 'helm-multi-swoop-all) ; find regexp in ALL open buffers
(global-set-key (kbd "C-x j") 'helm-multi-swoop)		 ; find regexp is SOME open buffers
(global-set-key (kbd "C-x b") 'helm-buffers-list) ; find among open buffers

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
(global-set-key (kbd "C-M-<left>")	'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>")	'windmove-up)
(global-set-key (kbd "C-M-<down>")	'windmove-down)

;; visualize undo-tree
(global-set-key (kbd "C-x t") 'undo-tree-visualize)

;; use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill current buffer and close pane
(global-set-key (kbd "C-x C-k") 'close-and-kill-this-pane)

;; smart-compile stuff!!!!
(global-set-key (kbd "C-x c") 'smart-compile)

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
(global-set-key (kbd "C-c C-l") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-n") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-a") 'mc/mark-all-like-this)

;; icicle-locate allows for much easier file location, using the OS's indexes
;; (global-set-key (kbd "C-x M-l") 'icicle-locate) ; nonfunctional right now

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

;; open file with wildcards
;;(global-set-key (kbd "C-c o") 'open-file-with-wildcards)
;; doesn't work lol

;; toggle letter casing from ALLCAPS to InitialCase to alllowercase
(global-set-key (kbd "C-x M-c") 'toggle-letter-case)

;; TOGGLE RELATIVE LINUM
(global-set-key (kbd "C-x C-l") 'linum-relative-toggle)

;; makes it so relative numbering STAYS ON after minibuffer exited
(add-hook 'minibuffer-exit-hook
					(lambda ()
						(setq linum-format 'linum-relative)))
(add-hook 'compilation-mode-hook
					(lambda ()
						(setq linum-format 'linum-relative)))
(add-hook 'gdb-mode-hook
					(lambda ()
						(setq linum-format 'linum-relative)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; sudo open file with C-x C-f /sudo::/path/to/file
;; more tramp stuff
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;; CEDET stuff that i'm not using rn
;; gnu global tagging
;; (load-file "~/.emacs.d/lisp/gtags.el")
;; (require 'gtags)

;;;;; turn on cedet and semantic mode
;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
;; http://cxwangyi.wordpress.com/2010/08/21/using-cedet-with-emacs/
;; (unless (featurep 'cedet)
;;     (add-to-list 'load-path "~/.emacs.d/cedet")
;;     (load-file "~/.emacs.d/cedet/cedet-devel-load.el"))

;; (global-ede-mode t)
;; (require 'semantic)
;; (require 'semantic/ia) ;; name completion and tag display
;; (require 'semantic/bovine/gcc) ;; use system include files
;; (require 'semantic/bovine/c)
;; (require 'semantic/db) ;; not sure if required, think not
;; ;; include Qt
;; (semantic-reset-system-include 'c-mode)
;; (semantic-reset-system-include 'c++-mode)
;; (setq qt5-base-dir "/opt/Qt/5.3/gcc_64/include")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/Qt/")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/QtGui/")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/QtCore/")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/QtTest/")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/QtNetwork/")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/QtSvg/")
;; (semantic-add-system-include "/opt/Qt/5.3/gcc_64/include/QtWidgets/") 
;; ;; (semantic-add-system-include "/usr/include/boost/")
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt5-base-dir "/QtCore/qconfig.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt5-base-dir "/QtCore/qconfig-dist.h"))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt5-base-dir "/QtCore/qglobal.h"))

;; ;; (hs-minor-mode) ;; C-c @ C-c for folding up code blocks!!!

;; (global-semanticdb-minor-mode t)
;; (global-cedet-m3-minor-mode t)
;; (set-default 'semantic-case-fold t)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-stickyfunc-mode t)
;; (global-semantic-decoration-mode t)
;; (global-semantic-idle-local-symbol-highlight-mode t)
;; (global-semantic-idle-scheduler-mode t)
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-idle-summary-mode t)

;; (defun add-tags-semantic-hook ()
;; 	(imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'add-tags-semantic-hook)

;; ;; if you want to enable support for gnu global
;; (when (cedet-gnu-global-version-check t)
;; 	(semanticdb-enable-gnu-global-databases 'c-mode)
;; 	(semanticdb-enable-gnu-global-databases 'c++-mode))

;; ;; enable ctags for some languages:
;; ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; (when (cedet-ectag-version-check t)
;; 	  (semantic-load-enable-primary-ectags-support))

;; (defun keybindings-cedet-hook ()
;; 	(local-set-key [(control return)] 'semantic-ia-complete-symbol)
;; 	(local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
;; 	(local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;; 	(local-set-key "\C-c=" 'semantic-decoration-include-visit)
;; 	(local-set-key "\C-cj" 'semantic-ia-fast-jump)
;; 	(local-set-key "\C-cq" 'semantic-ia-show-doc)
;; 	(local-set-key "\C-cs" 'semantic-ia-show-summary)
;; 	(local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
;; 	)
;; (add-hook 'c-mode-common-hook 'keybindings-cedet-hook)
;; ;; control return: whatever the symbol you are typing, this hot key automatically complete it for you.
;; ;; C-c?: another way to complete the symbol you are typing
;; ;; C-c>: when you typed . or -> after an object name, use this key to show possible public member functions or data members.
;; ;; C-cj: jump to the definition of the symbol under cursor
;; ;; C-cs: show a summary about the symbol under cursor
;; ;; C-cq: show the document of the symbol under cursor
;; ;; C-c=: visit the header file under cursor
;; ;; C-cp: toggle between the implementation and a prototype of symbol under cursor
;; ;; C-ce: when your cursor is in the scope of a class or one of its member function, list all methods in the class
;; ;; C-cC-r: show references of the symbol under cursor

;; (semantic-mode t) ;; GO GO GO

;; add org-mode stuff
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
