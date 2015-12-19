;;; keybindings are cool and fun
;;; i think there are like 200 of these in here

;;; reminders about existing keybindings:
;;; 1. i've remapped C-z to undo, so C-x C-z is a cute hack to suspend emacs (or
;;;    minimize when in graphical mode)
;;; 2. find current keybindings with C-h b and  use C-h k to look at the current
;;;    key sequence being entered! this is useful when creating new keybindings
;;; 3. C-Spc to start selection (set mark) in terminal!
;;; 4. remember that M-= gets word counts!

;;; globally usable basic text insertion or command-running shortcuts
(global-set-key (kbd "C-x d") #'open-dired-the-right-way)
(global-set-key (kbd "C-x C-d") #'open-dired-the-right-way)
(define-key dired-mode-map (kbd "M-f") #'find-dired)
;;; cause otherwise this doesn't work in graphical mode
(global-set-key (kbd "<C-return>") 'newline-and-indent)
;;; just destroy unused files
(global-set-key (kbd "C-x C-M-d") 'kill-buffer-and-move-file-to-trash)
;;; opposite of yank-pop
(global-set-key (kbd "C-M-y") 'yank-push)
;;; reset quit key combination to close
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x C-c C-q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c C-f") 'delete-frame)
;;; recognize camel case and work with it
(global-set-key (kbd "C-<right>") 'camel-case-right-word)
(global-set-key (kbd "C-<left>") 'camel-case-left-word)
;;; toggle letter casing from ALLCAPS to InitialCase to all lowercase
(global-set-key (kbd "C-x M-c") 'toggle-letter-case)
;;; kill buffer without prompting
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;;; quit-window is more useful than i previously thought
(global-set-key (kbd "C-c q") 'quit-window)

;;; compatibility fixes for windows
(global-set-key (kbd "<C-kp-home>")
                (lookup-key (current-global-map) (kbd "<C-home>") t))
(global-set-key (kbd "<C-kp-end>")
                (lookup-key (current-global-map) (kbd "<C-end>") t))

(global-set-key (kbd "C-c W") #'clear-beginning-whitespace)

;;; helm
;;; the below can also be applied over multiple lines with:
;;; C-u [number] M-x helm-swoop RET
(global-set-key (kbd "C-x o") 'helm-swoop)
;;; find regexp in ALL open buffers
(global-set-key (kbd "C-x f") 'helm-multi-swoop-all)
;;; find regexp in SOME open buffers
(global-set-key (kbd "C-x j") 'helm-multi-swoop)
;;; find buffer by name
(global-set-key (kbd "C-x b") 'helm-buffers-list)

;; after killing C-x o with helm,
;; let's make sure we do have buffer switching in the event of non-graphical
;; terminal-only editing
(global-set-key (kbd "C-x /") 'other-window)

;;; dired
;;; mildly useful
(define-key dired-mode-map (kbd "F") #'dired-find-marked-files-no-show)
(define-key dired-mode-map (kbd "* *") #'dired-mark-files-wildcard)
(define-key dired-mode-map (kbd "D") #'dired-flag-marked-files)
(defun dired-grep (pfx)
  (interactive "P")
  (call-interactively (if pfx #'dired-grep-marked-files #'grep)))
(define-key dired-mode-map (kbd "G") #'dired-grep)

;;; split-window management
;; open and close
(global-set-key (kbd "C-x <down>") 'split-window-below)
(global-set-key (kbd "C-x <right>") 'split-window-right)
(global-set-key (kbd "C-x <left>") 'split-window-horizontally)
(global-set-key (kbd "C-x <up>") 'split-window-vertically)
(global-set-key (kbd "C-x C-<down>") 'split-window-below)
(global-set-key (kbd "C-x C-<right>") 'split-window-right)
(global-set-key (kbd "C-x C-<left>") 'split-window-horizontally)
(global-set-key (kbd "C-x C-<up>") 'split-window-vertically)
(global-set-key (kbd "C-x e") 'delete-other-windows) ;; "expand"
(global-set-key (kbd "C-x p") 'delete-window) ;; "poof"
(global-set-key (kbd "C-x RET") 'call-last-kbd-macro) ; because expand destroyed
;; adjusting pane size
(global-set-key (kbd "C-x <home>") 'enlarge-window) ;; increase window height
(global-set-key (kbd "C-x <end>") 'shrink-window) ;; decrease window height
(global-set-key (kbd "C-x <prior>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <next>") 'shrink-window-horizontally)
;; window to fit content
(global-set-key (kbd "C-x !") 'balance-windows) ;; make all windows same height
;;; move among panes in a way that isn't totally fucked
(setq windmove-wrap-around t)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)

;; visualize undo-tree
(global-set-key (kbd "C-x t") 'undo-tree-visualize)

;; use ibuffer for buffer list
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; kill current buffer and close pane
(global-set-key (kbd "C-x C-k") 'close-and-kill-this-pane)

;; smart-compile stuff!!!!
(global-set-key (kbd "C-c C-k") 'smart-compile)

;;; w3m
(when (executable-find "w3m")
  (define-key w3m-mode-map (kbd "C-<tab>") 'w3m-tab-next-buffer)
  (define-key w3m-mode-map (kbd "C-l C-<tab>") 'w3m-tab-move-right)
  (define-key w3m-mode-map (kbd "<C-iso-lefttab>") 'w3m-tab-previous-buffer)
  (define-key w3m-mode-map (kbd "C-l <C-iso-lefttab>") 'w3m-tab-move-left)
  (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)
  (define-key w3m-mode-map (kbd "C-t") 'w3m-create-empty-session)
  (define-key w3m-mode-map (kbd "C-S-t") 'w3m-goto-url-new-session)
  (define-key w3m-mode-map (kbd "v") 'w3m-view-source))

;; make C-z undo instead of FUCKING UP MY ENTIRE LIFE by suspending
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)

;; open new file with given filename from minibuffer, or blank filename
(global-set-key (kbd "C-x C-n") 'open-new-file)

;; multiple cursors fun!!!
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "M-n") #'mc/mark-next-like-this)
(global-set-key (kbd "M-p") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-n") #'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-p") #'mc/unmark-previous-like-this)
(global-set-key (kbd "C-x C-a") #'mc/mark-all-like-this)

;; gofmt!!!
(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'go-fmt-file)))

;;; search all open buffers for regexp
;;; (really helm-swoop is better but if you need full POSIX regex then ok)
(global-set-key (kbd "C-c M-r") 'search-all-buffers)

(eval-after-load 'java-mode
  '(define-key java-mode-map (kbd "C-c C-w") nil))

;;; c/c++/java
(add-hook 'c-initialization-hook
	  (lambda ()
	    (define-key c-mode-map (kbd "C-j") 'newline-and-indent-ctrl-j)
	    (define-key c-mode-map (kbd "<C-return>")
              'newline-and-indent-ctrl-j)
	    (define-key c++-mode-map (kbd "C-j") 'newline-and-indent-ctrl-j)
	    (define-key c++-mode-map (kbd "<C-return>")
              'newline-and-indent-ctrl-j)
	    (define-key c++-mode-map (kbd "{") 'insert-brackets)
            (define-key java-mode-map (kbd "C-c C-w") nil)))

;;; cool but never used cause lol search key
(global-set-key (kbd "<XF86Search>") 'helm-multi-swoop-all)

;;; coffeescript
(eval-after-load "coffee-mode"
  '(progn
     (define-key coffee-mode-map (kbd "C-c C-k") #'smart-compile)
     (define-key coffee-mode-map (kbd "C-c C-r") #'coffee-send-region)
     (define-key coffee-mode-map (kbd "C-c C-c") #'coffee-compile-buffer)
     (define-key coffee-mode-map (kbd "C-M-h") nil)
     (define-key coffee-mode-map (kbd "C-M-g") #'restart-coffee)))

;;; js
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "C-c C-w") nil)
     (eval-after-load 'skewer-mode
        (progn
          (define-key js2-mode-map (kbd "C-M-x")
            #'skewer-eval-buffer-or-region)))
     (eval-after-load 'js-mode
        (progn
          (define-key js-mode-map (kbd "C-<tab>") #'web-beautify-js)
          (define-key js2-mode-map (kbd "C-<tab>") #'web-beautify-js)
          (add-keybinding-to-mode-maps "RET" #'js-newline-indent-for-real
                                       js-mode-map js2-mode-map)))))

;;; css
(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-<tab>") #'web-beautify-css))

;;; CPerl-mode
(add-hook 'cperl-mode-hook
          (lambda ()
            (define-key cperl-mode-map (kbd "C-c C-k") nil)
            (define-key cperl-mode-map (kbd "C-h f") #'cperl-perldoc)
            (define-key cperl-mode-map (kbd "C-c C-w") nil)
            (define-key cperl-mode-map (kbd "C-c C-v") nil)))

;;; lisp
;;; so it's all emacsy
(eval-after-load 'slime
  '(progn
     (add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
     (define-key lisp-mode-map (kbd "C-h f") 'slime-documentation)
     (define-key lisp-mode-map (kbd "C-h v") 'slime-documentation)
     (define-key lisp-mode-map (kbd "C-h h") 'slime-hyperspec-lookup)
     (define-key slime-mode-indirect-map (kbd "M-p")
       #'mc/mark-previous-like-this)
     (define-key slime-mode-indirect-map (kbd "M-n")
       #'mc/mark-next-like-this)))

;;; makefile
(eval-after-load "make-mode"
  '(progn
     (define-key makefile-gmake-mode-map (kbd "M-n") nil)
     (define-key makefile-gmake-mode-map (kbd "M-p") nil)))

;;; erc
(global-set-key (kbd "C-c C-e") 'message-erc-modded-chans)

;;; replacing text
(global-set-key (kbd "M-$") 'replace-string)
(global-set-key (kbd "M-#") 'replace-regexp)

;;; org-mode
(defun org-replace-forward-paragraph ()
  (interactive "^")
  (unless (eobp)
    (if (re-search-forward "[^\n]\n\n" nil t) (backward-char)
      (end-of-buffer))))
(defun org-replace-backward-paragraph ()
  (interactive "^")
  (unless (bobp)
    (if (re-search-backward "\n\n[^\n]" nil t) (forward-char)
      (beginning-of-buffer))))
(eval-after-load 'org
  '(progn
     (define-key org-mode-map (kbd "<C-up>") #'org-replace-backward-paragraph)
     (define-key org-mode-map (kbd "<C-down>") #'org-replace-forward-paragraph)
     (define-key org-mode-map (kbd "<C-S-up>") nil)
     (define-key org-mode-map (kbd "<C-S-down>") nil)
     (define-key org-mode-map (kbd "C-c C-v") nil)
     (define-key org-mode-map (kbd "C-c C-w") nil)
     (define-key org-mode-map (kbd "<tab>") nil)
     (define-key org-mode-map (kbd "C-c a") #'org-agenda)
     (define-key org-mode-map (kbd "C-c c") #'org-capture)
     (define-key org-mode-map (kbd "C-k") #'kill-line-or-region)
     (define-key org-mode-map (kbd "C-c a") nil)
     (add-to-list
      'org-mode-hook
      (lambda () (define-key org-mode-map (kbd "C-c C-k") #'smart-compile)))
     (define-key org-mode-map (kbd "S-<up>") #'org-shiftup)
     (define-key org-mode-map (kbd "C-c e") #'org-latex-export-to-pdf)
     (define-key org-mode-map (kbd "S-<down>") #'org-shiftdown)
     (define-key org-mode-map (kbd "C-p") #'outline-previous-heading)
     (define-key org-mode-map (kbd "C-n") #'outline-next-heading)
     (define-key org-mode-map (kbd "C-M-u") #'outline-up-heading)))

;;; convenience bindings from working with windows
(global-set-key (kbd "M-`") #'indent-regardless-of-mode)

;;; c#
(eval-after-load "csharp-mode"
  '(progn
     (define-key csharp-mode-map (kbd "RET") #'csharp-hack-newline)
     (define-key csharp-mode-map (kbd "(") #'just-electric-parens)
     (define-key csharp-mode-map (kbd "{") #'just-electric-bracket)
     (define-key csharp-mode-map (kbd ";") #'just-semicolon)))

;;; blegh
(global-set-key (kbd "<insertchar>") nil)
(global-set-key (kbd "<insert>") nil)

;;; markdown
(eval-after-load "markdown-mode"
  '(progn
     (define-key markdown-mode-map (kbd "M-n") #'mc/mark-next-like-this)
     (define-key markdown-mode-map (kbd "M-p") #'mc/mark-previous-like-this)))

(eval-after-load 'grep
  '(define-key grep-mode-map (kbd "G") #'refind-or-grep))

(global-set-key (kbd "C-M-h") nil)
(global-set-key (kbd "C-h d") #'describe-function-or-variable)
(global-set-key (kbd "C-M-h d")
  #'describe-function-or-variable-at-point)
;;; TODO: write this
;; (global-set-key (kbd "C-h C-d") #'find-function-or-variable)
(global-set-key (kbd "C-M-h C-d")
  #'find-function-or-variable-at-point)
(global-set-key (kbd "C-M-h f") #'describe-function-at-point)
(global-set-key (kbd "C-M-h v") #'describe-variable-at-point)
(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-M-h C-f") #'find-function-at-point)
(global-set-key (kbd "C-h C-v") #'find-variable)
(global-set-key (kbd "C-M-h C-v") #'find-variable-at-point)

;;; now for c
(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map (kbd "C-h C-d") #'ggtags-find-definition)
     (define-key c-mode-map (kbd "C-h r") #'ggtags-find-reference)
     (define-key c-mode-map (kbd "C-h d") #'ggtags-find-tag-dwim)
     (define-key c-mode-map (kbd "C-h C-s") #'ggtags-show-definition)
     (define-key c-mode-map (kbd "C-h l") #'ggtags-find-file)
     (define-key c-mode-map (kbd "C-M-h") #'ggtags-update-tags)
     (define-key c-mode-map (kbd "C-M-r") #'ggtags-reload)
     (define-key c-mode-map (kbd "C-c C-k") #'smart-compile)
     (define-key c-mode-map (kbd "C-c C-c")
       (lambda () (interactive)
         (ggtags-ensure-global-buffer (kill-compilation))))
     (define-key c-mode-map (kbd "C-c t") #'helm-gtags-find-tag)
     (define-key c-mode-map (kbd "C-c C-w") #'destroy-all-whitespace-nearby)
     (define-key c++-mode-map (kbd "C-c C-w")
       #'destroy-all-whitespace-nearby)
     (define-key c-mode-map (kbd "C-<tab>") #'clang-format-buffer)
     (define-key c++-mode-map (kbd "C-<tab>") #'clang-format-buffer)
     ;; and c++
     (define-key c++-mode-map (kbd "RET") #'newline-and-indent-fix-cc-mode)
     (define-key c-mode-map (kbd "RET") #'newline-and-indent-fix-cc-mode)))

;;; in the same vein
(global-set-key (kbd "C-x C-h") #'pop-to-mark-command)
(global-set-key (kbd "C-x M-h") #'unpop-to-mark-command)
(global-set-key (kbd "C-x C-p") #'previous-buffer)
(global-set-key (kbd "C-x M-p") #'next-buffer)

;;; more emacs lisp stuff
(define-key emacs-lisp-mode-map (kbd "C-M-x") #'eval-buffer-and-message)
(define-key emacs-lisp-mode-map (kbd "<return>") #'newline)

;;; erc stuff
(global-set-key (kbd "C-c M-e") #'message-erc-modded-chans)

;;; html stuff
(define-key html-mode-map (kbd "C-c C-v") nil)
(define-key html-mode-map (kbd "M-o") #'open-in-browser)
(define-key html-mode-map (kbd ">") #'html-autoclose-tag)
(define-key html-mode-map (kbd "M-s") #'html-split-tag)
(define-key html-mode-map (kbd "M-r") #'html-raise-tag)
(define-key html-mode-map (kbd "M-S-<up>") #'html-beginning-of-tag)
(define-key html-mode-map (kbd "C-<left>") #'html-skip-tag-or-token-backward)
(define-key html-mode-map (kbd "M-S-<left>") #'html-skip-to-beginning-of-tag)
(define-key html-mode-map (kbd "M-S-<right>") #'html-skip-to-end-of-tag)
(define-key html-mode-map (kbd "C-S-k") #'html-kill-tag-contents)
(define-key html-mode-map (kbd "M-S-<down>") #'html-end-of-tag)
(define-key html-mode-map (kbd "C-<right>") #'html-skip-tag-or-token-forward)
(define-key html-mode-map (kbd "C-k") #'html-kill-tag-after-point-int)
(define-key html-mode-map (kbd "C->") (lambda () (interactive) (insert ">")))
(define-key html-mode-map (kbd "C-<tab>") #'web-beautify-html)
(define-key html-mode-map (kbd "C-c C-h") #'html-insert-xhtml-header)
(define-key html-mode-map (kbd "RET") #'html-newline-indent)
(define-key html-mode-map (kbd "M-a M-<right>") #'html-slurp-tag-forward)
(define-key html-mode-map (kbd "M-a M-<left>") #'html-slurp-tag-backward)
(define-key html-mode-map (kbd "C-M-a C-M-<right>") #'html-barf-tag-forward)
(define-key html-mode-map (kbd "C-M-a C-M-<left>") #'html-barf-tag-backward)
(define-key html-mode-map (kbd "C-y") #'html-yank)
(define-key html-mode-map (kbd "M-y") #'html-yank-pop)

;;; dired
(define-key dired-mode-map (kbd "M-t") #'dired-touch-file)
(define-key dired-mode-map (kbd "l") #'dired-run-lisp)
(define-key dired-mode-map (kbd "f") #'find-name-dired)
(define-key dired-mode-map (kbd "K") #'dired-kill-marked-files)

;;; random
(global-set-key (kbd "C-c C-w") #'destroy-all-whitespace-nearby)

;;; magit
(eval-after-load "magit"
  '(progn
     (global-set-key (kbd "C-c g") 'magit-status)
     (global-set-key (kbd "C-c b") #'magit-blame)
     (global-set-key (kbd "C-c d") #'magit-diff)
     (global-set-key (kbd "C-c c") #'magit-show-commit)
     (eval-after-load 'org
       '(define-key org-mode-map (kbd "C-c c") #'magit-show-commit))
     (define-key magit-mode-map (kbd "<tab>") #'magit-tab-dwim)
     (define-key magit-mode-map (kbd "<backtab>") #'magit-section-up)))
(eval-after-load 'magit-blame
  '(progn (define-key magit-blame-mode-map (kbd "c") #'magit-show-commit)))

;;; LOL MAIL
(global-set-key (kbd "C-x m") nil)

(eval-after-load 'literate-coffee-mode
  '(define-key litcoffee-mode-map (kbd "C-c C-v")
     #'litcoffee-toggle-code-prose))

(global-set-key (kbd "C-c r") 'align-regexp)

;;; haskell
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "<return>")
       #'haskell-newline-actual-indent)
     (define-key haskell-mode-map (kbd "C-c C-v") nil)))

(define-key compilation-mode-map (kbd "G") #'compile)

(eval-after-load 'haskell-interactive-mode
  '(progn
     (define-key haskell-interactive-mode-map (kbd "C-c <tab>")
       #'haskell-process-do-info)
     (define-key haskell-interactive-mode-map (kbd "M-p") nil)
     (define-key haskell-interactive-mode-map (kbd "M-n") nil)
     (define-key haskell-interactive-mode-map (kbd "C-c C-k") #'smart-compile)))

;;; paredit
(define-key paredit-mode-map (kbd "M-t") 'transpose-sexps)
(define-key paredit-mode-map (kbd "M-;") 'fix-paredit-comment-dwim)
(define-key paredit-mode-map (kbd "C-M-<left>") 'windmove-left)
(define-key paredit-mode-map (kbd "C-M-<right>") 'windmove-right)
(define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward) ; remove key
                                        ; here (slurp-forward)
(define-key paredit-mode-map (kbd "C-c <right>") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward) ; remove key
                                        ; here (slurp-backward)
(define-key paredit-mode-map (kbd "C-c <left>") 'paredit-backward)
(define-key paredit-mode-map (kbd "M-a") nil) ; kill this, it's a global but
                                        ; it's annoying and i don't use it
(define-key paredit-mode-map (kbd "M-a M-a") 'paredit-add-parens-in-front)
(define-key paredit-mode-map (kbd "M-a M-s") 'paredit-remove-function-wrapper)
(define-key paredit-mode-map (kbd "M-q") #'fill-paragraph)
;; (global-set-key (kbd "RET") 'newline-and-indent)
(define-key paredit-mode-map (kbd "M-a M-<right>")
  'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-c <up>")
  'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "M-a M-<left>")
  'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-M-a C-M-<right>")
  'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-c <down>")
  'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-M-a C-M-<left>")
  'paredit-backward-barf-sexp)
(define-key slime-mode-indirect-map (kbd "C-M-a") nil)
(define-key slime-repl-mode-map (kbd "M-s") nil)
;; so that multiple-cursors can use these
(define-key paredit-mode-map (kbd "C-x C-l") 'mc/edit-lines)
(define-key paredit-mode-map (kbd "M-n") #'mc/mark-next-not-cider)
(define-key paredit-mode-map (kbd "M-p") #'mc/mark-prev-not-cider)
(define-key paredit-mode-map (kbd "C-M-n") 'mc/unmark-next-like-this)
(define-key paredit-mode-map (kbd "C-M-p") 'mc/unmark-previous-like-this)
(define-key paredit-mode-map (kbd "C-x C-a") 'mc/mark-all-like-this)
(define-key paredit-mode-map (kbd "C-M-y") 'paredit-yank-push)
(define-key paredit-mode-map (kbd "DEL") 'paredit-backspace-delete-highlight)
(define-key paredit-mode-map (kbd "M-S-<up>") #'paredit-backward-up)
(define-key paredit-mode-map (kbd "M-S-<down>") #'paredit-forward-up)

;;; clojure
(defun mc/mark-next-not-cider ()
  (interactive)
  (cond ((eq major-mode 'cider-repl-mode)
         (call-interactively #'cider-repl-next-input))
        ((eq major-mode 'racket-repl-mode)
         (call-interactively #'comint-next-input))
        (t
         (call-interactively #'mc/mark-next-like-this))))
(defun mc/mark-prev-not-cider ()
  (interactive)
  (cond ((eq major-mode 'cider-repl-mode)
         (call-interactively #'cider-repl-previous-input))
        ((eq major-mode 'racket-repl-mode)
         (call-interactively #'comint-previous-input))
        (t
         (call-interactively #'mc/mark-previous-like-this))))
(eval-after-load 'cider-mode
  '(progn
     (define-key cider-mode-map (kbd "C-c C-w")
       #'destroy-all-whitespace-nearby)))
(eval-after-load 'cider
  '(progn
     (add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
     (eval-after-load 'clojure-mode
       '(progn
          (define-key clojure-mode-map (kbd "C-h f") #'cider-doc)))))

;;; eww (lol)
(eval-after-load 'eww
  '(define-key eww-mode-map (kbd "C-c v") #'refresh-visual-line-mode))

;;; git-gutter
(global-set-key (kbd "C-x l") #'num-lines-file)
(global-set-key (kbd "C-c n") #'git-gutter:next-hunk)
(global-set-key (kbd "C-c p") #'git-gutter:previous-hunk)
(global-set-key (kbd "C-c s") #'git-gutter:stage-hunk)
(global-set-key (kbd "C-c D") #'git-gutter:popup-hunk)
(global-set-key (kbd "C-c R") #'git-gutter:revert-hunk)
(global-set-key (kbd "<home>") #'beg-of-line-text)
(global-set-key (kbd "<end>") #'move-end-of-line)

(global-set-key (kbd "C-c C-v") #'delete-whole-line)
(global-set-key (kbd "C-x C-r") #'revert-buffer-no-confirm)

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "C-c C-v") #'delete-whole-line)))

(eval-after-load 'tex-mode
  '(progn
     (define-key tex-mode-map (kbd "C-c C-w") nil)
     (define-key LaTeX-mode-map (kbd "C-c C-w") nil)
     (define-key LaTeX-mode-map (kbd "C-c C-v") nil)
     (define-key LaTeX-mode-map (kbd "C-k") #'kill-line-or-region)
     (define-key LaTeX-mode-map (kbd "\"") #'TeX-quote-region)
     (define-key tex-mode-map (kbd "C-c C-v") nil)
     (define-key tex-mode-map (kbd "C-c e") #'latex-compile)
     (define-key tex-mode-map (kbd "C-c C-b") #'bibtex-compile)
     (define-key LaTeX-mode-map (kbd "C-c m") #'latex-insert-math)
     (define-key latex-mode-map (kbd "C-c C-s")
       #'switch-to-latex-compile-output)))

(global-set-key (kbd "C-M-g") #'rerun-command)
(eval-after-load 'sh-mode
  '(define-key sh-mode-map (kbd "C-c C-w") #'destroy-all-whitespace-nearby))

;; (eval-after-load 'ein-notebook
;;   '(define-key ein:notebook-mode-map (kbd ".") #'self-insert-command))

;;; this is annoying
(global-set-key (kbd "C-x C-x") nil)

(eval-after-load 'ess
  '(progn
     (define-key ess-mode-map (kbd "C-c C-v") nil)
     (define-key ess-mode-map (kbd "C-x C-e") #'ess-eval-paragraph-and-go)))

(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)))

(eval-after-load 'org-plot
  '(progn (define-key org-mode-map (kbd "C-M-g") #'org-plot/gnuplot)))

(eval-after-load 'sml-mode
  '(define-key sml-mode-map (kbd "|") #'fix-sml-smart-pipe))

(eval-after-load 'multiple-cursors
  '(progn (define-key mc/keymap (kbd "<return>") nil)))

(global-set-key (kbd "M-i") #'imenu)

(eval-after-load 'tuareg
  '(progn (define-key tuareg-mode-map (kbd "C-c C-c") #'tuareg-eval-buffer)))

(eval-after-load 'inf-ruby
  '(progn (define-key ruby-mode-map (kbd "C-c C-c") #'ruby-send-buffer)))

(eval-after-load 'bison-mode
  '(progn
     (define-key bison-mode-map (kbd "RET") #'newline-and-indent)))

(eval-after-load 'prolog
  '(progn (define-key prolog-mode-map (kbd "C-c C-v") nil)))

(global-set-key (kbd "C-c a") #'cycle-shell-buffers)
