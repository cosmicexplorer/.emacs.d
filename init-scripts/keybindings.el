;; -*- lexical-binding: t -*-

;;; keybindings are cool and fun
;;; i think there are like 200 of these in here

;;; reminders about existing keybindings:
;;; 1. i've remapped C-z to undo, so C-x C-z is a cute hack to suspend emacs (or
;;;    minimize when in graphical mode)
;;; 2. find current keybindings with C-h b and  use C-h k to look at the current
;;;    key sequence being entered! this is useful when creating new keybindings
;;; 3. C-Spc to start selection (set mark) in terminal!
;;; 4. remember that M-= gets word counts!

(setq x-meta-keysym 'super
      x-super-keysym 'meta)

(global-set-key (kbd "M-Z") (cmd (insert " ")))
(global-set-key (kbd "C-s-f") #'forward-sexp)
(global-set-key (kbd "C-s-b") #'backward-sexp)
(global-set-key (kbd "M-.") nil)
(global-set-key (kbd "M-?") nil)
(global-set-key (kbd "C-x M-w") #'cycle-window-configuration)

(defvar prev-keymaps nil)

(defun set-key-dwim (map key cmd)
  (cl-check-type key string)
  (cl-check-type cmd (or command null))
  (cl-etypecase map
    (keymap
     (define-key map (kbd key) cmd)
     map)
    (null
     (global-set-key (kbd key) cmd)
     (current-global-map))))

(defun set-keys-helper (maps kill assign)
  (cl-check-type maps list)
  (cl-assert (> (length maps) 0))
  (cl-check-type kill list)
  (cl-check-type assign list)
  (let* ((keymaps (--map
                   (cl-etypecase it
                     (function (funcall it))
                     (symbol (symbol-value it))
                     (keymap it)
                     (null it))
                   maps))
         (commands (--map
                    (pcase-exhaustive it
                      ((pred stringp)
                       (list it nil))
                      (`(,(pred stringp) ,(pred commandp))
                       it))
                    (append kill assign))))
    (cl-assert (> (length keymaps) 0))
    (cl-assert (> (length commands) 0))
    (--map (cl-reduce (-lambda (cur-map (key-seq defn))
                        (set-key-dwim cur-map key-seq defn))
                      commands
                      :initial-value it)
           keymaps)))

(defun set-keys-in (keys-alist)
  (cl-assert (and keys-alist (listp keys-alist)) t)
  (cl-destructuring-bind (ft &rest others) keys-alist
    (cl-destructuring-bind (&key load map kill assign) ft
      (let ((map-list (if (listp map) map (list map))))
        (cl-check-type load symbol)
        (cl-check-type kill list)
        (cl-check-type assign list)
        (cl-check-type map-list list)
        (if load (eval-after-load load
                   `(set-keys-helper ',map-list ',kill ',assign))
          (set-keys-helper map-list kill assign))
        (when others (set-keys-in others))))))

;;; globally usable basic text insertion or command-running shortcuts
(global-set-key (kbd "C-M-;") #'newline-and-comment)
(global-set-key (kbd "C-x d") #'dired-find-containing-or-gen)
(global-set-key (kbd "C-x C-d") #'dired-find-containing-or-gen)
(define-key dired-mode-map (kbd "M-f") #'find-dired)
(define-key dired-mode-map (kbd "<tab>")
  (lambda ()
    (interactive)
    (call-interactively #'dired-maybe-insert-subdir)
    (recenter)))
(define-key dired-mode-map (kbd "<backtab>")
  (lambda ()
    (interactive)
    (call-interactively #'dired-up-directory)
    (recenter)))
(define-key dired-mode-map (kbd "S-<tab>")
  (lambda ()
    (interactive)
    (call-interactively #'dired-up-directory)
    (recenter)))
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
(defun kill-this-buffer-and-all-visiting (pfx)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (if pfx (kill-buf-and-all-visiting nil) (kill-this-buffer))))
(global-set-key (kbd "C-x k") #'kill-this-buffer-and-all-visiting)
;;; quit-window is more useful than i previously thought
(global-set-key (kbd "C-c q") 'quit-window)
(global-set-key (kbd "C-c C-q") 'quit-window)

;;; compatibility fixes for windows
(global-set-key (kbd "<C-kp-home>")
                (lookup-key (current-global-map) (kbd "<C-home>") t))
(global-set-key (kbd "<C-kp-end>")
                (lookup-key (current-global-map) (kbd "<C-end>") t))

(global-set-key (kbd "C-c W") #'clear-beginning-whitespace)

;;; helm
;;; the below can also be applied over multiple lines with:
;;; C-u [number] M-x helm-swoop RET
(defun my-helm-swoop ()
  (interactive)
  (helm-swoop :$query (thing-at-point 'symbol)))
(global-set-key (kbd "C-x o") #'my-helm-swoop)
;;; find regexp in ALL open buffers
(defun my-multi-swoop-all (pfx)
  (interactive "P")
  (if pfx (helm-multi-swoop-all (thing-at-point 'symbol))
    (helm-multi-swoop-all)))
(global-set-key (kbd "C-x f") 'my-multi-swoop-all)
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
(global-set-key (kbd "C-x e") #'delete-other-windows-maybe-save) ;; "expand"
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
  (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buf-remember)
  (define-key w3m-mode-map (kbd "C-t") 'w3m-create-empty-session)
  (define-key w3m-mode-map (kbd "C-S-t") 'w3m-restore-buffer)
  (define-key w3m-mode-map (kbd "<C-return>") 'w3m-goto-url-new-tab)
  (define-key w3m-mode-map (kbd "<C-mouse-1>") 'w3m-forget)
  (define-key w3m-mode-map (kbd "<C-drag-mouse-1>") 'w3m-forget)
  (define-key w3m-mode-map (kbd "<C-down-mouse-1>") 'w3m-goto-url-new-tab-mouse)
  (define-key w3m-mode-map (kbd "<down-mouse-1>") 'w3m-goto-url-mouse)
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

(defconst java-keys-alist
  '((:load cc-mode
     :map java-mode-map
     :kill ("(" "{" "C-c C-w"))))
(set-keys-in java-keys-alist)

(defconst rust-keys-alist
  '((:load rust-mode
     :map rust-mode-map
     :assign (("<C-tab>" rust-format-buffer)))))
(set-keys-in rust-keys-alist)
(defconst racer-keys-alist
  '((:load racer-mode
     :map racer-mode-map
     :assign (("C-h d" racer-describe)))))
(set-keys-in racer-keys-alist)

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
            (define-key c-mode-map (kbd "C-c C-o") #'c-insert-block)
            (define-key c++-mode-map (kbd "C-c C-o") #'cxx-insert-block)
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
(defconst js-mode-maps (list js-mode-map js2-mode-map))
(with-eval-after-spec (js-mode js2-mode)
  (define-key js2-mode-map (kbd "C-c C-w") nil)
  (add-keybinding-to-mode-maps "C-<tab>" #'web-beautify-js js-mode-maps)
  (add-keybinding-to-mode-maps
   "RET" #'js-newline-indent-for-real js-mode-maps))

(with-eval-after-spec skewer-mode
  (define-key skewer-mode-map (kbd "C-M-x") #'skewer-eval-buffer-or-region))

;;; css
(with-eval-after-spec css-mode
  (define-key css-mode-map (kbd "C-<tab>") #'web-beautify-css))

;;; CPerl-mode
(defconst cperl-keys-alist
  '((:load cperl-mode
     :map cperl-mode-map
     :kill ("C-c C-k" "C-c C-w" "C-c C-v")
     :assign (("C-h f" cperl-perldoc)
              ("C-c <tab>" cperl-linefeed)))))
(set-keys-in cperl-keys-alist)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-M-x") #'eval-buffer-and-message)
(define-key emacs-lisp-mode-map (kbd "<return>") #'newline)
(define-key paredit-mode-map (kbd "M-q") #'comment-fill-paragraph)
(define-key paredit-mode-map (kbd "M-.") nil)
(define-key paredit-mode-map (kbd "M-?") nil)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'view-macro-expansion)
(define-key emacs-lisp-mode-map (kbd "C-c C-j") #'eval-sexp-and-newline)
(define-key lisp-interaction-mode-map (kbd "C-c C-j") #'eval-sexp-and-newline)
(define-key emacs-lisp-mode-map (kbd "C-M-d") #'eval-defun)
(define-key paredit-mode-map (kbd "C-M-d") nil)
;;; TODO: make it use emacs-lisp-mode and other fun keybindings, along with
;;; making `edebug-eval-expression' use its OWN history
;; (define-key read-expression-map (kbd ""))

(with-eval-after-spec edebug
  ;; TODO
  ;; (define-key edebug-mode-map (kbd "e") #'my-edebug-eval-nicely)
  (define-key emacs-lisp-mode-map (kbd "C-S-d") #'edebug-defun)
  (define-key paredit-mode-map (kbd "C-S-d") nil))
(require 'edebug)

;;; lisp
;;; so it's all emacsy
(with-eval-after-spec slime
  (add-hook 'slime-repl-mode-hook #'enable-paredit-mode)
  (define-key lisp-mode-map (kbd "C-h f") 'slime-documentation)
  (define-key lisp-mode-map (kbd "C-h v") 'slime-documentation)
  (define-key lisp-mode-map (kbd "C-h h") 'slime-hyperspec-lookup)
  (when (boundp 'slime-mode-indirect-map)
    (define-key slime-mode-indirect-map (kbd "M-p")
      #'mc/mark-previous-like-this)
    (define-key slime-mode-indirect-map (kbd "M-n")
      #'mc/mark-next-like-this)))

;;; makefile
(defconst makefile-keys-alist
  '((:load make-mode
     :map (makefile-gmake-mode-map makefile-bsdmake-mode-map)
     :kill ("M-n" "M-p"))))
(set-keys-in makefile-keys-alist)

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

(defun my-org-up-section ()
  (interactive)
  (cond ((org-at-item-p)
         (org-beginning-of-item-list)
         (org-up-element)
         (-when-let* ((struct (org-list-struct))
                      (cur (car (member (assq (point-at-bol) struct) struct)))
                      (indent (cl-second cur)))
           (forward-char indent)))
        (t (org-up-element))))
(defun my-org-down-section ()
  (interactive)
  (if (org-at-heading-p) (org-down-element)
    (-if-let* ((struct (org-list-struct)))
        (-if-let* ((bol (point-at-bol))
                   (subitem (nth 1 (member (assq bol struct) struct)))
                   (st
                    (non-nil-and-equal
                     (org-list-has-child-p bol struct)
                     (cl-first subitem)))
                   (indent (cl-second subitem)))
            (progn
              (-when-let*
                  ((eol (point-at-eol))
                   (invisible-olay
                    (cl-find-if
                     (lambda (olay) (= (overlay-start olay) eol))
                     (overlays-at st)))
                   (invisible-prop (overlay-get invisible-olay 'invisible)))
                (org-list-set-item-visibility bol struct 'children))
              (goto-char st)
              (forward-char indent))
          (message "%s" "No deeper list item!"))
      (message "%s" "Can't go down further!"))))

(defun my-org-next-section ()
  (interactive)
  (if (org-at-item-p)
      (let* ((struct (org-list-struct))
             (cur-rest
              (cl-loop with bol = (point-at-bol)
                       for head = struct then (cdr head)
                       until (or (null head)
                                 (eq (caar head) bol))
                       finally (return head)))
             (cur (car cur-rest))
             (indent (cl-second cur))
             (next
              (cl-loop for item in (cdr cur-rest)
                       if (< (cl-second item) indent) return nil
                       until (eq (cl-second item) indent)
                       finally (return item))))
        (if next
            (progn
              (goto-char (cl-first next))
              (forward-char indent))
          (message "%s" "No more list items!")))
    (org-forward-element)))
(defun my-org-previous-section ()
  (interactive)
  (if (org-at-item-p)
      (let* ((struct (reverse (org-list-struct)))
             (cur-rest
              (cl-loop with bol = (point-at-bol)
                       for head = struct then (cdr head)
                       until (or (null head)
                                 (eq (caar head) bol))
                       finally (return head)))
             (cur (car cur-rest))
             (indent (cl-second cur))
             (next
              (cl-loop for item in (cdr cur-rest)
                       if (< (cl-second item) indent) return nil
                       until (eq (cl-second item) indent)
                       finally (return item))))
        (if next
            (progn
              (goto-char (cl-first next))
              (forward-char indent))
          (message "%s" "No previous list items!")))
    (org-backward-element)))

(defun my-org-beginning-of-section ()
  (interactive)
  (if (org-at-item-p)
      (org-beginning-of-item-list)
    (org-previous-visible-heading 1)))
(defun my-org-end-of-section ()
  (interactive)
  (if (org-at-item-p)
      (org-end-of-item-list)
    (org-next-visible-heading 1))
  (backward-char))

(defun my-org-previous-element ()
  (interactive)
  (when (org-at-item-p)
    (goto-char (caar (org-list-struct))))
  (org-backward-element))
(defun my-org-next-element ()
  (interactive)
  (when (org-at-item-p)
    (goto-char (caar (reverse (org-list-struct)))))
  (org-forward-element))

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
     ;; (define-key org-mode-map (kbd "C-p") #'outline-previous-heading)
     ;; (define-key org-mode-map (kbd "C-n") #'outline-next-heading)
     (define-key org-mode-map (kbd "C-p") #'my-org-previous-section)
     (define-key org-mode-map (kbd "C-n") #'my-org-next-section)
     (define-key org-mode-map (kbd "C-b") #'my-org-up-section)
     (define-key org-mode-map (kbd "C-f") #'my-org-down-section)
     (define-key org-mode-map (kbd "C-M-u") #'outline-up-heading)
     (define-key org-mode-map (kbd "M-a") nil)
     (define-key org-mode-map (kbd "M-a <up>") #'my-org-previous-section)
     (define-key org-mode-map (kbd "M-a <down>") #'my-org-next-section)
     (define-key org-mode-map (kbd "M-a <left>") #'my-org-up-section)
     (define-key org-mode-map (kbd "M-a <right>") #'my-org-down-section)
     (define-key org-mode-map (kbd "M-a M-<up>") #'my-org-previous-section)
     (define-key org-mode-map (kbd "M-a M-<down>") #'my-org-next-section)
     (define-key org-mode-map (kbd "M-a M-<left>") #'my-org-up-section)
     (define-key org-mode-map (kbd "M-a M-<right>") #'my-org-down-section)
     (define-key org-mode-map (kbd "M-a M-a") #'my-org-beginning-of-section)
     (define-key org-mode-map (kbd "M-a M-e") #'my-org-end-of-section)
     (define-key org-mode-map (kbd "M-a M-n") #'my-org-next-element)
     (define-key org-mode-map (kbd "M-a M-p") #'my-org-previous-element)))

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
(with-eval-after-spec markdown-mode
  (define-key markdown-mode-map (kbd "M-n") #'mc/mark-next-like-this)
  (define-key markdown-mode-map (kbd "M-p") #'mc/mark-previous-like-this)
  (define-key markdown-mode-map (kbd "`") #'markdown-literal-region-too)
  (define-key markdown-mode-map (kbd "C-k") #'kill-line-or-region)
  (define-key markdown-mode-map (kbd "C-c C-c") #'markdown-send-to-shell)
  (define-key markdown-mode-map (kbd "C-c C-z") #'markdown-switch-shell)
  (define-key markdown-mode-map (kbd "C-c C-b") #'rmd-export-pdf)
  (define-key markdown-mode-map (kbd "<M-right>") #'markdown-demote-list-item)
  (define-key markdown-mode-map (kbd "<M-left>") #'markdown-promote-list-item)
  (define-key markdown-mode-map (kbd "<M-up>") #'markdown-move-list-item-up)
  (define-key markdown-mode-map (kbd "<M-down>")
    #'markdown-move-list-item-down))

(with-eval-after-spec grep
  (define-key grep-mode-map (kbd "G") #'refind-or-grep)
  (define-key grep-mode-map (kbd "k") #'kill-grep))

;;; help/info/etc
;;; TODO: add option to split current window instead of poppping to new!
(cl-defun find-function-switch-pfx (cmd &key invert pfx-spec nomark then-do)
  (declare (indent 1))
  (lambda (&optional pfx)
    (interactive "P")
    (let ((result (switch-window-prep-fn pfx cmd pfx-spec invert nomark)))
      (when (functionp then-do)
        (pcase result
          (`(,window ,buffer)
           (funcall then-do window buffer))
          (_ nil))))))

(defconst function-help-keys-alist
  `((:map (nil)
     :kill ("C-M-h")
     :assign
     (("C-h f" ,(find-function-switch-pfx
                    (make-new-help #'describe-function)
                  :invert t))
      ("C-h v" ,(find-function-switch-pfx
                    (make-new-help #'describe-variable)
                  :invert t))
      ("C-h k" ,(find-function-switch-pfx
                    (make-new-help #'describe-key)
                  :invert t))
      ("C-h C-k" ,(find-function-switch-pfx
                      (make-new-help #'describe-key)
                    :invert t))
      ("C-h d" ,(find-function-switch-pfx
                   (make-new-help #'describe-function-or-variable)
                 :invert t))
      ("C-M-h d" ,(find-function-switch-pfx
                      (make-new-help #'describe-function-or-variable-at-point)
                    :invert t))
      ("C-h C-d" ,(find-function-switch-pfx #'find-function-or-variable))
      ("C-M-h C-d"
       ,(find-function-switch-pfx #'find-function-or-variable-at-point))
      ("C-M-h f"
       ,(find-function-switch-pfx (make-new-help #'describe-function-at-point)))
      ("C-M-h v"
       ,(find-function-switch-pfx (make-new-help #'describe-variable-at-point)))
      ("C-h C-f" ,(find-function-switch-pfx #'find-function))
      ("C-M-h C-f" ,(find-function-switch-pfx #'find-function-at-point))
      ("C-h C-v" ,(find-function-switch-pfx #'find-variable))
      ("C-M-h C-v" ,(find-function-switch-pfx #'find-variable-at-point))
      ("C-h b" ,(find-function-switch-pfx
                    (make-new-help #'describe-bindings)
                  :invert t))
      ("C-h l"
       ,(find-function-switch-pfx (make-new-help #'view-lossage) :invert t))))))
(set-keys-in function-help-keys-alist)

;;; c-h a -> apropos in help map
(define-key help-map "a"
  (find-function-switch-pfx #'apropos
    :then-do (lambda (win buf)
               (with-selected-window win
                 (with-current-buffer buf
                   (set-window-buffer win buf)
                   (set-window-point win (point-min)))))))

;;; FIXME: this doesn't work at all and it's stupid
;; (with-eval-after-spec help
;;   (define-key help-mode-map [remap push-button] #'help-do-button))
;;; FIXME: add link follow for ess help

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
(global-set-key (kbd "C-x h") #'pop-to-mark-command)
(global-set-key (kbd "C-x C-h") #'pop-buf-or-global-mark)
(global-set-key (kbd "C-x M-h") #'unpop-to-mark-command)
(global-set-key (kbd "C-x C-p") #'previous-buffer)
(global-set-key (kbd "C-x M-p") #'next-buffer)

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
(with-eval-after-spec magit
  (global-set-key (kbd "C-c g")
                  (find-function-switch-pfx #'magit-status :invert t))
  (global-set-key (kbd "C-c b") #'magit-blame)
  (define-key magit-mode-map (kbd "<tab>") #'magit-tab-dwim)
  (define-key magit-mode-map (kbd "<backtab>") #'magit-section-cycle-global)
  (define-key magit-status-mode-map (kbd "C-w") nil))
(with-eval-after-spec magit-blame
  (define-key magit-blame-mode-map (kbd "c") #'magit-show-commit))
(with-eval-after-spec magit-process
  (define-key magit-process-mode-map (kbd "k") #'magit-process-kill))

;;; LOL MAIL
(global-set-key (kbd "C-x m") nil)

(with-eval-after-spec literate-coffee-mode
  (define-key litcoffee-mode-map (kbd "C-c C-v") #'litcoffee-toggle-code-prose))

;;; haskell
(defconst hoogle-base-url "https://www.haskell.org/hoogle/")

(defvar hoogle-internet-history nil)

(defun hoogle-internet (query)
  (interactive
   (list
    (let ((def (not-whitespace-at-point)))
      (read-string
       (format "hoogle (default '%s'): " def)
       nil
       'hoogle-internet-history
       def))))
  (browse-url (url-encode-url (format "%s?hoogle=%s" hoogle-base-url query))))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "<return>")
       #'haskell-indentation-newline-and-indent)
     (define-key haskell-mode-map (kbd "C-c C-v") nil)
     (define-key haskell-mode-map (kbd "C-h f") #'hoogle-internet)))

;;; compilation
(define-key compilation-mode-map (kbd "g")
  (lambda (pfx)
    (interactive "P")
    (compilation-start (car compilation-arguments) (consp pfx))))
(define-key compilation-mode-map (kbd "G")
  (lambda () (interactive) (recompile t)))
(define-key compilation-mode-map (kbd "k") #'kill-compilation)
(define-key compilation-mode-map (kbd "p")
  (lambda ()
    (interactive)
    (compilation-previous-error 1)
    (recenter)))
(define-key compilation-mode-map (kbd "n")
  (lambda ()
    (interactive)
    (compilation-next-error 1)
    (recenter)))
(define-key compilation-mode-map (kbd "h") nil)
(define-key compilation-mode-map (kbd "b") #'back-to-window-before-compilation)
(global-set-key (kbd "C-c C-c b") #'back-to-compilation-window)
(global-set-key (kbd "C-c C-c w") #'reset-window-before-compilation)

;;; paredit
(define-key paredit-mode-map (kbd "M-t") 'transpose-sexps)
(define-key paredit-mode-map (kbd "M-;") 'fix-paredit-comment-dwim)
(define-key paredit-mode-map (kbd "C-M-<left>") 'windmove-left)
(define-key paredit-mode-map (kbd "C-M-<right>") 'windmove-right)
(define-key paredit-mode-map (kbd "C-<right>") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-c <right>") 'paredit-forward)
(define-key paredit-mode-map (kbd "C-<left>") 'paredit-backward)
(define-key paredit-mode-map (kbd "C-c <left>") 'paredit-backward)
(define-key paredit-mode-map (kbd "M-<right>") #'right-sexp-or-camel)
(define-key paredit-mode-map (kbd "M-<left>") #'left-sexp-or-camel)
(define-key paredit-mode-map (kbd "M-a") nil)
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
(eval-after-load 'slime
   '(progn
      (define-key slime-mode-indirect-map (kbd "C-M-a") nil)
      (define-key slime-mode-indirect-map (kbd "C-c C-v") #'delete-whole-line)
      (define-key slime-repl-mode-map (kbd "M-s") nil)))
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

(defun my-paredit-kill (&optional arg)
  (interactive "P")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (paredit-kill arg)))
(define-key paredit-mode-map (kbd "C-k") #'my-paredit-kill)

;;; comint!
(defun my-comint-move-prompt-region (fn)
  (lambda (N)
    (interactive "^p")
    (funcall-interactively fn N)))

(define-key comint-mode-map (kbd "M-a")
  (my-comint-move-prompt-region #'comint-previous-prompt))
(define-key comint-mode-map (kbd "M-m")
  (my-comint-move-prompt-region #'comint-next-prompt))

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
(global-set-key (kbd "C-c d") #'git-gutter:diff-and-switch)
(global-set-key (kbd "C-c r") #'git-gutter:revert-hunk)
(global-set-key (kbd "<home>") #'beg-of-line-text)
(global-set-key (kbd "<end>") #'end-of-maybe-visual-line)
(global-set-key (kbd "C-a") #'beg-of-line-text)
(global-set-key (kbd "C-e") #'end-of-maybe-visual-line)
(global-set-key (kbd "C-M-S-a") #'beginning-of-line)
(global-set-key (kbd "C-M-S-e") #'end-of-line)

(global-set-key (kbd "C-c C-v") #'delete-whole-line)
(global-set-key (kbd "C-x C-r") #'revert-buffer-no-confirm)

(eval-after-load 'python
  '(progn
     (define-key python-mode-map (kbd "C-c C-v") #'delete-whole-line)))

(eval-after-load 'tex-mode
  '(progn
     (define-key tex-mode-map (kbd "C-c C-w") nil)
     (define-key tex-mode-map (kbd "C-c C-v") nil)
     (define-key tex-mode-map (kbd "C-c e") #'latex-compile)
     (define-key tex-mode-map (kbd "C-c C-b") #'bibtex-compile)
     (progn
       ;; TODO: make auctex fix this shit
       (defconst LaTeX-mode-map latex-mode-map)
       (define-key LaTeX-mode-map (kbd "C-c C-w") nil)
       (define-key LaTeX-mode-map (kbd "C-c C-v") nil)
       (define-key LaTeX-mode-map (kbd "C-k") #'kill-line-or-region)
       (define-key LaTeX-mode-map (kbd "\"") #'TeX-quote-region)
       (define-key LaTeX-mode-map (kbd "C-c m") #'latex-insert-math)
       (define-key LaTeX-mode-map (kbd "$") #'latex-insert-math)
       (define-key LaTeX-mode-map (kbd "C-c C-s")
         #'switch-to-latex-compile-output)
       (define-key LaTeX-mode-map (kbd "C-c C-c")
         #'TeX-command-master)
       (define-key LaTeX-mode-map (kbd "<C-return>") #'newline-and-indent)
       (define-key LaTeX-mode-map (kbd "C-c `") #'TeX-next-error)
       (define-key LaTeX-mode-map (kbd "C-c e") #'LaTeX-environment)
       (define-key LaTeX-mode-map (kbd "C-x C-x") #'latex-double-cash))))

(eval-after-load 'sh-script
  '(define-key sh-mode-map (kbd "C-c C-w") nil))

;; (eval-after-load 'ein-notebook
;;   '(define-key ein:notebook-mode-map (kbd ".") #'self-insert-command))

;;; this is annoying
(global-set-key (kbd "C-x C-x") nil)

(with-eval-after-spec ess
  (define-key ess-mode-map (kbd "C-c C-v") nil)
  (define-key ess-mode-map (kbd "C-x C-e") #'ess-eval-paragraph)
  (define-key ess-mode-map (kbd "C-h f") #'ess-display-help-on-object)
  (define-key ess-mode-map (kbd "<C-return>") #'newline-and-indent)
  (define-key ess-mode-map (kbd "_") #'self-insert-command)
  (define-key ess-mode-map (kbd "M-RET") nil)
  (define-key inferior-ess-mode-map (kbd "C-h f")
    #'ess-display-help-on-object)
  (define-key inferior-ess-mode-map (kbd "C-c C-w") nil)
  (define-key inferior-ess-mode-map (kbd "RET") #'my-inf-ess-end-send-input)
  (define-key ess-help-mode-map (kbd "C-h f") #'ess-display-help-on-object)
  (define-key ess-mode-map (kbd "C-c '") #'ess-show-traceback)
  (define-key ess-tracebug-map  (kbd "C-c '") #'ess-show-traceback)
  (define-key inferior-ess-mode-map  (kbd "C-c '") #'ess-show-traceback)

  ;; TODO: ess eval!!!
  ;; (define-key ess-mode-map (kbd "M-:") #'my-ess-eval-this)
  ;; TODO: use some r package functions, maybe?
  (define-key ess-r-package-mode-map (kbd "C-c C-w") nil)
  (define-key ess-mode-map (kbd "C-M-h") nil))

(global-set-key (kbd "C-:") #'eval-expression)
(global-set-key (kbd "C-S-h f") #'describe-function)
(global-set-key (kbd "C-S-h v") #'describe-variable)
(global-set-key (kbd "C-S-h d") #'describe-function-or-variable)

(with-eval-after-spec helm
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") nil)
  (define-key helm-map (kbd "C-k") nil)
  (defun helm-cycle-down ()
    (interactive)
    (let ((helm-move-to-line-cycle-in-source t))
      (call-interactively #'helm-next-line)))
  (defun helm-cycle-up ()
    (interactive)
    (let ((helm-move-to-line-cycle-in-source t))
      (call-interactively #'helm-previous-line)))
  (define-key helm-map (kbd "<down>") #'helm-cycle-down)
  (define-key helm-map (kbd "C-n") #'helm-cycle-down)
  (define-key helm-map (kbd "<up>") #'helm-cycle-up)
  (define-key helm-map (kbd "C-p") #'helm-cycle-up))

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

(eval-after-load 'prolog
  '(progn (define-key prolog-mode-map (kbd "C-c C-v") nil)))

(global-set-key (kbd "C-w") nil)
(global-set-key (kbd "C-w c") #'cycle-shell-buffers)
(global-set-key (kbd "C-w C-c") #'cycle-shell-buffers)

(global-set-key (kbd "C-c k") #'do-keys-for-line)
(global-set-key (kbd "C-c f") #'do-for-line)

(global-set-key (kbd "M-RET") #'newline-continue-comment)

(with-eval-after-spec cc-mode
  (define-key c-mode-base-map (kbd "C-c C-a") nil)
  (define-key c-mode-base-map (kbd "C-c C-r") nil)
  (define-key c-mode-base-map (kbd "C-M-;") #'newline-and-comment))

(with-eval-after-spec ag
  (define-key ag-mode-map (kbd "g") #'re-ag-reset-args-and-recompile)
  (define-key ag-mode-map (kbd "G") #'re-ag))

(with-eval-after-spec helm-rg
  (global-set-key (kbd "C-c a") #'helm-rg)
  (global-set-key (kbd "C-c C-a") #'helm-rg))

(global-set-key (kbd "M-y") #'yank-pop)
(global-set-key (kbd "C-M-y") #'helm-show-kill-ring)
(define-key paredit-mode-map (kbd "C-M-y") #'helm-show-kill-ring)

(global-set-key (kbd "C-w b") #'push-buffer-to-kill-ring)

(define-key diff-mode-map (kbd "M-w") #'diff-mode-copy)
(define-key diff-mode-map (kbd "C-k") #'diff-mode-copy)
(define-key magit-mode-map (kbd "M-w") #'diff-mode-copy)
(define-key magit-mode-map (kbd "C-k") #'diff-mode-copy)
(with-eval-after-load 'git-rebase-mode
  (define-key git-rebase-mode-map (kbd "C-z") #'git-rebase-undo))

(global-set-key (kbd "C-M-h i") #'helm-info)
(global-set-key (kbd "<M-home>") #'beginning-of-buffer)
(global-set-key (kbd "<M-end>") #'end-of-buffer)
(global-set-key (kbd "C-c R") #'resurrect-buffer-from-file)

(with-eval-after-spec nxml-mode
  (define-key nxml-mode-map (kbd "C-c C-v") nil)
  (define-key nxml-mode-map (kbd "C-<tab>") #'xml-fmt))

(global-set-key (kbd "C-x C-M-h") #'run-shell)
(global-set-key (kbd "C-x C-p") #'restart-shell)

(define-key shell-mode-map (kbd "C-c C-w") #'destroy-all-whitespace-nearby)
(define-key shell-mode-map (kbd "<C-up>") nil)
(define-key shell-mode-map (kbd "<C-down>") nil)

(defun slime-eval-buffer-or-region ()
  (interactive)
  (if (use-region-p)
      (prog1 (call-interactively #'slime-eval-region)
        (message "evaluated region"))
    (prog1 (call-interactively #'slime-eval-buffer)
      (message "evaluated buffer %s" (buffer-name)))))
(eval-after-load 'slime
  '(progn
     (define-key slime-mode-map (kbd "C-c C-w") nil)
     (define-key slime-mode-map (kbd "C-M-x") #'slime-eval-buffer-or-region)))

(with-eval-after-load 'ensime-mode
  (define-key ensime-mode-map (kbd "C-c C-v") #'delete-whole-line)
  (define-key ensime-mode-map (kbd "M-n") #'mc/mark-next-like-this)
  (define-key ensime-mode-map (kbd "M-p") #'mc/mark-previous-like-this))

(define-key prog-mode-map (kbd "C-c C-f") #'center-function)

(global-set-key (kbd "C-v") #'delete-whole-line)

(with-eval-after-load 'scala-mode
  (define-key scala-mode-map (kbd "M-q") #'comment-fill-paragraph))

(with-eval-after-load 'ensime
  (define-key ensime-mode-map (kbd "C-c e") #'ensime-print-errors-at-point)
  (define-key ensime-mode-map (kbd "C-c t") #'ensime-type-at-point)
  (define-key ensime-mode-map (kbd "C-c C-e")
    #'ensime-show-all-errors-and-warnings)
  (define-key ensime-mode-map (kbd "C-c l") #'ensime-inf-eval-buffer))

(defun highlight-to-begin ()
  (interactive "^")
  (call-interactively #'beginning-of-buffer)
  (call-interactively #'exchange-point-and-mark)
  (call-interactively #'exchange-point-and-mark))
(defun highlight-to-end ()
  (interactive "^")
  (call-interactively #'end-of-buffer)
  (call-interactively #'exchange-point-and-mark)
  (call-interactively #'exchange-point-and-mark))

(global-set-key (kbd "C-M-<") #'highlight-to-begin)
(global-set-key (kbd "C-M->") #'highlight-to-end)

;;; isearch
(define-key isearch-mode-map (kbd "<backspace>") #'isearch-del-char)
(define-key isearch-mode-map (kbd "M-l") #'isearch-toggle-normal)

(define-key Info-mode-map (kbd "b") #'Info-history-back)
(define-key Info-mode-map (kbd "f") #'Info-history-forward)

(global-set-key (kbd "M-=") #'count-chars-words-lines-buffer)
(global-set-key (kbd "M-T") #'transpose-lines)
