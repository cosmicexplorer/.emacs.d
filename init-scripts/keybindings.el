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
;;; cause otherwise this doesn't work in graphical mode
(global-set-key (kbd "<C-return>") 'newline-and-indent)
;;; just destroy unused files
(global-set-key (kbd "C-x d") 'kill-buffer-and-move-file-to-trash)
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
;;; TODO: doesn't work (used to though)
(global-set-key (kbd "C-x M-c") 'toggle-letter-case)
;;; kill buffer without prompting
(global-set-key (kbd "C-x k") 'kill-this-buffer)
;;; functions to save point in a file; for example, when you have a place you
;;; are modifying but often need to move somewhere else and return back, and you
;;; don't have enough screen space to split screen constantly
(global-set-key (kbd "C-c C-s") 'save-point)
(global-set-key (kbd "C-c C-a") 'goto-saved-point)

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
;;; cause this needs to exist
(define-key dired-mode-map "c" 'find-file)
;;; mildly useful
(define-key dired-mode-map (kbd "F") 'dired-do-find-marked-files)

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
;; adjusting pane size
(global-set-key (kbd "C-x <home>") 'enlarge-window) ;; increase window height
(global-set-key (kbd "C-x <end>") 'shrink-window) ;; decrease window height
(global-set-key (kbd "C-x <prior>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-x <next>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x RET") 'shrink-window-if-larger-than-buffer) ;; shrink
;; window to fit content
(global-set-key (kbd "C-x !") 'balance-windows) ;; make all windows same height
;;; move among panes in a way that isn't totally fucked
(setq windmove-wrap-around t)
;; original meta
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

;; translate stuff into hex (and back??)
(global-set-key (kbd "C-x h") 'hexl-mode)

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

;; kill all active dired buffers at once
;;; TODO: make this kill all buffers that tend to accumulate (helm/dired/tramp)
(global-set-key (kbd "C-x M-d") 'kill-dired-buffers)

;; open new file with given filename from minibuffer, or blank filename
(global-set-key (kbd "C-x C-n") 'open-new-file)

;; bind the key that doesn't do dired but should go to dired
;;; (C-x d is set to something else)
(global-set-key (kbd "C-x C-d") 'dired)

;; multiple cursors fun!!!
(global-set-key (kbd "C-x C-l") 'mc/edit-lines)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-M-n") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-p") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-x C-a") 'mc/mark-all-like-this)

;; gofmt!!!
(add-hook 'go-mode-hook
          (lambda () (local-set-key (kbd "C-c f") 'go-fmt-file)))

;;; search all open buffers for regexp
;;; (really helm-swoop is better but if you need full POSIX regex then ok)
(global-set-key (kbd "C-c M-r") 'search-all-buffers)

;;; c/c++/java
(add-hook 'c-initialization-hook
	  (lambda ()
	    (define-key c-mode-map (kbd "C-j") 'newline-and-indent-ctrl-j)
	    (define-key c-mode-map (kbd "<C-return>")
              'newline-and-indent-ctrl-j)
	    (define-key c++-mode-map (kbd "C-j") 'newline-and-indent-ctrl-j)
	    (define-key c++-mode-map (kbd "<C-return>")
              'newline-and-indent-ctrl-j)
	    (define-key c++-mode-map (kbd "{") 'insert-brackets)))

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)
;;; cool but never used cause lol search key
(global-set-key (kbd "<XF86Search>") 'helm-multi-swoop-all)

;; TODO: make this work!
;; (add-hook 'org-mode-hook
;;           (define-key org-mode-map (kbd "<tab>") 'smart-tab))

;;; coffeescript
(with-eval-after-load "coffee-mode"
 'after-load-functions
 (define-key coffee-mode-map (kbd "C-c C-k") 'smart-compile)
 (define-key coffee-mode-map (kbd "C-c C-c") 'coffee-compile-buffer))

;;; js
(with-eval-after-load "js.el"
  (define-key js-mode-map (kbd "RET") 'newline-and-indent-fix-js-mode))