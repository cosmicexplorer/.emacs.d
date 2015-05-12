;;; some of these are mine, some are heavily adapted from emacswiki, some are
;;; copy/paste from emacswiki
;;; macros at bottom

;;; helper function used for loading custom scripts littered throughout here
(defun local-file-path (filename)
  (concat (expand-file-name
           (if load-file-name
               (file-name-directory (file-truename load-file-name))
             default-directory))
   "/" filename))

;;; helper function often used in keybinding mappings
(defun add-keybinding-to-mode-maps (keys-pressed func-to-call-quoted
                                                 &rest mode-maps-list)
  "Adds function to a given list of mode maps upon pressing a given key."
  (interactive)
  (loop for mode-map in mode-maps-list
        do (define-key mode-map (kbd keys-pressed) func-to-call-quoted)))

;;; so that there's a space after inline comments in coffeescript
(defun coffeescript-comment-do-what-i-really-mean (arg)
  (interactive "*P")
  (coffee-comment-dwim arg)
  (when (char-equal (char-before) 35)   ; 35 == '#'
    (insert " ")))

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
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name))
                 (not
                  (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

(defcustom search-all-buffers-ignored-files
  (list (rx-to-string
         '(and
           bos
           (or
            ".bash_history"
            "TAGS")
           eos)))
  "Files to ignore when searching buffers via \\[search-all-buffers]."
  :type 'editable-list)

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun newline-continue-comment ()
  (interactive)
  (if (point-in-comment)
      (comment-indent-new-line)
    (newline-and-indent)))

;; kill current buffer and close pane
(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it
also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
      (delete-window)))

;; open new file (i wrote this!!!)
(defun open-new-file ()
  (interactive)
  (switch-to-buffer ;; switches to operating buffer
   (generate-new-buffer ;; creates new buffer with given name
    (generate-new-buffer-name
     (read-string "new filename: " ;; reads from minibuffer, with given default
                  ;; value
                  nil nil "*newbuf*")))) ;; with default title *newbuf*
  (normal-mode))

(defun massive-text ()
  (interactive)
  (set-face-attribute 'default nil :height 400))

(defun big-text ()
  (interactive)
  (set-face-attribute 'default nil :height 200))

(defun little-text ()
  (interactive)
  (set-face-attribute 'default nil :height 100))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;; helper functions for toggle-letter-case
;;; https://stackoverflow.com/questions/27798296/check-if-a-character-not-string-is-lowercase-uppercase-alphanumeric
(defun wordp (c)
  (= ?w (char-syntax c)))
(defun lowercasep (c)
  (and (wordp c) (= c (downcase c))))
(defun uppercasep (c)
  (and (wordp c) (= c (upcase c))))
(defun whitespacep (c)
  (= 32 (char-syntax c)))
(defun string-capitalized-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]*\\'" str)))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: lowercase->ALL CAPS->Initial Caps->(cycle)."
  (interactive)
  (let ((orig-pt (point)))
    (let* ((beg (region-beginning))
           (end (region-end))
           (reg (buffer-substring-no-properties beg end))
           (case-fold-search nil))
      (cond
       ((not (string-match "[A-Z]" reg))
        ;; if no uppercase (all lowercase)
        ;; -> ALL CAPS
        (loop
         ;; not sure why the 1- is required
         ;; i think it's some silly intricacy of emacs region selection
         for pt from 0 upto (1- (- end beg))
         with next-char = 0
         do (progn
              (setq next-char (aref reg pt))
              (when (lowercasep next-char)
                (goto-char (+ beg pt))
                ;; delete, then insert; net zero change
                (delete-char 1)
                (insert-char (upcase next-char) 1 t)))))
       ((not (string-match "[a-z]" reg))
        ;; if no lowercase (all uppercase)
        ;; -> Initial Caps
        (let ((before-first-char (char-before beg))
              (first-char (char-after beg)))
          (when (and (not (wordp before-first-char))
                   (lowercasep first-char))
            (goto-char beg)
            (delete-char 1)
            (insert-char (upcase first-char) 1 t)))
        (loop
         for pt from 1 upto (1- (- end beg))
         with next-char = 0 and prev-char = 0
         do (progn
              (setq next-char (aref reg pt)
                    prev-char (aref reg (1- pt)))
              (cond ((and (not (wordp prev-char))
                          (lowercasep next-char))
                     (goto-char (+ beg pt))
                     (delete-char 1)
                     (insert-char (downcase next-char)))
                    ((and (wordp prev-char)
                          (uppercasep next-char))
                     (goto-char (+ beg pt))
                     (delete-char 1)
                     (insert-char (downcase next-char) 1 t))))))
       (t
        ;; if mixture of upper/lowercase, "assume" Init Caps
        ;; -> lowercase
        (loop
         for pt from 0 upto (1- (- end beg))
         with next-char = 0
         do (progn
              (setq next-char (aref reg pt))
              (when (uppercasep next-char)
                (goto-char (+ beg pt))
                (delete-char 1)
                (insert-char (downcase next-char) 1 t))))))
      (goto-char orig-pt)
      (set-mark end))))

(defadvice toggle-letter-case (after deactivate-mark-nil activate)
  (when (called-interactively-p)
    (setq deactivate-mark nil)))

(defun newline-and-indent-fix-cc-mode ()
  "cc-mode's indentation procedures upon adding a new bracket or paren are
annoying. This fixes that."
  (interactive)
  (insert-char 97)
  (insert-char 59)
  (clang-format-line)                   ; clang-formats previous line
  (delete-backward-char 2))

(defun newline-and-indent-fix-js-mode ()
  (interactive)
  (newline)
  (insert-char 97)
  (insert-char 59)
  (web-beautify-format-region
   web-beautify-js-program
   (get-beginning-of-prev-line)
   (get-end-of-next-line))
  (forward-line 1)
  (goto-char (line-end-position))
  (delete-backward-char 2))

(defun count-num-lines-in-buffer ()
  (count-lines (point-min) (point-max)))

(defun get-lines-in-buffer-str ()
  (interactive)
  (int-to-string integer-buffer-line-count))

(defun get-current-line-as-string ()
  (interactive)
  (buffer-substring-no-properties
   (line-beginning-position) (line-end-position)))

(defun move-point-to-beginning-of-line ()
  (interactive)
  (goto-char (line-beginning-position)))

(defun move-point-to-end-of-line ()
  (interactive)
  (goto-char (line-end-position)))

(defun newline-and-indent-ctrl-j ()
  (interactive)
  (let ((chars-from-end-of-line (- (line-end-position) (point))))
    (newline-and-indent)
    (insert-char 97)                    ; insert a
    (insert-char 59)                    ; insert semicolon
    (move-point-to-beginning-of-line)
    (c-indent-command)
    (move-point-to-end-of-line)
    (backward-char chars-from-end-of-line)
    (if (/= chars-from-end-of-line 0)
        (progn
          (newline-and-indent)
          (move-point-to-beginning-of-line)
          (backward-char)))
    ;; removes a and semicolon
    (delete-backward-char 2)))

(defun string-is-capitalized-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]*\\'" str)))

(defun char-is-capitalized-p (char)
  (let ((case-fold-search nil)
        (str-from-char (string char)))
    (string-match-p "\\`[A-Z]*\\'" str-from-char)))

(defun insert-brackets ()
  (interactive)
  (insert "{}")
  (backward-char 1))

(defun camel-case-right-word ()
  (interactive "^")                     ; highlights region if shifted
  (let ((cur-point (point))
        fin-point
        cap-letter-index)
    (right-word)
    (setq fin-point (point)
          cap-letter-index (- fin-point cur-point))
    ;; check if string is all caps; if so, skip
    (when (not (string-is-capitalized-p (buffer-substring cur-point fin-point)))
      (progn
        (loop for letter-index from 1 upto (- fin-point cur-point)
              while (= cap-letter-index (- fin-point cur-point))
              do (if (char-is-capitalized-p
                      (char-after (+ cur-point letter-index)))
                     (setq cap-letter-index letter-index)))
        (goto-char (+ cur-point cap-letter-index))))))

(defun camel-case-left-word ()
  (interactive "^")                     ; highlights region if shifted
  (let ((cur-point (point))
        fin-point
        cap-letter-index)
    (right-word -1)              ; used because left-word is screwy, same effect
    (setq fin-point (point)
          cap-letter-index (- fin-point cur-point))
    ;; check if string is all caps; if so, skip
    (when (not (string-is-capitalized-p (buffer-substring cur-point fin-point)))
      (progn
        (loop for letter-index from -1 downto (- fin-point cur-point)
              while (= cap-letter-index (- fin-point cur-point))
              do (if (char-is-capitalized-p
                      (char-after (+ cur-point letter-index)))
                     (setq cap-letter-index letter-index)))
        (goto-char (+ cur-point cap-letter-index))))))

(defun kill-selected-region-default (&optional lines)
  "When selection highlighted, C-k stores all characters in the kill ring,
instead of just the final line."
  (interactive "p")     ; gets beg and end from emacs as args
  (message (number-to-string lines))
  (if (use-region-p)    ; if region selected
      (kill-region (region-beginning) (region-end))
    (if (= lines 1)
        (kill-line)
      (kill-line lines))))
(global-set-key (kbd "C-k") 'kill-selected-region-default)

(defun fix-paredit-comment-dwim (&optional arg)
  "Fixed paredit-comment-dwim's behavior on inline comments."
  (interactive)
  (if arg
      (paredit-comment-dwim arg)
    (paredit-comment-dwim))
  (unless (string-match "^[\s]*;"
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
    (insert " ")))

;; get useful keybindings for lisp editing
(defun fix-lisp-keybindings ()
  "Changes about three million personalized keybindings for lisp editing with
SLIME and Paredit. Not for the faint of heart."
  (interactive)
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
  (global-set-key (kbd "RET") 'newline-and-indent) ; set as global because
                                        ; define-key doesn't work, not sure why
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
  (when (eq major-mode 'lisp-mode)
    (define-key slime-mode-indirect-map (kbd "C-M-a") nil)
    (define-key slime-mode-indirect-map (kbd "M-p") nil)
    (define-key paredit-mode-map (kbd "M-p") nil)
    (define-key slime-mode-indirect-map (kbd "M-n") nil)
    (define-key paredit-mode-map (kbd "M-n") nil))
  (when (eq major-mode 'slime-repl-mode)
    (define-key slime-repl-mode-map (kbd "M-s") nil))
  ;; so that multiple-cursors can use these
  (define-key paredit-mode-map (kbd "C-x C-l") 'mc/edit-lines)
  (define-key paredit-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key paredit-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-M-n") 'mc/unmark-next-like-this)
  (define-key paredit-mode-map (kbd "C-M-p") 'mc/unmark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key paredit-mode-map (kbd "C-M-y") 'paredit-yank-push)
  (define-key paredit-mode-map (kbd "DEL") 'paredit-backspace-delete-highlight))

;; so that paredit-kill works the way it should on regions
(defadvice paredit-kill (around paredit-kill-region-dwim)
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    ad-do-it))
(ad-activate 'paredit-kill)

;; create parens and add adjacent two elements to sexp created by parens
(defun paredit-add-parens-in-front ()
  ;; TODO: add to this later; slurp all sexps until closing paren would be very
  ;; helpful i think
  (interactive)
  (let ((sel-beg nil) (sel-end nil))
    (if (use-region-p)
        (setq sel-beg (region-beginning) sel-end (region-end))
      (progn
        (paredit-forward)
        (setq sel-end (point))
        (paredit-backward)
        (setq sel-beg (point))))
    (message (number-to-string sel-beg))
    (goto-char sel-beg)
    (paredit-open-parenthesis) ; adds closing paren too thanks to electric pair
                                        ; (or maybe it's paredit itself)
                                        ;       (paredit-forward-slurp-sexp)
    ))

(defun paredit-backspace-delete-highlight ()
  "Makes it so that backspace deletes all highlighted text in paredit mode.
Breaks the rules a little bit, but makes me a lot less insane."
  (interactive)
  (if (use-region-p)                    ; if a region is selected
      (delete-region (region-beginning) (region-end))
    (paredit-backward-delete)))

(defun paredit-remove-function-wrapper ()
  ;; TODO: this one is very imperative, not so lispy
  ;; it's really useful though so hopefully history will forgive me
  "Removes all arguments to the left of point within sexp, and removes enclosing
parentheses. CURRENTLY BROKEN"
  (interactive)
  (let ((sel-beg nil) (sel-end nil)) ; set beginning and end of selection
    (if (use-region-p)
        (setq sel-beg (region-beginning) sel-end (region-end))
      (progn
        (paredit-forward)
        (setq sel-end (point))
        (paredit-backward)
        (setq sel-beg (point))))
    (goto-char sel-beg)
    (let ((paren-counter 0))
      (loop until (and (char-equal (preceding-char) 40) ; 40 is open parenthesis
                       (= paren-counter 0))
            do (progn
                 (if (char-equal (preceding-char) 41) ; 41 is closed parenthesis
                     (incf paren-counter))
                 (if (char-equal (preceding-char) 40)   ; open paren
                     (decf paren-counter))
                 (paredit-backward-delete)
                 (decf sel-end))))
    (goto-char sel-end)
    (let ((paren-counter 0))
      (loop until (and (char-equal (following-char) 41) ; closed paren
                       (= paren-counter 0))
            do (progn
                 (if (char-equal (following-char) 40)   ; open paren
                     (incf paren-counter))
                 (if (char-equal (following-char) 41)   ; closed paren
                     (decf paren-counter))
                 (paredit-forward-delete))))
    (paredit-splice-sexp)))

(defun replace-char-in-range (from-char to-char beg end)
  (let ((was-char-replaced nil))
    (loop for index from beg upto end
          do (when (< index (point-max))
               (if
                   (char-equal (char-after index) from-char)
                   (save-excursion
                     (setq was-char-replaced t)
                     (goto-char index)
                     (delete-char 1)
                     (insert-char to-char)))))
    was-char-replaced))

(defun remove-newlines-from-buffer ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (loop for line-index from 1 upto (count-num-lines-in-buffer)
          do (progn
               (if (string-equal (get-current-line-as-string) "")
                   (delete-forward-char 1)
                 (forward-line 1))))))

(defun kill-buffer-and-move-file-to-trash ()
  "Doesn't delete the file, just moves it to /tmp so that it goes away."
  (interactive)
  (set-buffer-modified-p nil)           ; pretends buffer not modified
  (let ((msg
         (shell-command-to-string
          (concat "mv " "\"" (buffer-file-name) "\"" " /tmp/"))))
    (let ((compiled-file                  ;  save generated filename
           (cond ((or (eq major-mode 'c-mode)
                      (eq major-mode 'c++-mode))
                  (file-name-sans-extension (buffer-file-name)))
                 ((eq major-mode 'java-mode)
                  (concat
                   (file-name-sans-extension (buffer-file-name)) ".class"))
                 ((eq major-mode 'coffee-mode)
                  (concat (file-name-sans-extension (buffer-file-name)) ".js"))
                 (t nil))))
      (unless (and compiled-file
                   (file-exists-p compiled-file))
        (setq compiled-file nil))
      (if compiled-file
          (message
           (concat msg
                   (shell-command-to-string
                    (concat "mv " "\"" compiled-file "\"" " /tmp/"))))
        (message msg))))
  (kill-this-buffer))

(defun string/starts-with (s begins)
      "Return non-nil if string S starts with BEGINS."
      (cond ((>= (length s) (length begins))
             (string-equal (substring s 0 (length begins)) begins))
            (t nil)))

(defun is-buffer-beautifiable (buf-name)
  "Determines whether file is in directory specified in ~/.emacs.d/no-beautify,
  and if so, only beautifies the current line instead of the entire
  file."
  (loop with is-covered-in-file = nil
        for line in (json-read-file (concat init-home-folder-dir "no-beautify"))
        do (when (string/starts-with buf-name (cdr line))
             (setq is-covered-in-file t))
        finally (return (not is-covered-in-file))))

(defun get-beginning-of-prev-line ()
  (if (bolp)
      (point)
    (save-excursion
      (forward-line -1)
      (line-beginning-position))))

(defun get-end-of-next-line ()
  (if (eolp)
      (point)
    (save-excursion
      (forward-line 1)
      (line-end-position))))

(defun yank-push ()
  (interactive)
  (loop for i from 1 to 2               ; twice
        do (if (= (length kill-ring-yank-pointer) 0)
               nil
             (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))))
  (yank-pop))

(defun paredit-yank-push ()
  (interactive)
  (loop for i from 1 to 2               ; twice
        do (if (= (length kill-ring-yank-pointer) 0)
               nil
             (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))))
  (paredit-yank-pop))

;;; functions to save point in a file; for example, when you have a place you
;;; are modifying but often need to move somewhere else and return back, and you
;;; don't have enough screen space to split screen constantly
;;; TODO: make some functionality to keep point in the same spot when scrolling
;;; so this little hack isn't needed anymore
(let ((saved-point 1))
  (defun save-point ()
    "Saves point in a single-value register. Use goto-saved-point to return to
that point."
    (interactive)
    (setq saved-point (point))
    (message (concat "Point saved (" (number-to-string saved-point) ")")))
  (defun goto-saved-point ()
    "Moves point to a point previously saved with save-point. By default, moves
 to position 1."
    (interactive)
    (goto-char saved-point)
    (recenter)
    (message
     (concat "Moved to saved point (" (number-to-string saved-point) ")"))))

(defun load-display-time ()
  "load the display time upon invocation!"
  (setq display-time-day-and-date t)
  (display-time))

;;; macros
;;; http://stackoverflow.com/a/26137517/2518889
(defmacro with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  `(when (eq system-type ,type)
     ,@body))
(put 'with-system 'lisp-indent-function 1)

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")
(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

;;; the below two functions rely on eshell using eshell-last-input-start,
;;; eshell-last-input-end, and eshell-last-output-start internally! hopefully
;;; these won't change.........................................
;;; currently working on emacs 24.5
(when save-eshell-history
  (defun eshell-send-input-to-history ()
    (append-to-file
     (concat "--in--: " default-directory ": "
             (format-time-string current-date-time-format (current-time))
             "\n"
             (buffer-substring-no-properties
              (marker-position eshell-last-input-start)
              (point)))
     nil eshell-user-output-file)
    ;; because append-to-file cheerfully tells us that it has written to the
    ;; file, and that's annoying
    (message ""))

  (defun eshell-send-output-to-history ()
    (append-to-file
     (let* ((str
             (concat "--out--: " default-directory ": "
                     (format-time-string current-date-time-format
                                         (current-time))
                     "\n"
                     (buffer-substring-no-properties
                      (marker-position eshell-last-input-end)
                      (marker-position eshell-last-output-start))))
            (res (string-match "\n" str)))
       (if res
           str
         (concat str "\n")))
     nil eshell-user-output-file)
    (message "")))

;;; functions to save and reset window configuration to a list
;;; since i only use a single frame mostly the intricacies of multiple frames
;;; and window configurations likely won't be an issue
(defun save-current-window-configuration ()
  (interactive)
  (setq window-configuration-ring (cons (current-window-configuration)
                                        window-configuration-ring)))
(defvar cur-window-config nil)
(defun cycle-window-configuration ()
  (interactive)
  (if (not window-configuration-ring)
      (message "No window configurations saved!")
    (setq cur-window-config (cdr cur-window-config))
    (unless cur-window-config
      (setq cur-window-config window-configuration-ring)
      (message "Looped through window configuration ring!"))
    (set-window-configuration (car cur-window-config))))
(defun clear-window-configurations ()
  (interactive)
  (setq window-configuration-ring nil
        cur-window-config nil))

;;; clear out all files which no longer exist or have moved
(defun clear-all-buffers-to-deleted-files ()
  (interactive)
  (loop for buf in (buffer-list)
        do (with-current-buffer buf
             (unless
                 (or (not (buffer-file-name))
                     (file-exists-p (buffer-file-name))
                     (get-buffer-process (current-buffer))
                     (string= "*slime-repl sbcl*" (buffer-name)))
               (kill-buffer buf)))))

;;; load file into buffer asynchronously
;;; this is unfortunately somewhat useless because emacs has to stop the world
;;; to receive sentinel input from a process, so if anything it's just more
;;; annoying than waiting for find-file to complete synchronously. i've forked
;;; emacs at https://github.com/cosmicexplorer/emacs-async to provide true
;;; asynchronous support variables used for communication between process and
;;; emacs
(defvar async-load-is-buffer-modified-outside nil)
(setq-default async-load-is-buffer-modified-outside nil)
(make-variable-buffer-local 'async-load-is-buffer-modified-outside)
(defvar async-load-is-modifying-buffer nil)
(setq-default async-load-is-modifying-buffer nil)
(make-variable-buffer-local 'async-load-is-modifying-buffer)
(defvar async-load-line-count 0)
(setq-default async-load-line-count 0)
(make-variable-buffer-local 'async-load-line-count)

(defun async-load-after-change-function (beg end len)
  (unless async-load-is-modifying-buffer
    (setq async-load-is-buffer-modified-outside t)))

(defmacro with-async-load-modification (buffer &rest body)
  `(with-current-buffer ,buffer
     (setq async-load-is-modifying-buffer t)
     (save-excursion
       (goto-char (point-max))
       ,@body)
     (setq async-load-is-modifying-buffer nil)))
;;; makes it more aesthetically pleasing to use this
(put 'with-async-load-modification 'lisp-indent-function 1)

(defun async-load-file (filepath)
  (unless (and (file-exists-p filepath)
               (not (file-directory-p filepath))
               (file-readable-p filepath))
    (throw 'file-not-found (concat "specified filepath " filepath
                                   " could not be read.")))
  (let* ((process-connection-type nil)   ; use pipe
         ;; reimplementing part of the standard find-file function :(
         (arg-buffer (get-buffer-create (generate-new-buffer-name
                                         (file-name-nondirectory filepath))))
         (arg-proc (start-process
                    (concat "async-load-file: " filepath) arg-buffer
                    "cat" filepath))
         (arg-modify))
    ;; see if we can setup appropriate mode hooks before loading contents
    (with-current-buffer arg-buffer
      (set-visited-file-name filepath t)
      (normal-mode)
      (add-hook 'after-change-functions #'async-load-after-change-function))
    (set-process-filter
     arg-proc
     (lambda (proc str)
       (with-async-load-modification (process-buffer proc)
         (let ((prev-async-load-line-count async-load-line-count)
               ;; newline is ascii code 10
               (added-line-count (cl-count 10 str)))
           (insert str)
           (setq async-load-line-count (+ prev-async-load-line-count
                                          added-line-count))
           ;; update modes here because first two lines of file can often
           ;; contain fun things like file-local variables caught by normal-mode
           (when (and (< prev-async-load-line-count 2)
                      (>= async-load-line-count 2))
             (normal-mode))))))
    (set-process-sentinel
     arg-proc
     (lambda (proc ev)
       (if (string= ev "finished\n")
           ;; update modes again here after the whole file has loaded
           ;; in case file-local variables/etc have been defined again
           (with-current-buffer (process-buffer proc)
             (normal-mode)
             (set-buffer-modified-p async-load-is-buffer-modified-outside))
         ;; alert the user that SOMETHING BAD HAS HAPPENED!!!!
         (switch-to-buffer (process-buffer proc))
         (goto-char (point-max))
         (message ev))
       (remove-hook #'after-change-functions
                    #'async-load-after-change-function)))
    ;; return the process used to fill the buffer
    ;; (the actual buffer can be found from the process)
    arg-proc))


;;; save buffers to disk and get them back
(defun reread-visited-files-from-disk ()
  (with-current-buffer (find-file saved-files)
    (goto-char (point-min))
    (loop while (not (eobp))
          do (let ((cur-line (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position))))
               ;; the \" is so that windows paths with C: are parsed correctly
               (if (string-match
                    "^\\([^:]+\\):\"\\([^\"]+\\)\":\\([[:digit:]]+\\)$"
                    cur-line)
                   (let ((active-filetype
                          (match-string 1 cur-line))
                         (active-filename
                          (match-string 2 cur-line))
                         (active-point
                          (match-string 3 cur-line)))
                     (unless (or (string= cur-line saved-files)
                                 (string= cur-line ""))
                       (cond ((string= active-filetype "visit")
                              (with-current-buffer
                                  (find-file-noselect active-filename)
                                (goto-char (string-to-number active-point))))
                             (t (throw 'no-such-active-filetype
                                       (concat "i don't recognize "
                                               active-filetype "!"))))
                       ;; figure this out later!!!
                       ;; (async-load-file cur-line)
                       (message "")))
                 (with-current-buffer "*scratch*"
                   (insert (concat "couldn't parse this line of "
                                   saved-files ": \"" cur-line "\""))
                   (newline)))
               (forward-line)))
    (kill-buffer)))

(defun save-visiting-files-to-buffer ()
    (interactive)
    ;; TODO: make this more error-resistant, somehow. having to send emacs a
    ;; sigterm because this function fails on quit is annoying.
    (with-current-buffer (find-file saved-files)
         (erase-buffer)
         (loop for buf in (buffer-list)
               do (let ((bufname (buffer-file-name buf))
                        (buf-pt (with-current-buffer buf (point))))
                    (unless (or (not bufname)
                                (string-equal bufname saved-files)
                                (string-match-p "^/ssh:" bufname))
                      (insert (concat "visit" ":\""
                                      bufname "\":"
                                      (number-to-string buf-pt)))
                      (newline))))
         (save-buffer)
         (kill-buffer)))

;;; checking for features
(defmacro with-feature (feature-sym &rest body)
  ,(when (featurep ,feature-sym) ,@body))
(put 'with-feature 'lisp-indent-function 1)
