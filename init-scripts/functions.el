;;; some of these are mine, some are heavily adapted from emacswiki, some are
;;; copy/paste from emacswiki
;;; macros at bottom

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

(defun big-text ()
  (interactive)
  (set-face-attribute 'default nil :height 400))

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

;; stolen from the ergo emacs guy
;;; TODO: figure out how to make the region remain the same when shift selecting
;;; from right side!
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
                (insert-char (downcase next-char) 1 t)))))))
    (goto-char orig-pt)))

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
  (define-key paredit-mode-map (kbd "C-x C-l") 'mc/edit-lines) ; so that
                                        ; multiple-cursors can use these
  (define-key paredit-mode-map (kbd "M-n") 'mc/mark-next-like-this)
  (define-key paredit-mode-map (kbd "M-p") 'mc/mark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-M-n") 'mc/unmark-next-like-this)
  (define-key paredit-mode-map (kbd "C-M-p") 'mc/unmark-previous-like-this)
  (define-key paredit-mode-map (kbd "C-x C-a") 'mc/mark-all-like-this)
  (define-key paredit-mode-map (kbd "C-M-y") 'paredit-yank-push)
  (define-key paredit-mode-map (kbd "DEL") 'paredit-backspace-delete-highlight))

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
        for line in (json-read-file "~/.emacs.d/no-beautify")
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

(let ((saved-point 1))
  (defun save-point ()
    "Saves point in a single-value register. Use with goto-saved-point. Defaults
 to position 1."
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

;;; macros
;;; http://stackoverflow.com/a/26137517/2518889
(defmacro with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  `(when (eq system-type ,type)
     ,@body))

(defun load-display-time ()
  "load the display time upon invocation!"
  (setq display-time-day-and-date t)
  (display-time))
