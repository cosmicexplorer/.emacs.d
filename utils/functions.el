;;; some of these are mine, some are heavily adapted from emacswiki, some are
;;; copy/paste from emacswiki

(require 'cc-mode)
(require 'csharp-mode)

(defun send-message-to-scratch (msg)
  (with-current-buffer (get-buffer-create "*scratch*")
    (goto-char (point-max))
    (insert msg)
    (unless (bolp)
      (newline))))

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
  (when (and (char-before) (char-equal (char-before) 35))   ; 35 == '#'
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
  "Files to ignore when searching buffers via `search-all-buffers'
(\\[search-all-buffers])."
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

(defun medium-text ()
  (interactive)
  (set-face-attribute 'default nil :height 150))

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
;;; https://stackoverflow.com/questions/27798296/check-if-a (continued)
;;; -character-not-string-is-lowercase-uppercase-alphanumeric
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
  (when (called-interactively-p 'any)
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
              do (if (and (char-after (+ cur-point letter-index))
                          (char-is-capitalized-p
                           (char-after (+ cur-point letter-index))))
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
              do (if (and (char-after (+ cur-point letter-index))
                      (char-is-capitalized-p
                       (char-after (+ cur-point letter-index))))
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
  (interactive "P")
  (cond ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        ((numberp (ad-get-arg 0))
         (kill-line (ad-get-arg 0)))
        (t
         ad-do-it)))
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
  ;; TODO: make this work
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
          (if (eq system-type 'windows-nt)
              (concat "move " "\"" (convert-standard-filename
                                    (buffer-file-name)) "\" \""
                                    (getenv "temp") "\"")
            (concat "mv " "\"" (buffer-file-name) "\"" " /tmp/")))))
    (let ((compiled-file                  ;  save generated filename
           (cond ((or (eq major-mode 'c-mode)
                      (eq major-mode 'c++-mode)
                      (eq major-mode 'csharp-mode))
                  (file-name-sans-extension (buffer-file-name)))
                 ((eq major-mode 'java-mode)
                  (concat
                   (file-name-sans-extension (buffer-file-name)) ".class"))
                 ((eq major-mode 'coffee-mode)
                  (concat (file-name-sans-extension (buffer-file-name)) ".js"))
                 ((eq major-mode 'markdown-mode)
                  (concat
                   (file-name-sans-extension (buffer-file-name)) ".html"))
                 (t nil))))
      (unless (and compiled-file
                   (file-exists-p compiled-file))
        (setq compiled-file nil))
      (if compiled-file
          (message
           (concat msg
                   (shell-command-to-string
                    (if (eq system-type 'windows-nt)
                        (concat "move " "\"" (convert-standard-filename
                                              compiled-file) "\" "
                                (getenv "temp"))
                      (concat "mv " "\"" compiled-file "\"" " /tmp/")))))
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
(make-variable-buffer-local 'async-load-is-buffer-modified-outside)
(setq-default async-load-is-buffer-modified-outside nil)
(defvar async-load-is-modifying-buffer nil)
(make-variable-buffer-local 'async-load-is-modifying-buffer)
(setq-default async-load-is-modifying-buffer nil)

(defmacro with-async-load-modification (buffer &rest body)
  `(with-current-buffer ,buffer
     (let ((prev-pt (point)))
       (goto-char (point-max))
       ,@body
       (goto-char prev-pt))))
;;; makes it more aesthetically pleasing to use this
(put 'with-async-load-modification 'lisp-indent-function 1)

(defvar async-load-buffer-size 10000
  "Size of string before string shoved into file.")
(defvar async-load-buffer ""
  "Per-file async loading buffer.")
(make-variable-buffer-local 'async-load-buffer)
(setq-default async-load-buffer "")
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
         (setq async-load-buffer (concat async-load-buffer str))
         (when (> (length async-load-buffer) async-load-buffer-size)
           (insert async-load-buffer)
           (setq async-load-buffer "")))))
    (set-process-sentinel
     arg-proc
     (lambda (proc ev)
       ;; update modes again here after the whole file has loaded
       (with-async-load-modification (process-buffer proc)
         (insert async-load-buffer)
         (setq async-load-buffer "")
         (normal-mode)
         (set-buffer-modified-p nil))
       (unless (and (stringp ev) (string= ev "finished\n"))
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
  (with-current-buffer (find-file-noselect saved-files)
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
                     (unless (or (string= active-filename saved-files)
                                 (string= cur-line ""))
                       (cond ((string= active-filetype "visit")
                              (with-demoted-errors "save-session: %S"
                                ;; (async-load-file
                                ;;  (file-truename active-filename))
                                (with-current-buffer
                                    (find-file-noselect
                                     (file-truename active-filename))
                                  (goto-char (string-to-number active-point)))
                                 ))
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

(defvar init-loaded-fully nil
  "Set to t after init loads fully.")
(defun save-visiting-files-to-buffer (&optional is-called-interactively)
  (interactive "p")
  (when (or is-called-interactively
            ;; used so that init failures (which do not load any files from the
            ;; saved-files file) do not delete all history
            init-loaded-fully)
    ;; TODO: make this more error-resistant, somehow. having to send emacs a
    ;; sigterm because this function fails on quit is annoying.
    (with-current-buffer (find-file saved-files)
      (erase-buffer)
      (loop for buf in (buffer-list)
            do (let ((bufname (and (buffer-file-name buf) (file-truename (buffer-file-name buf))))
                     (buf-pt (with-current-buffer buf (point))))
                 (unless (or (not bufname)
                             (string-equal bufname saved-files))
                   (insert (concat "visit" ":\""
                                   bufname "\":"
                                   (number-to-string buf-pt)))
                   (newline))))
      (save-buffer)
      (kill-buffer))))

;;; checking for features
(defmacro with-feature (feature-sym &rest body)
  ,(when (featurep ,feature-sym) ,@body))
(put 'with-feature 'lisp-indent-function 1)

;;; erc prompt
(defun get-erc-prompt ()
  (concat (erc-current-nick) ">"))

(defun rejoin-erc ()
  (interactive)
  (destroy-all-erc-stuff)
  (loop for serv-tuple in erc-server-pass-alist
        do (erc :server (first serv-tuple)
                :port (or (second serv-tuple) erc-port)
                :nick (or (third serv-tuple) erc-nick)
                :password (fourth serv-tuple))))

;;; windows stuff
(defun dewindowsify-path (path)
  "Turn a windows path PATH into a format find-file (and the rest of emacs)
likes."
  ;; 92 is backslash, 47 is slash
  (map 'string (lambda (char) (if (char-equal char 92) 47 char)) path))

;;; encoding issues
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (car found)))
    found))

;;; get name of buffer and other info as required
(defun get-buffer-id (&optional buf)
  (with-current-buffer (if buf buf (current-buffer))
    (concat (buffer-name)
            (if (buffer-file-name)
                (concat ", at " (buffer-file-name))
              ""))))

(defun kill-message-buffer-id (&optional buf)
  ;; unless is emacs-internal buffer
  (unless (char-equal (aref (buffer-name) 0) 32)
    (message "%s" (concat "killed " (get-buffer-id buf)))))

;;; allow for searchable names of w3m buffers
;;; TODO: make this work
(defun w3m-rename-buf ()
  (when (and (eq major-mode 'w3m-mode)
             w3m-current-title w3m-current-url
             (not (string= w3m-current-url ""))
             (not (string= w3m-current-title "")))
    (rename-buffer
     (concat w3m-current-title ":" w3m-current-url " "
             (let ((str (buffer-name)))
               (when (string-match "\\*w3m\\*.*" str)
                 (match-string 0 str))))
     t)))

;;; org-mode stuff
(defadvice org-kill-line (around org-kill-region-advice)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    ad-do-it))
(ad-activate 'org-kill-line)

;;; TODO: make these work
;; (defadvice org-open-at-point (around org-recenter-open)
;;   (let* ((prev-pt (point))
;;          (res ad-do-it))
;;     (when (/= prev-pt (point))
;;       (message "hey")
;;       (recenter))
;;     res))
;; (ad-activate 'org-open-at-point)

;; (defadvice org-mark-ring-goto (around org-recenter-mark-goto)
;;   (let ((prev-pt (point))
;;         (res ad-do-it))
;;     (when (/= prev-pt (point))
;;       (message "lol")
;;       (recenter))
;;     res))
;; (ad-activate 'org-mark-ring-goto)

;;; string manipulation
(defun get-last-n-of-str (n str)
  (substring str (- (length str) n)))

;;; indent everything
(defun indent-regardless-of-mode ()
  (interactive)
  (if (use-region-p)
      (indent-region (region-beginning) (region-end))
    (funcall indent-line-function)))

;;; linum-relative does the same thing as this, except the first line of the
;;; let* has (diff1 (abs (- line-number linum-relative-last-pos))) and for the
;;; life of me i will never understand why the abs is there
(defun fix-linum-relative (line-number)
  "Makes useful offsets for `linum-relative', equal to the appropriate prefix
\"C-u NUM\" argument for `kill-line'. Used because `linum-relative-plusp-offset'
also affects negative line numbers, even though it says it doesn't."
  (let* ((diff1 (- line-number linum-relative-last-pos))
         (diff (if (minusp diff1) diff1 (+ diff1 linum-relative-plusp-offset)))
	 (current-p (= diff linum-relative-plusp-offset))
	 (current-symbol (if (and linum-relative-current-symbol current-p)
			     (if (string= "" linum-relative-current-symbol)
				 (number-to-string line-number)
			       linum-relative-current-symbol)
			   (number-to-string diff)))
	 (face (if current-p 'linum-relative-current-face 'linum)))
    (propertize (format linum-relative-format current-symbol) 'face face)))
(defun get-linum-relative-symbol ()
  "Makes the string `linum-relative' uses to point to the current line any one
of `linum-relative-symbols' from calling `sxhash' on the result of
`buffer-name'."
  (interactive)
  (setq linum-relative-current-symbol
        (let ((index
               (mod (sxhash (buffer-name)) (length linum-relative-symbols))))
          (substring linum-relative-symbols index (1+ index)))))

;;; commenting is dumb
;; (defun insert-string-before-each-line-in-range
;;     (str beg end &optional trim-whitespace)
;;   "Inserts STR at beginning of each line in range denoted by BEG and END. If
;; range doesn't begin at the beginning of the line, then the first line in the
;; range is not marked. Cuts off whitespace from the ends of lines if
;; TRIM-WHITESPACE is non-nil."
;;   (let ((orig-pos (point)))
;;     (goto-char beg)
;;     (loop with num-insertions-before-point = 0
;;           with total-insertion-length = 0
;;           and cur-end = end
;;           and str-length = (length str)
;;           while (< (point) cur-end)
;;           do (progn
;;                (when (bolp)
;;                  (when (<= (point) orig-pos)
;;                    (incf num-insertions-before-point str-length))
;;                  (insert str)
;;                  (incf cur-end str-length)
;;                  (incf total-insertion-length str-length)
;;                  (when trim-whitespace
;;                    (let ((prev-pt (point)))
;;                      (loop while
;;                            (and (whitespacep (char-after))
;;                                 (not (char-equal (char-after) (str2char "\n"))))
;;                            do (forward-char))
;;                      (decf total-insertion-length (- (point) prev-pt))
;;                      (decf cur-end (- (point) prev-pt))
;;                      (delete-region prev-pt (point))
;;                      (goto-char prev-pt))
;;                    ;; super inefficient lol
;;                    (nuke-trailing-whitespace)))
;;                (forward-char))
;;           finally (progn
;;                     (goto-char (+ orig-pos num-insertions-before-point))
;;                     (throw t)
;;                     (return total-insertion-length)))))

(defun insert-string-before-each-line-in-range
    (str beg end &optional trim-whitespace)
  "Inserts STR at beginning of each line in range denoted by BEG and END. If
range doesn't begin at the beginning of the line, then the first line in the
range is not marked. Cuts off whitespace from the ends of lines if
TRIM-WHITESPACE is non-nil."
  (let ((orig-pos (point)))
    (goto-char beg)
    (loop with num-insertions-before-point = 0
          with total-insertion-length = 0
          and cur-end = end
          and str-length = (length str)
          while (< (point) cur-end)
          do (progn
               (when (bolp)
                 (when (<= (point) orig-pos)
                   (incf num-insertions-before-point str-length))
                 (insert str)
                 (incf cur-end str-length)
                 (incf total-insertion-length str-length))
               (forward-char))
          finally (progn
                    (goto-char (+ orig-pos num-insertions-before-point))
                    (return total-insertion-length)))))

(defun frontier-of-text-for-line (left-or-right &optional pos)
  "Returns the position either the leftmost or rightmost character in the line
which contains POS, depending upon the value of LEFT-OR-RIGHT. If neither 'left
nor 'right is given as an argument, assumes right."
  (save-excursion
    (let ((orig-pt (if pos pos (point))))
      (goto-char orig-pt)
      (if (eq left-or-right 'left)
          (end-of-line)
        (beginning-of-line))
      (loop with final-text-char = -1
            with first-text-char = -1
            with was-final-char = nil
            until was-final-char
            do (progn
                 (unless (whitespacep (char-after (point)))
                   (when (= -1 first-text-char)
                     (setq first-text-char (point)))
                   (setq final-text-char (point)))
                 (when (or (and (eq left-or-right 'left)
                                (bolp))
                           (and (not (eq left-or-right 'left))
                                (eolp)))
                   (setq was-final-char t))
                 (if (eq left-or-right 'left) (backward-char)
                   (forward-char)))
            finally
            (return final-text-char)))))

(defun c-comment-region-stars (reg-beg reg-end num-stars-arg)
  "Comments all text within a given region between REG-BEG and REG-END, or the
current line if no region is active. NUM-STARS-ARG is given by prefix argument,
and determines the number of asterisks ('*') to insert before the initial
delimiter and after the closing comment delimiter. If a blank argument is given,
it formats the region using 'javadoc' syntax, with two stars on the initial line
and a single star for each line in between. This \"pushes\" the region affected
to the beginning of the line containing the `region-beginning', and the end of
the line containing `region-end'."
  (interactive "P")
  ;; num-stars is nil if the single-prefix argument is given, which signals
  ;; using javadoc
  (let ((num-stars (cond ((numberp num-stars-arg)
                          (if (zerop num-stars-arg) 1 num-stars-arg))
                         ((consp num-stars-arg) nil)
                         ((null num-stars-arg) 1)
                         (t (throw 'unrecognized-prefix-arg num-stars-arg))))
        ;; the definitions of beg and end could have been done in one line, but
        ;; i thought it was fun to make them super "generic"
        (beg
         (apply
          (if (use-region-p)
              (lambda (fun &rest args)
                (save-excursion
                  (goto-char reg-beg)
                  (apply fun args)))
            #'funcall)
          (list #'frontier-of-text-for-line 'left)))
        (end (apply
              (if (use-region-p)
                  (lambda (fun &rest args)
                    (goto-char
                     (if (save-excursion (goto-char (region-end)) (bolp))
                         (1- reg-end)
                       reg-end))
                    (apply fun args))
                #'funcall)
              (list #'frontier-of-text-for-line 'right)))
        (begin-insertions 0)
        (end-insertions 0)
        (star-insertions 0)
        (trim-deletions 0)
        (one-line nil))
    (setq one-line (loop for char across (buffer-substring reg-beg reg-end)
                         do (when (char-equal char (str2char "\n"))
                              (return nil))
                         finally (return t)))
    (goto-char beg)
    (let ((insert-begin-str
           (concat comment-start
                   (make-string (1- (or num-stars 2)) (str2char "*"))
                   (if num-stars comment-padding "\n"))))
      (insert insert-begin-str)
      (setq begin-insertions (length insert-begin-str)))
    (goto-char (+ end begin-insertions))
    (let ((insert-end-str
           (concat (if num-stars comment-padding "\n")
                   (make-string (1- (or num-stars 1)) (str2char "*"))
                   comment-end)))
      (insert insert-end-str)
      (setq end-insertions (length insert-end-str)))
    (unless num-stars
      (setq trim-deletions
            (trim-whitespace-in-region beg (+ end begin-insertions)))
      (with-current-buffer "*scratch*"
        (insert (number-to-string trim-deletions) "\n"))
      (setq star-insertions
            (insert-string-before-each-line-in-range
             (concat "*" comment-padding)
             (if (save-excursion (goto-char beg) (bolp)) (1+ beg) beg)
             (- (+ end begin-insertions) trim-deletions))))
     (let ((real-beg beg)
           (real-end
            (- (+ end begin-insertions star-insertions end-insertions)
               trim-deletions)))
      (c-indent-region real-beg real-end t))
    ;; c-indent-region is loud and annoying
    (message "")))

(defun c-comment-end-of-line ()
  "Meant to be set as `comment-insert-comment-function'. Performs commenting the
way I prefer, and regards `comment-padding', unlike the standard version."
  (interactive)
  (goto-char (frontier-of-text-for-line 'right))
  (insert (if (or
               (bolp)
               (string-match-p "^\s*$" (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position))))
              "" comment-padding)
          comment-start comment-padding)
  (unless (string-equal comment-end "")
    (save-excursion
      (insert comment-padding comment-end))))

(defun in-comment-p ()
  (nth 4 (syntax-ppss)))

;;; TODO: fix csharp-maybe-insert-codedoc and the indentation of attributes
;;; (get/set) and methods in c# and see if there's a way to hook into
;;; ido-find-file so that it doesn't fail completely???

(defun just-electric-bracket ()
  (interactive) (insert "{}") (backward-char 1))

(defun just-electric-parens ()
  (interactive) (insert "()") (backward-char 1))

(defun just-semicolon ()
  (interactive) (insert ";"))

(defun csharp-hack-newline ()
  (interactive)
  (let ((annoying-identifiers
         (delete-dups
          (append '(")") (c-lang-const c-type-list-kwds csharp)
                  (c-lang-const c-block-stmt-1-kwds csharp)
                  (c-lang-const c-block-stmt-2-kwds csharp)
                  '("else")))))
    (cond
     ((and (char-before) (char-equal (char-before) (str2char "."))
           (not (in-comment-p)))
      (backward-char)
      (newline-and-indent)
      (forward-char))
     ((string-match-p
       (concat (regexp-opt annoying-identifiers) "\s*$")
       (buffer-substring-no-properties
        (line-beginning-position) (point)))
      (let ((pt (point))
            (next-pt nil))
        (insert ";")
        (newline-and-indent)
        (setq next-pt (point))
        (goto-char pt)
        (delete-char 1)
        (goto-char (1- next-pt))))
     ((and (string-match-p
            "(\s*$" (buffer-substring-no-properties
                     (line-beginning-position) (point)))
           (char-equal (char-after) (str2char ")")))
      (newline) (indent-for-tab-command))
     (t (newline-and-indent)))))

(defun csharp-hack-parenthesis ()
  (interactive)
  (when (and
           (not (whitespacep (char-before)))
           (string-match-p
            (concat (regexp-opt
                     (c-lang-const c-block-stmt-2-kwds csharp) 'words)
                    "\s*$")
            (buffer-substring-no-properties
             (line-beginning-position) (point))))
    (insert " "))
  (insert "()")
  (backward-char))

;;; todo: try creating a namespace, then a class, in an empty file, see what
;;; happens. fix that.

;;; compilation-mode
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

;;; basic utilities
(defun nuke-whitespace-except-this-line ()
  (interactive)
  (let* ((orig-pt (point))
         (left-pt
          (loop while (and (char-before) (whitespacep (char-before))
                           (not (bolp)))
                do (backward-char)
                finally (let ((res (point)))
                          (goto-char orig-pt)
                          (return res))))
         (right-pt
          (loop while (and (not (eolp)) (char-after) (whitespacep (char-after)))
                do (forward-char)
                ;; if we're not at the end of a line, we do want whitespace
                ;; eradicated
                finally (let ((res (point)))
                          (goto-char orig-pt)
                          (return (if (eolp) res nil)))))
         (buf-str (and right-pt (buffer-substring left-pt right-pt)))
         (offset (and right-pt (- right-pt orig-pt))))
    (delete-trailing-whitespace)
    (when right-pt
      (insert buf-str)
      (backward-char offset))))

;;; read-only text

(defun make-readonly-region-modifiable (beg end)
  (interactive "r")
  (let ((inhibit-read-only t))
    (put-text-property beg end 'read-only nil)))

(defun make-all-readonly-text-in-buffer-modifiable ()
  (interactive)
  (make-readonly-region-modifiable (point-min) (point-max)))

;;; for initialization
(defun actual-setup-submodules ()
  (unless dont-ask-about-git
    (if (not (executable-find "git"))
        (send-message-to-scratch
         "git not installed! some features will be unavailable.")
      (let ((git-submodule-buf-name "*git-submodule-errors*")
            (prev-wd default-directory))
        (cd init-home-folder-dir)
        (unwind-protect
            (let ((submodule-out-buf
                   (get-buffer-create git-submodule-buf-name)))
              (unless (zerop (call-process
                              "git" nil submodule-out-buf nil
                              "submodule" "update" "--init" "--recursive"))
                (throw 'submodule-failure "init failed")))
          (cd prev-wd))
        (kill-buffer git-submodule-buf-name)))))

(defvar submodules-to-make nil)
(defun make-submodule (folder-name make-cmd &rest make-args)
  (add-to-list 'submodules-to-make (list folder-name make-cmd make-args)))
(defun actual-make-submodule (submodule-args)
  (destructuring-bind (folder-name make-cmd make-args) submodule-args
      (unless (member folder-name submodule-makes-to-ignore)
        (if (not (executable-find make-cmd))
            (with-current-buffer "*scratch*"
              (insert "couldn't find " make-cmd " to build " folder-name "!"))
          (let ((make-output-buf (get-buffer-create
                                  (concat "*" folder-name "-make-errors*")))
                (prev-wd default-directory))
            (cd (concat init-home-folder-dir "/" folder-name))
            (set-process-sentinel
             (apply #'start-process
                    (append
                     (list (concat "make-" folder-name)
                           (buffer-name make-output-buf) make-cmd)
                     make-args))
             (lambda (proc ev)
               (if (and (stringp ev) (string= ev "finished\n"))
                   (kill-buffer (process-buffer proc))
                 (when (process-live-p proc) (kill-process proc))
                 (switch-to-buffer (process-buffer proc))
                 (throw 'submodule-make-failure ev))))
            (cd prev-wd))))))
(defun actual-make-all-submodules ()
  (mapcar #'actual-make-submodule submodules-to-make))

;;; TODO: make this work lol
(defun update-packages-in-list ()
  (remove-hook 'tabulated-list-revert-hook #'update-packages-in-list)
  (package-menu-mark-upgrades)
  ;; below is ripped from `package.el'
  (let (install-list delete-list cmd pkg-desc)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq cmd (char-after))
        (unless (eq cmd ?\s)
          (setq pkg-desc (tabulated-list-get-id))
          (cond ((eq cmd ?D)
                 (push pkg-desc delete-list))
                ((eq cmd ?I)
                 (push pkg-desc install-list))))
        (forward-line)))
    (when (or install-list delete-list)
      (package-menu-execute t))))

(defun update-all-packages ()
  (interactive)
  (package-list-packages)
  (let ((package-buf (current-buffer)))
    (bury-buffer)
    (with-current-buffer package-buf
      (make-local-variable 'tabulated-list-revert-hook)
      (add-hook 'tabulated-list-revert-hook #'update-packages-in-list))))

(provide 'functions)
