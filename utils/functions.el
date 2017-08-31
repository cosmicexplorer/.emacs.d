;;; -*- lexical-binding: t -*-

;;; some of these are mine, some are heavily adapted from emacswiki, some are
;;; copy/paste from emacswiki

(require 'utilities)

(defun path-split (str)
  (cl-check-type str string)
  (split-string str path-separator t "[[:space:]\n]+"))

(defun get-exec-path ()
  (--> (append exec-path
               (path-split (getenv "PATH"))
               (path-split (shell-command-to-string "echo -n $PATH")))
       (cl-remove-if-not #'file-directory-p it)
       (cl-mapcar #'file-truename it)
       (cl-remove-duplicates it :test #'string-equal)))

(defmacro logify (expr) `(not (not ,expr)))

(defconst sentinel-successful-exit-msg "finished\n")

(defcustom pop-on-error-display-function #'pop-to-buffer
  "What function to call to display the buffer in `pop-to-buffer-on-error'.
Should accept a single argument, which is the buffer to display."
  :type 'function)

(defun pop-to-buffer-on-error (buffer-or-name sig data)
  (declare (indent 2))
  (cl-check-type buffer-or-name (or buffer string))
  (cl-check-type sig symbol)
  (cl-check-type data list)
  (let* ((buffer (get-buffer buffer-or-name)))
    (when (buffer-live-p buffer)
      (funcall pop-on-error-display-function buffer))
    (signal sig data)))

(defun xor (&rest args)
  (cl-reduce
   (lambda (cur next)
     (if cur (not next) next))
   args
   :initial-value nil))

(defconst make-stage-empty-symbol 'empty
  "Used in `make-stage-for-lists' to signify that a list has ended.")

(defun make-stage-fill-value (val cars cdrs)
  (cl-loop for car in cars
           for cdr in cdrs
           if (eq cdr make-stage-empty-symbol)
           collect val into new-cars
           and collect nil into new-cdrs
           else
           collect car into new-cars
           and collect cdr into new-cdrs
           finally return (list new-cars new-cdrs)))

(defun make-stage-for-lists (fill lists)
  (-let* (((stage &as cars cdrs)
           (cl-loop for list in lists
                    if (consp list)
                    collect (car list) into cars
                    and collect (cdr list) into cdrs
                    else
                    collect nil into cars
                    and collect make-stage-empty-symbol into cdrs
                    finally return (list cars cdrs))))
    (if (cl-every (l (eq _ make-stage-empty-symbol)) cdrs)
        stage
      (pcase-exhaustive fill
        ('nil stage)
        (`(value ,val)
         (make-stage-fill-value val cars cdrs))
        ((and (pred functionp) fn)
         (list (funcall fn cars cdrs)
               (cl-substitute nil make-stage-empty-symbol cdrs :test #'eq)))))))

;; TODO: macro to make tiny let forms less annoying and push
;; indentation in a little less
(defun zip-safe (fill &rest lists)
  (declare (indent 1))
  (cl-loop
   for (cars cdrs) = (make-stage-for-lists fill lists)
   then (make-stage-for-lists fill cdrs)
   if (cl-find make-stage-empty-symbol cdrs :test #'eq)
   return stages
   else
   collect cars into stages
   end
   finally return stages))

(defun unzip-list (fill list)
  (apply (apply-partially #'zip-safe fill) list))

(cl-defmacro escape-list (form)
  `(cons #'list ,form))

(cl-defmacro once-only ((&rest names) form)
  (declare (indent 1))
  (if (not names) form
    (let ((gensyms (--map (cl-gensym) names)))
      `(let (,@(--map `(,it (cl-gensym)) gensyms))
         `(let (,@(-map (-lambda ((g n)) `(,g ',n))
                        ,(escape-list
                          (-map (-lambda ((g n))
                                  (escape-list `(,g ,n)))
                                (zip-safe nil gensyms names)))))
            ,(let (,@(escape-list (-map (-lambda ((n g)) `(,n ,g))
                                        (zip-safe nil names gensyms))))
               ,form))))))

(cl-defmacro with-gensyms ((&rest syms) &rest body)
  (declare (indent 1))
  `(let ,(--map `(,it (cl-gensym)) syms)
     ,@body))

(defun send-message-to-scratch (&rest msg-args)
  (with-current-buffer (get-buffer-create "*scratch*")
    (goto-char (point-max))
    (insert (apply #'format msg-args))
    (unless (bolp) (newline))))

;;; helper function used for loading custom scripts littered throughout here
(defun local-file-path (filename)
  (concat (expand-file-name
           (if load-file-name
               (file-name-directory (file-truename load-file-name))
             default-directory))
          "/" filename))

;;; helper function often used in keybinding mappings
(defun add-keybinding-to-mode-maps
    (keys-pressed func-to-call-quoted mode-maps-list)
  "Adds function to a given list of mode maps upon pressing a given key."
  (cl-loop for mode-map in mode-maps-list
           do (define-key mode-map (kbd keys-pressed) func-to-call-quoted)))

;;; FIXME: this isn't used anywhere and it seems like it could be useful
(defun add-keys-to-modes (cmd maps-spec &rest kbds)
  "Map KBDS to CMD in MAPS-SPEC.

MAPS-SPEC can be t, nil, a map, a symbol, or a list.

nil:	map KBDS with `global-set-key'.
t:	map KBDS with `define-key' in the `major-mode' of the `current-buffer'.
map:	map KBDS with `define-key' in the given map.
symbol:	if the `symbol-value' is a map, map KBDS with `define-key' in that map.
list:	map KBDS over all elements of the list as described above."
  (unless maps-spec
    (cl-mapc (lambda (kbd) (global-set-key kbd cmd)) kbds)))

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
        (revert-buffer t t t))))
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
(defun close-and-kill-this-pane (pfx)
  "If there are multiple windows, then close this pane and kill the buffer in it
also."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (if pfx (kill-buf-and-all-visiting nil) (kill-this-buffer))
    (if (not (one-window-p))
        (delete-window))))

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

(defalias 'small-text 'little-text)

(defun even-littler-text ()
  (interactive)
  (set-face-attribute 'default nil :height 90))

(defun increase-font-size ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ attr 10))))

(defun increase-font-size-a-little ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (+ attr 5))))

(defun decrease-font-size ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- attr 10))))

(defun decrease-font-size-a-little ()
  (interactive)
  (let ((attr (face-attribute 'default :height)))
    (set-face-attribute 'default nil :height (- attr 5))))

(define-minor-mode change-font-size-mode
  "Change font size interactively!" nil "FontSize"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") #'increase-font-size)
    (define-key map (kbd "<down>") #'decrease-font-size)
    (define-key map (kbd "<left>") #'decrease-font-size-a-little)
    (define-key map (kbd "<right>") #'increase-font-size-a-little)
    map))

(defun tiny-text ()
  (interactive)
  (set-face-attribute 'default nil :height 50))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent)
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;; helper functions for toggle-letter-case
;;; https://stackoverflow.com/questions/27798296/check-if-a (continued)
;;; -character-not-string-is-lowercase-uppercase-alphanumeric
(defun wordp (c)
  (and c (= ?w (char-syntax c))))
(defun lowercasep (c)
  (and c (wordp c) (= c (downcase c))))
(defun uppercasep (c)
  (and c (wordp c) (= c (upcase c))))
(defun whitespacep (c)
  (and c (= 32 (char-syntax c))))
(defun string-capitalized-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z]*\\'" str)))

(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: lowercase->ALL CAPS->Initial Caps->(cycle)."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (reg (buffer-substring-no-properties beg end))
         (case-fold-search nil))
    (cond
     ((not (string-match-p "[a-z]" reg))
      ;; if no lowercase (all uppercase)
      ;; -> lowercase
      (downcase-region beg end))
     ((not (string-match-p "[A-Z]" reg))
      ;; if no uppercase (all lowercase)
      ;; -> Initial Caps
      (upcase-initials-region beg end))
     (t
      ;; if mixture of upper/lowercase, "assume" Init Caps
      ;; -> ALL CAPS
      (upcase-region beg end)))))

(defadvice toggle-letter-case (after deactivate-mark-nil activate)
  (when (called-interactively-p 'any)
    (setq deactivate-mark nil)))

;;; adding literal null char causes git to treat file like binary
(defconst +cc-control-sequence+ (concat "a" (make-string 1 0) ";"))
(defun newline-and-indent-fix-cc-mode ()
  "cc-mode's indentation procedures upon adding a new bracket or paren are
annoying. This fixes that."
  (interactive)
  (when (memq major-mode '(c-mode c++-mode)) (clang-format-line))
  (newline-and-indent))

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
  (when (characterp char)
    (let ((case-fold-search nil)
          (str-from-char (string char)))
      (string-match-p "\\`[A-Z]*\\'" str-from-char))))

(defun insert-brackets ()
  (interactive)
  (insert "{}")
  (backward-char 1))

(defun camel-case-right-word ()
  (interactive "^")                     ; highlights region if shifted
  (block out
    (let ((start-pt (point))
          (fin-pt (save-excursion (right-word) (point)))
          (go-forth (not (char-is-capitalized-p (char-after (1+ (point)))))))
      (when go-forth
        (forward-char))
      (when (and (not (char-is-capitalized-p (char-after)))
                 (not go-forth))
        (return-from out (forward-char)))
      (loop do (forward-char)
            while (and (< (point) fin-pt)
                       (or (and go-forth
                                (not (char-is-capitalized-p (char-before))))
                           (and (not go-forth)
                                (char-is-capitalized-p (char-before)))))
            finally (progn
                      (when (< (point) fin-pt) (backward-char 2))
                      (while (and (< (point) fin-pt)
                                  (not (char-is-capitalized-p (char-after))))
                        (forward-char)))))))

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

(defun fix-paredit-comment-dwim (arg)
  "Fixed paredit-comment-dwim's behavior on inline comments."
  (interactive "P")
  (paredit-comment-dwim arg)
  (unless (or (string-match-p "^[[:space:]]*;"
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))
              (string-match-p "[[:space:]]" (make-string 1 (char-before))))
    (insert " ")))

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

(defgroup my-customizations nil "My personal `defcustom' set.")
(defcustom kill-buffer-trash-alist nil
  "alist for other files to delete in `kill-buffer-and-move-file-to-trash'"
  :group 'my-customizations
  :type '(alist :key-type symbol :value-type function))

(defun make-move-buf-to-tmp-file-command (fname)
  (let* ((short-name (file-name-nondirectory fname))
         (tmp-file (make-temp-file short-name))
         (shell-cmd
          (if (eq system-type 'windows-nt)
              (format
               "move \"%s\" \"%s\""
               (convert-standard-filename fname)
               (convert-standard-filename tmp-file))
            (format "mv \"%s\" \"%s\"" fname tmp-file))))
    (list shell-cmd tmp-file)))

(defun kill-buffer-and-move-file-to-trash (pfx)
  "Doesn't delete the file, just moves it to `temporary-file-directory' so that
it goes away."
  (interactive "P")
  (set-buffer-modified-p nil)
  (let ((fname (buffer-file-name)))
    (cl-destructuring-bind (cmd tmp-file)
        (make-move-buf-to-tmp-file-command fname)
      (let ((compiled-file
             (let ((res (assq major-mode kill-buffer-trash-alist)))
               (when res (funcall (cdr res) (buffer-file-name))))))
        (if pfx (close-and-kill-this-pane nil) (kill-this-buffer))
        (if compiled-file
            (cl-destructuring-bind (next-cmd next-tmp-file)
                (make-move-buf-to-tmp-file-command compiled-file)
              (message (concat "'%s' -> '%s' (output: '%s')\n"
                               "'%s' -> '%s' (output: '%s')")
                       fname tmp-file (shell-command-to-string cmd)
                       compiled-file next-tmp-file
                       (shell-command-to-string next-cmd)))
          (message "'%s' -> '%s' (output: '%s')"
                   fname tmp-file (shell-command-to-string cmd)))))))

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

;;; macros
;;; http://stackoverflow.com/a/26137517/2518889
(defmacro with-system (type &rest body)
  "Evaluate body if `system-type' equals type."
  (declare (indent 1))
  `(when (eq system-type ,type) ,@body))

(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")
(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func. Note the weekly
scope of the command's precision.")

;;; the below two functions rely on eshell using eshell-last-input-start,
;;; eshell-last-input-end, and eshell-last-output-start internally! hopefully
;;; these won't change.........................................
;;; currently working on emacs 24.5
(defun eshell-append-str (str)
  (unless (zerop (with-temp-buffer
                   (insert str)
                   (shell-command-on-region
                    (point-min) (point-max)
                    (format ">> %s" eshell-user-output-file)
                    t)))
    (error "failed to write eshell output")))

(defun eshell-send-input-to-history ()
  (let ((str
         (concat
          "--in--: " default-directory ": "
          (format-time-string current-date-time-format (current-time))
          "\n"
          (buffer-substring-no-properties
           (marker-position eshell-last-input-start)
           (point)))))
    (eshell-append-str str)))

(defun eshell-send-output-to-history ()
  (let* ((str
          (concat "--out--: " default-directory ": "
                  (format-time-string current-date-time-format
                                      (current-time))
                  "\n"
                  (buffer-substring-no-properties
                   (marker-position eshell-last-input-end)
                   (marker-position eshell-last-output-start))))
         (res (string-match-p "\n$" str)))
    (eshell-append-str (if res str (concat str "\n")))))

(defvar-local prev-shell-input "")
(defvar-local was-last-output nil)

(defun shell-append-str (str)
  (unless (zerop (with-temp-buffer
                   (insert str)
                   (shell-command-on-region
                    (point-min) (point-max)
                    (format ">> %s" shell-user-output-file)
                    t)))
    (error "failed to write shell output")))

(defun shell-send-input-to-history (str-to-send)
  (let ((str
         (ansi-color-filter-apply
          (concat "\n" "--in--: " default-directory ": "
                  (format-time-string current-date-time-format
                                      (current-time))
                  "\n"
                  str-to-send))))
    (shell-append-str str)
    (setq prev-shell-input str-to-send)
    (setq was-last-output nil)))

(defvar my-shell-prompt-pattern "^[^#%$>
-]*[#$%>] *")

(defun shell-send-output-to-history (str-to-send)
  (let ((extra-prompt-regexen
         (list (concat ".\\{,4\\}"
                       (regexp-quote (user-login-name))
                       "@.*$")
               my-shell-prompt-pattern)))
    (let ((treated-str
           (trim-whitespace
            (replace-regexp-in-string
             "%[[:space:]\r\n]*\\'"
             ""
             (replace-regexp-in-string
              (concat
               "\\("
               (reduce (lambda (a b) (concat a "\\|" b))
                       extra-prompt-regexen)
               "\\)")
              ""
              (replace-regexp-in-string
               (regexp-quote
                (trim-whitespace prev-shell-input))
               ""
               (ansi-color-filter-apply str-to-send))))))
          (whitespace-regex "\\`[[:space:]\r\n]*\\'")
          (header-str (if was-last-output ""
                        (concat "--out--: " default-directory ": "
                                (format-time-string current-date-time-format
                                                    (current-time))
                                "\n"))))
      (unless (or (string-match-p whitespace-regex str-to-send)
                  (string-match-p whitespace-regex treated-str))
        (shell-append-str (concat header-str treated-str))
        (setq was-last-output t)))))

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

(defun alist-process-modify-result (op result)
  (-let* (((kv-used kv-left matched unmatched) result)
          (real-matched (cl-ecase op
                          (swap
                           (cl-assert (null matched))
                           kv-used)
                          (keep matched)
                          ((nil remove)
                           (cl-assert (null matched))
                           nil))))
    (list kv-used kv-left real-matched unmatched)))

(cl-defun alist-modify-range
    (k-v-alist alist &key (test #'eq) op max-changes keep-kv)
  ;; (op = nil) => 'remove
  (cl-check-type k-v-alist list)
  (cl-check-type alist list)
  (cl-check-type test function)
  (cl-check-type max-changes (or null wholenum))
  (cl-check-type keep-kv boolean)
  (cond
   ((or
     ;; if either is empty
     (not (and k-v-alist alist))
     ;; if no changes allowed
     (eql max-changes 0))
    (alist-process-modify-result op (list nil k-v-alist nil alist)))
   ;; if both have 1 element
   ((not (or (cdr alist) (cdr k-v-alist)))
    (-let* ((((k . v)) alist)
            (((key . val)) k-v-alist)
            (res
             (if (funcall test k key)
                 (append
                  (list k-v-alist nil)
                  (cl-ecase op
                    ((nil remove swap) (list nil nil))
                    (keep (list (list (cons k v)) nil))))
               (list nil k-v-alist nil alist))))
      (alist-process-modify-result op res)))
   (t
    (cl-loop
     for cells on alist
     for (cur . _) = cells
     with k-v-used = nil
     with k-v-left = k-v-alist
     for kv-to-use = (if keep-kv k-v-alist k-v-left)
     with matched = nil
     with unmatched = nil
     with changes-made = 0
     until (or (and max-changes (>= changes-made max-changes))
               (not kv-to-use))
     do (-let* (((_ cur-unmatched kv-matched _)
                 (alist-modify-range `(,cur) kv-to-use :test test :op 'keep)))
          (if cur-unmatched
              (push cur unmatched)
            (cl-assert (and kv-matched
                            (= 1 (length kv-matched))))
            (incf changes-made)
            (-let* (((_ new-kv matching-used nonmatching-used)
                     (alist-modify-range
                      kv-matched k-v-used :test test :op 'keep))
                    ((_ _ _ still-left)
                     (alist-modify-range
                      kv-matched k-v-left :test test :op 'remove)))
              (setq k-v-used (append new-kv matching-used nonmatching-used)
                    k-v-left still-left))
            (cl-ecase op
              ((nil remove swap))
              (keep
               (push cur matched)))))
     ;; if we quit early, consider the untouched cells as "unmatched"
     finally return
     (let* ((all-unmatched (append unmatched cells))
            (real-kv-used (reverse k-v-used)))
       (alist-process-modify-result
        op (list real-kv-used k-v-left matched all-unmatched)))))))

(defconst new-buffer-with-basename "*formatted-code*")

(defun new-buffer-with (basename contents &optional mode-or-func)
  (let ((mode
         (if (functionp mode-or-func) mode-or-func major-mode)))
    (with-current-buffer (generate-new-buffer
                          (or basename new-buffer-with-basename))
      (insert contents)
      (funcall mode)
      (current-buffer))))

(defun message-my-errors (err-type msg buf)
  (display-warning 'my-errors msg :debug buf))

(define-error 'subproc-error "A subprocess exited with a non-zero status"
  'my-errors)

(defun pp-code-subproc (cmd args &optional basename new-buf-mode)
  (let* ((contents (buffer-string))
         (buf (if new-buf-mode
                  (new-buffer-with basename contents new-buf-mode)
                (current-buffer)))
         (code
          (with-current-buffer buf
            (let ((debug-on-error nil)
                  (debug-on-signal nil))
              (condition-case err
                  (apply
                   #'call-process-region
                   (append (list (point-min) (point-max) cmd t t nil) args))
                (error (error-message-string err)))))))
    (if (or (not (integerp code))
            (/= code 0))
        (with-current-buffer buf
          (let ((errstr (buffer-string)))
            (erase-buffer)
            (insert contents)
            (pop-to-buffer buf)
            (signal 'subproc-error
                    (list (format "command %s failed: %s" cmd
                                  (if (integerp code)
                                      (format "exited with status %d" code)
                                    code))
                          errstr))))
      (unless (eq (current-buffer) buf)
        (pop-to-buffer buf)))))

(defconst json-fmt-buf-basename "*json-formatted*")

(defun json-fmt (&optional pfx)
  (interactive "P")
  (pp-code-subproc "jq" (list ".") json-fmt-buf-basename (when pfx 'json-mode)))

(defconst xml-fmt-buf-basename "*xml-formatted*")

(defcustom xmllint-pretty-level 1
  "Value for the \"--pretty\" argument used in xmllint. See `xml-fmt' for
details."
  :type '(radio (const 0)
                (const 1)
                (const 2)))

(defun xml-fmt (&optional pfx)
  (interactive "P")
  (let ((prog "xmllint")
        (args (list "--pretty" (number-to-string xmllint-pretty-level) "-"))
        (buf-name xml-fmt-buf-basename)
        (mode (when pfx 'nxml-mode)))
    (pp-code-subproc prog args buf-name mode)))

;;; TODO: make better macro for anonymous functions which doesn't require
;;; writing out `lambda' a million times
(defun o (&rest funs)
  (lambda (&rest args)
    (cl-reduce 'funcall (cl-rest funs)
               :from-end t
               :initial-value (apply (car funs) args))))

(defmacro z (expr)
  `(lambda () ,expr))

(defmacro cmd (expr)
  `(lambda ()
     (interactive)
     ,expr))

(defmacro l (expr)
  (with-gensyms (arg)
    `(lambda (,arg)
       ,(cl-subst arg '_ (macroexpand-all expr) :test #'eq))))

(defmacro |> (&rest exprs) `(l (-> _ ,@exprs)))
(defmacro |-> (&rest exprs) `(l (--> _ ,@exprs)))

(defmacro *> (expr)
  `(l (cl-mapcar (l ,expr) _)))

(defcustom save-visiting-files-reject-regexps (list (rx bos (in space)))
  "Regexps used to reject files to save on shutdown."
  :type '(repeat string))

(defun get-sorted-visiting-file-buffers ()
  (cl-sort
   (cl-remove-if
    (lambda (buf)
      (let ((fname (buffer-file-name buf)))
        (if (not fname) t
          (cl-some (lambda (reg) (string-match-p reg fname))
                   save-visiting-files-reject-regexps))))
    (buffer-list))
   #'string-lessp :key #'buffer-file-name))

(defcustom save-visiting-files-alist
  `((buffer-file-name . "visit"))
  "Alist of functions to call to determine what filetype will be recorded within
`save-visiting-files-to-buffer'."
  :type '(alist :key-type function
                :value-type string))

(defun format-visiting-file-line (type path pt)
  (when (and type (stringp type)
             path (file-exists-p path)
             pt (wholenump pt))
    (format "%s:\"%s\":%d" type path pt)))

(defun save-visiting-files-to-buffer ()
  (interactive)
  (clean-nonvisiting-buffers)
  ;; TODO: make this more error-resistant, somehow. having to send emacs a
  ;; sigterm because this function fails on quit is annoying.
  (let ((saved-buf (find-file saved-files)))
    (with-current-buffer saved-buf
      (erase-buffer)
      (-let* ((visiting (get-sorted-visiting-file-buffers))
              (tupled
               (--keep
                (format-visiting-file-line
                 (cdr (-first (-lambda (e) (funcall (car e) it))
                              save-visiting-files-alist))
                 (file-truename (buffer-file-name it))
                 (with-current-buffer it (point)))
                visiting))
              (str (--reduce-from (format "%s\n%s" acc it) "" tupled)))
        (insert (replace-regexp-in-string "\\`\\s-+" "" str))
        (save-buffer)))
    (kill-buffer saved-buf)))

;;; checking for features
(defmacro with-feature (feature-sym &rest body)
  ,(when (featurep ,feature-sym) ,@body))
(put 'with-feature 'lisp-indent-function 1)

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

(defconst temp-buffer-name-regexp
  "\\`\\s-*\\*[a-zA-Z0-9-_]*\\*?[a-zA-Z0-9-_]*\\*\\s-*\\'")

(defun buf-info ()
  (list (current-buffer) (buffer-name) (buffer-file-name)))

(cl-defun print-buf-path-on-kill (op name fname)
  (message "%s '%s', at '%s'"
           op name (or fname "<no path>")))

(cl-defun no-msg-temp-buffers (name fname)
  (and fname (not (string-match-p temp-buffer-name-regexp name))))

(defvar last-killed-buf nil)
(defcustom do-message-on-op #'no-msg-temp-buffers
  "Whether to send a `message' every time a buffer is killed within
`message-buffer-id'.

Can be a boolean, string, or function. If boolean and non-nil, a message is
sent. If a string, it uses that as a regex to match against the buffer's
`buffer-name'; if the match succeeds, the the message is sent. If a function,
the function is called with a list of three elements: the buffer, its name, and
its filename, and a message is sent if the predicate returns non-nil."
  :type '(choice boolean regexp function))

(defun message-buffer-id (op)
  (lambda ()
    (-let* (((buf name fname) (buf-info))
            (msg do-message-on-op))
      (setq last-killed-buf buf)
      (when (cl-typecase msg
              (boolean (logify msg))
              (string (string-match-p msg name))
              (function (funcall msg name fname)))
        (print-buf-path-on-kill op name fname)))))

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

(defun w3m-goto-url-new-tab ()
  (interactive)
  (let ((active (w3m-active-region-or-url-at-point)))
    (when active (w3m-goto-url-new-session active))))

(defun w3m-goto-url-new-tab-mouse ()
  (interactive)
  (call-interactively #'mouse-set-point)
  (call-interactively #'w3m-goto-url-new-tab))

(defun w3m-goto-url-mouse ()
  (interactive)
  (call-interactively #'mouse-set-point)
  (let ((active (w3m-active-region-or-url-at-point)))
    (when active
      (w3m-goto-url active))))

(defun w3m-forget () (interactive))

(defvar w3m-urls nil)
(defvar w3m-cur-url-ptr nil)
(defun w3m-delete-buf-remember ()
  (interactive)
  (push w3m-current-url w3m-urls)
  (setq w3m-cur-url-ptr w3m-urls)
  (call-interactively #'w3m-delete-buffer))

(defun w3m-restore-buffer ()
  (interactive)
  (if w3m-cur-url-ptr
      (w3m-goto-url-new-session (car w3m-cur-url-ptr))
    (message "%s" "No old urls to restore!"))
  (setq w3m-cur-url-ptr (cdr w3m-cur-url-ptr)))

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

(defconst fix-linum-relative-default-sym ">")

(defun nonzerop (arg) (not (zerop arg)))

(cl-deftype natural-num ()
  `(and wholenum (satisfies nonzerop)))

(defun squish-number-to-width (width num)
  (cl-check-type width natural-num)
  (cl-check-type num number)
  (let ((rep (format "%d" num)))
    (if (<= (length rep) width) rep
      (substring rep 0 width))))

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
         (current-symbol (if current-p
                             (if (string-empty-p linum-relative-current-symbol)
                                 fix-linum-relative-default-sym
                               linum-relative-current-symbol)
                           (format "%d" diff)))
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
                 (if (eq left-or-right 'left)
                     (unless (bobp) (backward-char))
                   (unless (eobp) (forward-char))))
            finally
            (return final-text-char)))))

(defun c-comment-region-stars (reg-beg reg-end num-stars-arg)
  "Comments all text within a given region between REG-BEG and REG-END, or the
current line if no region is active. NUM-STARS-ARG is given by prefix argument,
and determines the number of asterisks ('*') to insert before the initial
delimiter and after the closing comment delimiter. If a blank argument is given,
it formats the region using javadoc syntax, with two stars on the initial line
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
        (beg (if (not (use-region-p)) (frontier-of-text-for-line 'left)
               (save-excursion
                 (goto-char reg-beg) (frontier-of-text-for-line 'left))))
        (end (let ((res
                    (if (not (use-region-p)) (frontier-of-text-for-line 'right)
                      (goto-char reg-end)
                      (if (>= (point) (frontier-of-text-for-line 'right))
                          reg-end
                        (forward-line -1)
                        (goto-char (frontier-of-text-for-line 'right))
                        (if (> (point) reg-beg) (point)
                          (goto-char reg-beg)
                          (goto-char (frontier-of-text-for-line 'right))
                          (point))))))
               (if (save-excursion (goto-char res)
                                   (looking-back "[[:space:]\n]+"))
                   (save-excursion
                     (goto-char res)
                     (re-search-backward "[^[:space:]\n]" nil t)
                     (1+ (point)))
                 res)))
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
                   (if num-stars
                       (if (looking-at-p "[[:space:]]*\n") "" comment-padding)
                     "\n"))))
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
               (string-match-p "^[[:space:]]*$" (buffer-substring-no-properties
                                                 (line-beginning-position)
                                                 (line-end-position))))
              "" comment-padding)
          comment-start comment-padding)
  (unless (string-equal comment-end "")
    (save-excursion
      (insert comment-padding comment-end))))

(defun in-comment-p ()
  (nth 4 (syntax-ppss)))

(defun just-electric-bracket ()
  (interactive) (insert "{}") (backward-char 1))

(defun just-electric-parens ()
  (interactive) (insert "()") (backward-char 1))

(defun just-semicolon ()
  (interactive) (insert ";"))


;;; todo: try creating a namespace, then a class, in an empty file, see what
;;; happens. fix that.

;;; compilation-mode
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))

;;; basic utilities
(defvar trailing-whitespace-regexp "\\([\t \u00A0]+\\)$")
(defvar previous-whitespace-regexp "[\t \u00A0]+")
(defvar trailing-whitespace-maybe-regexp "\\([\t \u00A0]*\\)$")
(defun in-whitespace-region-p ()
  (and (looking-back previous-whitespace-regexp)
       (looking-at trailing-whitespace-maybe-regexp)))

(defvar destroy-whitespace t)

(defun search-back-bol-or-reg (reg)
  (let ((reg-pt (1+ (save-excursion (re-search-backward reg nil t))))
        (bol-pt (line-beginning-position)))
    (goto-char (max bol-pt reg-pt))))

(defun nuke-whitespace-except-this-line ()
  (interactive)
  (when destroy-whitespace
    (if (not (in-whitespace-region-p)) (whitespace-cleanup)
      (let* ((pt (point))
             (str (buffer-substring
                   (let ((res (search-back-bol-or-reg "[^[:space:]]")))
                     (if res (point)
                       (goto-char pt)
                       (point-min)))
                   pt)))
        (whitespace-cleanup)
        (insert str)))))

;;; read-only text

(defun make-readonly-text-modifiable ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (use-region-p)
        (put-text-property (region-beginning) (region-end) 'read-only nil)
      (put-text-property (point-min) (point-max) 'read-only nil))))

;;; for initialization
(defun run-git-updates (submodule-out-buf)
  (or (with-current-buffer submodule-out-buf
        (and
         (msg-evals (((buffer-string) bstr)) :before "init")
         (zerop
          (call-process
           "git" nil t nil
           "submodule" "init"))
         (msg-evals (((buffer-string) bstr)) :before "checkout")
         (zerop
          (call-process
           "git" nil t nil
           "submodule" "foreach" "git" "checkout" "master"))
         (msg-evals (((buffer-string) bstr)) :before "fetch")
         (zerop
          (call-process-shell-command
           "timeout -k 1 10 git submodule foreach git fetch"
           nil t nil))
         (msg-evals (((buffer-string) bstr)) :before "rev-parse")
         (progn
           (erase-buffer)
           (zerop
            (call-process
             "git" nil t nil
             "submodule" "--quiet" "foreach"
             "printf \"%s:%s:%s\\n\" $path $(git rev-parse --sq HEAD @{u})")))
         (msg-evals (((buffer-string) bstr)) :before "hashes")
         (-let* (((all-dirs failed)
                  (progn
                    (goto-char (point-min))
                    (cl-loop
                     while (re-search-forward
                            "^\\([^:]*\\):'\\([^']*\\)':'\\([^']*\\)'$"
                            nil t)
                     for (dir sha1 sha2) = (-map #'match-string (range 1 3))
                     collect dir into all-dirs
                     unless (string= sha1 sha2)
                     collect dir into failed
                     finally return (list all-dirs failed)))))
           (msg-evals (all-dirs failed) :before "run-git-updates end")
           (cl-every
            (lambda (dir)
              (let ((default-directory (expand-file-name dir)))
                (zerop
                 (call-process-shell-command
                  "timeout -k 1 10 git pull origin master"
                  nil t nil))))
            failed)
           (list all-dirs failed))))
      (progn
        (with-current-buffer submodule-out-buf
          (goto-char (point-max))
          (insert "submodule failure!"))
        (error "submodule failure"))))

(defun actual-setup-submodules ()
  (cl-assert (internet-connected-p))
  (cl-assert (executable-find "git"))
  (-let* ((git-submodule-buf-name "*git-submodule-errors*")
          (default-directory init-home-folder-dir)
          (submodule-out-buf
           (get-buffer-create git-submodule-buf-name))
          ((all-dirs failed)
           (let ((debug-on-error nil))
             (condition-case err
                 (run-git-updates submodule-out-buf)
               (error
                (with-temp-buffer
                  (cl-assert
                   (zerop
                    (call-process
                     "git" nil t nil
                     "submodule" "--quiet" "foreach" "echo $path")))
                  (let* ((out (buffer-string))
                         (processed
                          (replace-regexp-in-string "\n\\'" "" out)))
                    (list (split-string processed "\n") t))))))))
    (if failed
        (progn
          (pop-to-buffer submodule-out-buf)
          (goto-char (point-max)))
      (kill-buffer git-submodule-buf-name))
    all-dirs))

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
  ;; TODO: fix this lol
  (interactive)
  (package-list-packages)
  (let ((package-buf (current-buffer)))
    (bury-buffer)
    (with-current-buffer package-buf
      (make-local-variable 'tabulated-list-revert-hook)
      (add-hook 'tabulated-list-revert-hook #'update-packages-in-list))))

;;; a grep for the modern world
(defun regrep ()
  (interactive)
  (let ((cmd
         (read-shell-command
          "Run grep (like this): " (or (car grep-history) grep-command)
          'grep-history)))
    (prependn cmd grep-history)
    (grep cmd)))

(defun refind ()
  (interactive)
  (let ((cmd
         (read-shell-command
          "Run find-grep (like this): "
          (or (car grep-find-history) grep-find-command)
          'grep-find-history)))
    (prependn cmd grep-find-history)
    (grep-find cmd)))

(defun refind-or-grep ()
  (interactive)
  (let ((prev-comp (car compilation-arguments))
        (mode (second compilation-arguments)))
    (if (not (string-equal (substring prev-comp 0 4) "find"))
        (regrep)
      (refind))))

;;; finding things in emacs lisp
(defun describe-function-at-point ()
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (describe-function symb))))

(defun describe-variable-at-point ()
  (interactive)
  (let ((symb (variable-at-point)))
    (when (and symb (not (equal symb 0)))
      (describe-variable symb))))

(defun describe-function-or-variable-at-point ()
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb (not (equal symb 0)))
        (describe-variable symb)
      (let ((fun-symb (function-called-at-point)))
        (when fun-symb (describe-function fun-symb))))))

(defun function-or-symbol (sym) (or (fboundp sym) (boundp sym)))
(defun describe-function-or-variable ()
  (interactive)
  (let* ((symb (variable-at-point))
         (fun-symb (function-called-at-point))
         (real-symb (if (zerop symb) fun-symb symb))
         (val (completing-read
               (concat "describe function or variable: "
                       (when real-symb
                         (format "(default %s) " (symbol-name real-symb))))
               obarray #'function-or-symbol t nil nil
               (when real-symb (symbol-name real-symb)))))
    (if (fboundp (intern val)) (describe-function (intern val))
      (describe-variable (intern val)))))

(defun find-function-or-variable ()
  (interactive)
  (let* ((symb (variable-at-point))
         (fun-symb (function-called-at-point))
         (real-symb (if (zerop symb) fun-symb symb))
         (val (completing-read
               (concat "find function or variable: "
                       (when real-symb
                         (format "(default %s) " (symbol-name real-symb))))
               obarray #'function-or-symbol t nil nil
               (when real-symb (symbol-name real-symb)))))
    (if (fboundp (intern val)) (find-function (intern val))
      (find-variable (intern val)))))

(defun find-function-or-variable-at-point ()
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb (not (equal symb 0)))
        (find-variable symb)
      (let ((fun-symb (function-called-at-point)))
        (when fun-symb (find-function fun-symb))))))

;;; http://stackoverflow.com/a/21047199/2518889
(defun shelllike-filter (proc string)
  (let* ((buffer (process-buffer proc))
         (window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (if (not (mark)) (push-mark))
      (exchange-point-and-mark) ;Use the mark to represent the cursor location
      (dolist (char (append string nil))
        (cond ((char-equal char ?\r)
               (move-beginning-of-line 1))
              ((char-equal char ?\n)
               (move-end-of-line 1) (newline))
              (t
               (if (/= (point) (point-max)) ;Overwrite character
                   (delete-char 1))
               (insert char))))
      (exchange-point-and-mark))
    (if window
        (with-selected-window window
          (goto-char (point-max))))))

;;; mark stuff
;;; http://stackoverflow.com/a/14539202/2518889
(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

(defun kill-buf-and-all-visiting (pfx)
  (interactive "P")
  (loop
   with dir = (if pfx (ido-read-directory-name "directory to kill: ")
                default-directory)
   with dir-reg = (concat "\\`" (regexp-quote (expand-file-name dir)))
   for buf in (buffer-list)
   do (let ((place (expand-file-name
                    (or (let ((name (buffer-file-name buf)))
                          (when name (file-name-directory name)))
                        (with-current-buffer buf default-directory)))))
        (when (string-match-p dir-reg place) (kill-buffer buf)))))

(defun dired-kill-marked-files ()
  (interactive)
  (loop for file in (mapcar #'expand-file-name (dired-get-marked-files))
        do (let ((buf (find-buffer-visiting file)))
             (when buf (kill-buffer buf)))))

(defun magit-kill-commit-buffer-no-query ()
  (interactive)
  (setq kill-buffer-query-functions
        (remove 'git-commit-kill-buffer-noop kill-buffer-query-functions))
  (kill-this-buffer))

(defun eval-buffer-and-message (prefix-given)
  (interactive "P")
  (let* ((reg (region-active-p))
         (res (if reg (eval-region (region-beginning) (region-end)
                                   (when current-prefix-arg (current-buffer)))
                (eval-buffer nil (when current-prefix-arg (current-buffer))))))
    (message "%s %s" "evaluated" (if reg "region" (buffer-name)))))

;;; html functions
(require 'sgml-mode)

(defun html-autoclose-tag ()
  (interactive)
  (if (not (within-tag-p))
      (sgml-close-tag)
    (let ((context (car (save-excursion (sgml-get-context)))))
      (if (assoc (save-excursion (backward-char) (aref context 4))
                 html-autoclosable-tags)
          (progn
            (goto-char (aref context 3))
            (delete-backward-char 1)
            (sgml-close-tag))
        (goto-char (aref context 3))
        (sgml-close-tag)
        (goto-char (aref context 3))))))

(defun html-beginning-of-tag ()
  (interactive)
  (let ((context (car (save-excursion (sgml-get-context)))))
    (goto-char (aref context 2))))

(defun html-end-of-tag ()
  (interactive)
  (html-beginning-of-tag)
  (sgml-skip-tag-forward 1))

(defun html-inner-tag ()
  (interactive)
  (let ((context (car (save-excursion (sgml-get-context)))))
    (cond ((eq (aref context 1) 'open)
           (goto-char (aref context 3)))
          ((eq (aref context 1) 'close)
           (goto-char (aref context 2)))
          (t (error "i didn't know what to do in this situation")))))

(defun html-skip-to-beginning-of-tag ()
  (interactive)
  (html-skip-tag-or-token-backward t))

(defun html-skip-tag-or-token-forward (prefix)
  (interactive "^P")
  (if prefix
      (let ((context (car (save-excursion (sgml-get-context)))))
        (goto-char (aref context 3)))
    (if (within-tag-p)
        (let ((context (car (save-excursion (sgml-get-context)))))
          (cond ((and (= (point) (1+ (aref context 2)))
                      (eq (aref context 1) 'open))
                 (html-inner-tag))
                ((> (save-excursion (camel-case-right-word) (point))
                    (aref context 3))
                 (goto-char (aref context 3)))
                (t (camel-case-right-word))))
      (let* ((prev-ctx (car (save-excursion (sgml-get-context))))
             (prev-pt (point)))
        (sgml-skip-tag-forward 1)
        (unless (= (aref prev-ctx 2)
                   (aref (car (save-excursion (sgml-get-context))) 2))
          (backward-char)
          (html-inner-tag))
        (when (= (point) prev-pt)
          (sgml-skip-tag-forward 1))))))

(defun html-skip-to-end-of-tag ()
  (interactive)
  (html-skip-tag-or-token-forward t))

(defun html-skip-tag-or-token-backward (prefix)
  (interactive "^P")
  (if prefix
      (let ((context (car (save-excursion (sgml-get-context)))))
        (goto-char (aref context 2)))
    (if (within-tag-p)
        (let ((context (car (save-excursion (sgml-get-context)))))
          (cond ((and (= (point) (1- (aref context 3)))
                      (eq (aref context 1) 'close))
                 (html-inner-tag))
                ((< (save-excursion (camel-case-left-word) (point))
                    (aref context 2))
                 (goto-char (aref context 2)))
                (t (camel-case-left-word))))
      (let ((prev-ctx (car (save-excursion (sgml-get-context)))))
        (if (= (point) (aref prev-ctx 3)) (sgml-skip-tag-backward 1)
          (sgml-skip-tag-backward 1)
          (unless (= (aref prev-ctx 2)
                     (aref (car (save-excursion (sgml-get-context))) 2))
            (forward-char)
            (html-inner-tag)))))))

(defun html-newline-indent ()
  "`newline-and-indent' is weird for `html-mode' for some reason. This bypasses
that."
  (interactive)
  (save-excursion (clear-whitespace))
  (let ((is-at-end-of-ctx
         (save-excursion
           (backward-char)
           (= (1+ (point))
              (aref (car (save-excursion (sgml-get-context))) 3)))))
    (clear-whitespace)
    (save-excursion
      (forward-line 1)
      (indent-according-to-mode))
    (when is-at-end-of-ctx
      (insert "\n")
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))))

(defun html-insert-xhtml-header (prefix)
  (interactive "P")
  (let ((val (point-max)))
    (when prefix (erase-buffer))
    (insert "<?xml version=\"1.0\" encoding=\"utf-8\" ?>" "\n"
            "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"" "\n"
            "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "\n")
    (when prefix
      (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\" "
              "lang=\"en\" xml:lang=\"en\">" "\n"
              "<meta http-equiv=\"Content-Type\" "
              "content=\"text/html;charset=utf-8\" />" "\n"
              "<meta name=\"viewport\" "
              "content=\"width=device-width, initial-scale=1\" />" "\n"
              "<head></head>" "\n"
              "<body></body>" "\n"
              "</html>")
      (web-beautify-html-buffer)
      (goto-char (point-min)))))

(defun html-kill-tag-after-point-int (prefix)
  (interactive "P")
  (cond ((eq last-command 'html-kill-tag-after-point-int)
         (html-kill-tag-after-point t))
        ((use-region-p)
         (kill-region (region-beginning) (region-end)))
        ((numberp prefix)
         (kill-line prefix))
        ((car prefix) (html-kill-tag-contents))
        (t (html-kill-tag-after-point)))
  (sgml-indent-line))

(defun html-yank (&optional pfx)
  (interactive "P")
  (let ((pt (point)))
    (yank pfx)
    (indent-region pt (point))
    (indent-according-to-mode)))

(defun html-yank-pop (&optional pfx)
  (interactive "P")
  (let ((pt (point)))
    (setq last-command 'yank)
    (yank-pop pfx)
    ;; yank-pop sets mark to beginning of pasted text, so this works
    (indent-region (mark) (point))
    (indent-according-to-mode)))

(defun html-kill-tag-contents ()
  (interactive)
  (if (within-tag-p)
      (let ((context (car (save-excursion (sgml-get-context)))))
        (goto-char (aref context 2))
        (forward-char)
        (camel-case-right-word)
        (html-kill-tag-after-point))
    (let ((context (car (save-excursion (sgml-get-context)))))
      (goto-char (aref context 3))
      (let ((close-context
             (save-excursion
               (loop with ctx = (car (save-excursion (sgml-get-context)))
                     until (and (> (point) (aref ctx 2))
                                (< (point) (aref ctx 3)))
                     do (progn
                          (camel-case-right-word)
                          (setq ctx (car (save-excursion (sgml-get-context)))))
                     finally (return ctx)))))
        (kill-region (point) (aref close-context 2))))))

(defun html-kill-tag-after-point (&optional append-arg)
  (let* ((e-o-l (1+ (line-end-position)))
         (line-str (buffer-substring (point) e-o-l)))
    (if (string-match-p "\\`[[:space:]]*\\'" line-str)
        (progn
          (delete-region (point) e-o-l)
          (when append-arg
            (kill-append line-str nil)))
      (let (prev-pt)
        (if (within-tag-p)
            (progn
              (let* ((context (car (save-excursion (sgml-get-context))))
                     (final-point (1- (aref context 3))))
                (when (char-equal (char-before final-point) ?/)
                  (decf final-point))
                ;; TODO: make this actually parse stuff, a space within quotes
                ;; doesn't matter to us
                (if (whitespacep (char-after))
                    (loop while (whitespacep (char-after))
                          do (forward-char))
                  (loop until (or (whitespacep (char-before))
                                  (= (point) (aref context 2)))
                        do (backward-char)
                        finally (when (= (point) (aref context 2))
                                  (camel-case-right-word)
                                  (loop while (not (whitespacep (char-after)))
                                        do (forward-char)))))
                (if append-arg
                    (kill-append (buffer-substring (point) final-point) nil)
                  (kill-region (point) final-point))
                (loop while (whitespacep (char-before))
                      do (delete-backward-char 1))))
          (ignore-errors (backward-char))
          (re-search-forward "<")
          (setq prev-pt
                (if (char-equal (char-after) ?/)
                    (save-excursion
                      (progn (sgml-skip-tag-backward 1) (point)))
                  (backward-char 1)
                  (point)))
          (sgml-skip-tag-forward 1)
          (let ((str (buffer-substring prev-pt (point))))
            (delete-region prev-pt (point))
            (if append-arg
                (kill-append str nil)
              (kill-new str))))))))

(defvar html-autoclosable-tags
  '(("link" . t)
    ("hr" . t)
    ("br" . t)
    ("meta" . t)))

(defun within-tag-p ()
  (let* ((context
          (save-excursion
            (car (sgml-get-context))))
         (pt (point)))
    (and context
         (> pt (aref context 2))
         (< pt (aref context 3)))))

(defun html-slurp-tag-backward ()
  (interactive)
  (let ((pt (point)))
    (html-beginning-of-tag)
    (forward-char)
    (let ((open-tag-ctx (car (save-excursion (sgml-get-context)))))
      (goto-char (aref open-tag-ctx 3))
      (let* ((beg-open (aref open-tag-ctx 2))
             (end-open (aref open-tag-ctx 3))
             (begin-tag-str (buffer-substring beg-open end-open)))
        (delete-region beg-open end-open)
        (sgml-skip-tag-backward 1)
        (insert begin-tag-str)))
    (insert "\n")
    (indent-according-to-mode)
    (let* ((new-pt (point))
           (next-pt (progn (sgml-skip-tag-forward 1) (point))))
      (loop while (whitespacep (char-after))
            do (delete-char 1))
      (insert "\n")
      (indent-according-to-mode)
      (indent-region new-pt next-pt)
      (goto-char new-pt))))

;;; TODO: don't do anything (and leave a message) if there's nothing to slurp or
;;; barf
(defun html-slurp-tag-forward ()
  (interactive)
  (let ((pt (point)))
    (html-end-of-tag)
    (backward-char)
    (let ((close-tag-ctx (car (save-excursion (sgml-get-context)))))
      (goto-char (aref close-tag-ctx 2))
      (let* ((beg-close (aref close-tag-ctx 2))
             (end-close (aref close-tag-ctx 3))
             (end-tag-str (buffer-substring beg-close end-close)))
        (delete-region beg-close end-close)
        (sgml-skip-tag-forward 1)
        (insert end-tag-str)))
    (backward-char)
    (html-inner-tag)
    (insert "\n")
    (indent-according-to-mode)
    (sgml-skip-tag-backward 1)
    (loop while (whitespacep (char-before))
          do (delete-backward-char 1))
    (insert "\n")
    (let* ((new-pt-begin (point))
           (new-pt (progn (sgml-skip-tag-forward 1) (point))))
      (indent-region new-pt-begin new-pt)
      (sgml-skip-tag-backward 1))))

(defun destroy-whitespace-around-me ()
  (interactive)
  (loop while (whitespacep (char-before)) do (delete-backward-char 1))
  (loop while (whitespacep (char-after)) do (delete-char 1)))

(defun destroy-all-whitespace-nearby ()
  (interactive)
  (loop while (let ((c (char-after)))
                (when c (string-match-p "[[:space:]\n]" (string c))))
        do (delete-char 1))
  (loop while (let ((c (char-before)))
                (when c (string-match-p "[[:space:]\n]" (string c))))
        do (delete-backward-char 1)))

(defun html-barf-tag-forward ()
  (interactive)
  (let ((pt (point)))
    (html-end-of-tag)
    (backward-char)
    (html-inner-tag)
    (sgml-skip-tag-backward 1)
    (let* ((beg-tag-pt (point))
           (end-tag-pt (progn (sgml-skip-tag-forward 1) (point)))
           (tag-str (buffer-substring beg-tag-pt end-tag-pt)))
      (delete-region beg-tag-pt end-tag-pt)
      (sgml-skip-tag-forward 1)
      (insert "\n" tag-str)
      (indent-region (- (point) (length tag-str)) (point))
      (goto-char beg-tag-pt)
      (loop while (whitespacep (char-before)) do (delete-backward-char 1))
      (loop while (whitespacep (char-after)) do (delete-char 1))
      (insert "\n")
      (indent-according-to-mode))
    (goto-char pt)
    (beginning-of-line)
    (indent-according-to-mode)))

(defun html-barf-tag-backward ()
  (interactive)
  (let ((pt (point)))
    (html-beginning-of-tag)
    (forward-char)
    (html-inner-tag)
    (sgml-skip-tag-forward 1)
    (let* ((end-tag-pt (point))
           (beg-tag-pt (progn (sgml-skip-tag-backward 1) (point)))
           (tag-str (buffer-substring beg-tag-pt end-tag-pt)))
      (delete-region beg-tag-pt end-tag-pt)
      (sgml-skip-tag-backward 1)
      (insert tag-str "\n")
      (indent-region (- (point) (1+ (length tag-str))) pt)
      (goto-char end-tag-pt)
      (destroy-whitespace-around-me)
      (insert "\n")
      (indent-according-to-mode))
    (goto-char pt)
    (beginning-of-line)
    (indent-according-to-mode)))

(defun html-split-tag ()
  (interactive)
  (html-beginning-of-tag)
  (let* ((begin-pt (point))
         (end-pt (progn (sgml-skip-tag-forward 1) (point))))
    (backward-char)
    (html-inner-tag)
    (delete-region (point) end-pt)
    (loop while (whitespacep (char-after))
          do (delete-char 1))
    (loop while (whitespacep (char-before))
          do (delete-backward-char 1))
    (insert "\n")
    (indent-according-to-mode)
    (forward-line -1)
    (end-of-line)
    (let ((true-end-pt (point)))
      (goto-char (1+ begin-pt))
      (html-inner-tag)
      (let ((reg-len (- (point) begin-pt)))
        (delete-region begin-pt (point))
        (loop while (whitespacep (char-before))
              do (progn (delete-backward-char 1) (decf reg-len)))
        (loop while (whitespacep (char-after))
              do (progn (delete-char 1) (decf reg-len)))
        (insert "\n")
        (incf reg-len)
        (indent-region (point) (+ end-pt reg-len))
        (loop while (whitespacep (char-after)) do (forward-char))))))

(defun html-raise-tag ()
  (interactive)
  (let* ((end-pt (progn (sgml-skip-tag-forward 1) (point)))
         (beg-pt (progn (sgml-skip-tag-backward 1) (point)))
         (str (buffer-substring beg-pt end-pt))
         (beg-outer-tag (progn (html-beginning-of-tag) (point)))
         (end-outer-tag (progn (sgml-skip-tag-forward 1) (point))))
    (delete-region beg-outer-tag end-outer-tag)
    (loop while (whitespacep (char-before)) do (delete-backward-char 1))
    (loop while (whitespacep (char-after)) do (delete-char 1))
    (insert "\n")
    (indent-according-to-mode)
    (forward-line -1)
    (end-of-line)
    (insert "\n")
    (let* ((beg-pt (point))
           (end-pt (progn (insert str) (point))))
      (indent-region beg-pt end-pt)
      (sgml-skip-tag-backward 1))))

;;; dired
(defun dired-touch-file (filename)
  (interactive (list (read-string "filename: ")))
  (let ((res
         (shell-command-to-string
          (concat
           "touch "
           (shell-quote-argument
            (file-relative-name filename default-directory))))))
    (revert-buffer)
    (goto-char (point-min))
    (re-search-forward (concat "\\s-" (regexp-quote filename) "$"))
    (dired-move-to-filename)
    (message "%s" res)))

;;; random
(defun clear-whitespace (&optional prefix)
  (interactive "P")
  (if prefix
      (delete-horizontal-space)
    (destroy-whitespace-around-me)
    (insert "\n"))
  (indent-according-to-mode))

(defun strip-char-from-format-string (char fmt-str)
  "Removes all instances of %<CHAR> from FMT-STR, where %<CHAR> is not preceded
by another percent."
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     (format "\\(\\(?:[^%%]\\|\\`\\)\\(?:%%%%\\)*\\)%%%s"
             (regexp-quote (char-to-string char)))
     "\\1"
     fmt-str)))

(defun strip-multiple-chars-from-format-string (chars fmt-str)
  (let ((cur-str fmt-str))
    (loop for char in chars
          do (setq cur-str (strip-char-from-format-string char cur-str))
          finally (return cur-str))))

;;; cl extensions
(defun find-multiple-in-seq (seq &rest find-args)
  (if (null find-args) t
    (and (cl-find-if (lambda (item) (equal item (car find-args))) seq)
         (apply #'find-multiple-in-seq (cons seq (cdr find-args))))))
(defun remove-multiple-in-seq (seq &rest remove-args)
  (if (null remove-args) seq
    (apply
     #'remove-multiple-in-seq
     (cons (cl-remove-if (lambda (item) (equal item (car remove-args))) seq)
           (cdr remove-args)))))

;;; TODO: transpose-tags function

;;; js stuff
(defun js-newline-indent-for-real ()
  (interactive)
  (newline-and-indent)
  (save-excursion (forward-line 1) (indent-for-tab-command)))

(defun litcoffee-toggle-code-prose ()
  (interactive)
  (let ((cur-indent 0))
    (beginning-of-line)
    (loop while (and (char-after) (whitespacep (char-after)))
          do (progn (delete-char 1) (incf cur-indent)))
    (when (< cur-indent 4)
      (loop for i from 1 to 4 do (insert " ")))))

(defun open-in-browser (pfx)
  (interactive "P")
  (let ((buf (current-buffer)))
    (eww-open-file (buffer-file-name))
    (when pfx (kill-buffer buf))))

(defun open-dired-the-right-way (pfx)
  (interactive "P")
  (if pfx (dired default-directory) (ido-dired)))

(defun refresh-visual-line-mode ()
  (interactive)
  (visual-line-mode)
  (visual-line-mode))

;;; C-u grep moves point to beginning of buffer
(defadvice grep (around restore-point activate)
  (let ((pt (point))) ad-do-it (goto-char pt)))

(defun num-lines-file ()
  (interactive)
  (message "%s %d %s" "buffer has" (count-lines (point-min) (point-max))
           "lines"))

(defcustom dired-lisp-alist nil
  "Alist of regexps and lisp to run on such regexps using `dired-run-lisp'."
  :group 'dired
  :type '(alist :key-type regexp :value-type (repeat sexp)))

(defun dired-get-default-cmd (files)
  (second
   (car
    (remove-if-not
     (lambda (pair)
       (every (lambda (f) (string-match-p (first pair) f)) files))
     dired-lisp-alist))))

(defun dired-parse-function (args-fun files)
  (mapcar args-fun files))

(defconst +dired-run-lisp-no-command-msg+ "No lisp command provided.")
(defconst +dired-run-lisp-buf-name+ "*Dired Lisp*")

(defun dired-lisp-get-args ()
  (let* ((files (dired-get-marked-files))
         (def (dired-get-default-cmd files)))
    (list files def
          (read-from-minibuffer
           (concat "lisp to run: " (if def (concat "[" def "]") ""))))))

(defun dired-run-lisp (files default func-and-args)
  (interactive (dired-lisp-get-args))
  (if (and (equal func-and-args "") (not default))
      (message "%s" +dired-run-lisp-no-command-msg+)
    (let ((out
           (format
            "%S"
            (dired-parse-function
             (if (equal func-and-args "") default
               (read func-and-args))
             files))))
      (with-current-buffer (get-buffer-create +dired-run-lisp-buf-name+)
        (erase-buffer)
        (insert out))
      (message "%s" out))))

(defun replace-regexp-string-from-alist (str alist &rest replace-args)
  (loop for pair in alist
        do (setq str
                 (apply #'replace-regexp-in-string
                        (append (list (first pair) (second pair) str)
                                replace-args))))
  str)

(defun create-regex-from-wildcard (wild-str)
  (concat
   "\\`"
   (replace-regexp-string-from-alist
    (regexp-quote wild-str)
    '(("\\\\\\*" ".*")
      ("{" "\\(?:")
      ("}" "\\)")
      ("," "\\|"))
    nil t)
   "\\'"))

(defun dired-mark-files-wildcard (wild-str)
  (interactive
   (list (read-from-minibuffer "Mark files (wildcard): ")))
  (dired-mark-files-regexp (create-regex-from-wildcard wild-str)))

(defun dired-flag-marked-files (files)
  (interactive
   (list (dired-get-marked-files)))
  (let ((dired-marker-char dired-del-marker)
        (reg (regexp-opt (mapcar #'file-name-nondirectory files))))
    (message "%s" reg)
    (dired-flag-files-regexp reg)))

;;; TODO: make emacs do threading async stuff
;; (defun dired-run-lisp-async (files default func-and-args)
;;   (interactive (dired-lisp-get-args))
;;   (async-start
;;    (lambda () (funcall #'dired-run-lisp files default func-and-args))
;;    (lambda (msg) (message "%s" msg))))

(defun dired-grep-marked-files (cmd)
  (interactive
   (let* ((files (dired-get-marked-files))
          (default (grep-default-command))
          (grep-cmd
           (read-shell-command
            "grep args: " (if current-prefix-arg default grep-command)
            'grep-history (if current-prefix-arg nil default))))
     (list (concat grep-cmd " " (mapconcat #'shell-quote-argument files " ")))))
  (grep cmd))

(defun clean-certain-buffers (reg)
  (interactive
   (list
    (cond ((equal current-prefix-arg '(4))
           (read-regexp "buffer deletion regex: "))
          ((not current-prefix-arg)
           (concat "\\`" (regexp-quote (expand-file-name default-directory))))
          (t (read-regexp "folder to delete buffers in: ")))))
  (mapc
   (lambda (buf)
     (let* ((buf-fname (buffer-file-name buf))
            (fname (and buf-fname (expand-file-name buf-fname)))
            do-kill)
       (with-current-buffer buf
         (setq do-kill
               (or (string-match-p reg (expand-file-name default-directory))
                   (and fname (string-match-p reg fname)))))
       (when do-kill (kill-buffer buf))))
   (buffer-list)))

(defun get-range-of-list (beg end l)
  (loop
   for el in l
   with cur-count = 0
   while (<= cur-count end)
   with results = nil
   do (progn (when (>= cur-count beg) (push el results)) (incf cur-count))
   finally (return (reverse results))))

(defun show-function-source (fn-arg)
  (interactive
   (let* ((fn (function-called-at-point))
          (enable-recursive-minibuffers t)
          (val
           (completing-read
            (if fn (format "show source for (default %s): " fn)
              "show source for: ")
            obarray 'fboundp t nil nil
            (and fn (symbol-name fn)))))
     (list (if (equal val "")
               fn (intern val)))))
  (message "%S" (symbol-function fn-arg)))

(defun rename-buffer-file (name)
  (interactive
   (list (read-string "new name: ")))
  (when (buffer-file-name) (delete-file (buffer-file-name)))
  (rename-buffer (generate-new-buffer-name name))
  (when (buffer-file-name)
    (set-visited-file-name name)
    (save-buffer)))

(defun beg-of-maybe-visual-line ()
  (interactive "^")
  (if (derived-mode-p 'text-mode) (beginning-of-visual-line)
    (beginning-of-line)))

(defun end-of-maybe-visual-line ()
  (interactive "^")
  (if (derived-mode-p 'text-mode) (end-of-visual-line) (end-of-line)))

(defun beg-of-line-text ()
  (interactive "^")
  (beg-of-maybe-visual-line)
  (while (whitespacep (char-after)) (right-char)))

(defun end-of-line-text ()
  (interactive "^")
  (end-of-maybe-visual-line)
  (while (whitespacep (char-before)) (left-char)))

(defun delete-all-weird-buffers ()
  (interactive)
  (mapatoms
   (lambda (atom)
     (when (and (string-match-p "\\`cleanup" (symbol-name atom))
                (functionp atom))
       (funcall atom)))))

(defun delete-whole-line (n)
  (interactive "p")
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (loop for i from 1 upto n
        do (progn
             (delete-region (line-beginning-position) (line-end-position))
             (unless (eobp) (delete-char 1)))))

(defun skewer-eval-buffer-or-region ()
  (interactive)
  (cl-destructuring-bind (start end)
      (if (use-region-p) (list (min (region-beginning) (region-end))
                               (max (region-beginning) (region-end)))
        (list (point-min) (point-max)))
    (let ((string (buffer-substring-no-properties start end)))
      (skewer-flash-region start end)
      (skewer-eval string #'skewer-post-minibuffer))))

(defun get-emacs-build-time ()
  (interactive)
  (let ((str (format-time-string "%Y-%m-%d@%H:%M:%S" emacs-build-time)))
    (if (called-interactively-p) (message "%s" str) str)))

(defun restart-coffee ()
  (interactive)
  (let* ((cur-wind (get-buffer-window))
         (cof-buf (get-buffer "*CoffeeREPL*"))
         ;; default to current window, when coffee repl not yet started
         (cof-window (get-buffer-window cof-buf)))
    (when cof-buf
      (set-process-query-on-exit-flag (get-buffer-process cof-buf) nil)
      (kill-buffer cof-buf))
    (let* ((new-repl-buf (coffee-repl))
           (new-repl-win (get-buffer-window new-repl-buf)))
      (with-selected-window new-repl-win (previous-buffer))
      (window--display-buffer new-repl-buf cof-window 'reuse)
      (select-window cur-wind))))

(defun fix-sml-smart-pipe (arg)
  (interactive "P")
  (while (looking-back "[[:space:]]") (delete-backward-char 1))
  (sml-electric-pipe)
  (let ((pt (point)))
    (beginning-of-line)
    (insert " ")
    (goto-char (1+ pt))
    (destroy-all-whitespace-nearby)
    (insert "  ")
    (backward-char))
  (let ((pt (point)))
    (beginning-of-line)
    (backward-char)
    (while (looking-back "[[:space:]]") (delete-backward-char 1))
    (if (not arg) (goto-char pt)
      (destroy-all-whitespace-nearby)
      (insert " ")
      (forward-char 2)))
  (insert "a")
  (indent-for-tab-command)
  (delete-backward-char 1))

(defvar async-shell-buffers nil)
(defvar async-shell-buffers-pointer nil)

(defun display-buf-no-win-save-shell-command-buf (buf alist)
  (setq async-shell-buffers-pointer
        (setq async-shell-buffers
              (remove-if-not #'buffer-live-p (cons buf async-shell-buffers))))
  (display-buffer-no-window buf alist))

(defun cycle-shell-buffers (arg)
  (interactive "P")
  (setq async-shell-buffers
        (remove-if-not #'buffer-live-p async-shell-buffers))
  (unless (buffer-live-p (car async-shell-buffers-pointer))
    (setq async-shell-buffers-pointer async-shell-buffers))
  (if (null async-shell-buffers) (message "%s" "No shell buffers found!")
    (let* ((ptr async-shell-buffers-pointer)
           (buf (car ptr)))
      (if (not (buffer-live-p buf))
          (cycle-shell-buffers arg)
        (setq async-shell-buffers-pointer (cdr async-shell-buffers-pointer))
        (if arg (select-window (display-buffer buf)) (switch-to-buffer buf))))))

(defadvice mc/mark-next-like-this (around no-error activate)
  (condition-case err
      ad-do-it
    (error
     (ding)
     (message "%s" (error-message-string err)))))

(defadvice mc/mark-previous-like-this (around no-error activate)
  (condition-case err
      ad-do-it
    (error
     (ding)
     (message "%s" (error-message-string err)))))

(defun clear-beginning-whitespace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p "$")
      (let ((pt (point)))
        (re-search-forward "[^[:space:]]" nil t)
        (backward-char)
        (delete-region pt (point))))))

(defcustom latex-engine "pdflatex"
  "LaTeX engine to use for `latex-compile'.")
(defvar latex-compile-output-buffer "*TeX-Compile-Output*")
(defun latex-compile ()
  (interactive)
  (save-buffer)
  (unless (zerop (call-process latex-engine nil latex-compile-output-buffer t
                               (buffer-file-name)))
    (switch-to-latex-compile-output t)))

(defvar bibtex-command "bibtex")
(defun bibtex-compile ()
  (interactive)
  (unless (zerop (call-process "bibtex" nil latex-compile-output-buffer t
                               (file-name-base)))
    (switch-to-latex-compile-output t)))

(defun switch-to-latex-compile-output (arg)
  (interactive "P")
  (let ((buf (get-buffer latex-compile-output-buffer)))
    (set-window-point
     (if arg (display-buffer buf)
       (display-buffer-same-window buf nil))
     (with-current-buffer buf (point-max)))))

(defvar ediff-prev-window-config nil)
(defadvice ediff (before save-window-config activate)
  (setq ediff-prev-window-config (current-window-configuration)))
(defadvice ediff-buffers (before save-window-config activate)
  (setq ediff-prev-window-config (current-window-configuration)))
(defadvice ediff-quit (after restore-window-config activate)
  (set-window-configuration ediff-prev-window-config))
(defun ediff-these-buffers ()
  "Hack to just ediff what I want."
  (interactive)
  (let ((winlist (window-list)))
    (destructuring-bind (first-win second-win) winlist
      (if (eq second-win (window-in-direction 'right first-win))
          (ediff-buffers (window-buffer first-win) (window-buffer second-win))
        (ediff-buffers (window-buffer second-win) (window-buffer first-win))))))

(defun kill-line-or-region (pfx)
  (interactive "P")
  (cond (pfx (kill-line (if (listp pfx) nil pfx)))
        ((region-active-p) (call-interactively #'kill-region))
        (t (call-interactively #'kill-visual-line))))

(defun TeX-quote-region ()
  (interactive)
  (if (not (region-active-p)) (call-interactively #'TeX-insert-quote)
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (goto-char reg-beg)
      (insert TeX-open-quote)
      (goto-char (+ reg-end (length TeX-open-quote)))
      (insert TeX-close-quote))))

(defun cleanup-ag-buffers ()
  (interactive)
  (loop for buf in (buffer-list)
        do (with-current-buffer buf
             (when (eq major-mode 'ag-mode)
               (kill-buffer)))))

(defun latex-inside-math ()
  (let ((face (plist-get (text-properties-at (point) (current-buffer)) 'face)))
    (eq face 'font-latex-math-face)))

(defun latex-skip-to-end-of-math ()
  (condition-case nil
      (let (res)
        (while (and (latex-inside-math) (not (eobp)))
          (setq res (re-search-forward "\\(\\\\\\)*\\$")))
        res)
    (error nil)))

(defun latex-insert-math ()
  (interactive)
  (if (use-region-p)
      (let ((reg-beg (region-beginning))
            (reg-end (region-end)))
        (goto-char reg-beg)
        (insert "$")
        (goto-char (1+ reg-end))
        (insert "$"))
    (if (latex-inside-math)
        (unless (latex-skip-to-end-of-math)
          (insert "$"))
      (insert "$$")
      (backward-char))))

(defvar markdown-literal-char "`")
(defun markdown-literal-region-too ()
  (interactive)
  (if (not (use-region-p)) (insert markdown-literal-char)
    (let ((reg-beg (region-beginning))
          (reg-end (region-end)))
      (goto-char reg-beg)
      (insert markdown-literal-char)
      (goto-char (+ reg-end (length markdown-literal-char)))
      (insert markdown-literal-char))))

(defun is-in-git-repo (&optional dir)
  (let ((cur-dir (or dir default-directory)))
    (if (string= (expand-file-name cur-dir) (expand-file-name "/"))
        (file-exists-p (expand-file-name (concat cur-dir "/.git")))
      (or (file-exists-p (expand-file-name (concat cur-dir "/.git")))
          (is-in-git-repo (expand-file-name (concat cur-dir "/..")))))))

(defun git-repo-is-github (&optional dir)
  (unless (executable-find "git") (error "no git executable found"))
  (and (is-in-git-repo dir)
       (zerop (shell-command "git remote -v | grep github.com"))))

(defun negativep (num) (< num 0))

(defun prefix-lines-or-region-get-start-num-lines (pfx)
  (cond ((numberp pfx) (list (line-beginning-position) (abs pfx)
                             (if (negativep pfx) 'backward 'forward)))
        ((use-region-p)
         (let ((reg-beg (region-beginning))
               (reg-end (region-end)))
           (save-excursion
             (list
              (progn
                (goto-char reg-beg)
                (line-beginning-position))
              (progn
                (goto-char reg-end)
                (let ((pt (point)))
                  (beg-of-line-text)
                  (let ((res (count-lines reg-beg reg-end)))
                    (if (< (point) pt) (1+ res) res))))
              'forward))))
        (t (error "no numeric prefix or region!"))))

(defun do-keys-for-line (pfx)
  (interactive "P")
  (do-for-line pfx (key-binding
                    (read-key-sequence "key sequence for command:"))))

(defun do-for-line (pfx cmd)
  (interactive (list current-prefix-arg (intern (read-extended-command))))
  (destructuring-bind (start num-lines direction)
      (prefix-lines-or-region-get-start-num-lines pfx)
    (let ((movement (cond ((eq direction 'forward) 1)
                          ((eq direction 'backward) -1)
                          (t (error "invalid direction")))))
      (goto-char start)
      (loop for i from 1 to num-lines
            do (progn
                 (call-interactively cmd)
                 (forward-line movement))))))

(defvar-local ag-args nil)

(defun re-ag-flip-regexp (kwargs)
  (if (plist-get kwargs :regexp) (remove-from-plist kwargs :regexp)
    (append '(:regexp t) kwargs)))

(defun re-ag (pfx)
  (interactive "P")
  (if (not ag-args) (message "%s" "no previous ag found!")
    (destructuring-bind (str dir &rest kwargs) ag-args
      (let* ((regexp-in-kwargs (plist-get kwargs :regexp))
             (now-is-regexp (or (and regexp-in-kwargs (not pfx))
                                (and (not regexp-in-kwargs) pfx)))
             (search-prompt
              (if now-is-regexp "Search regexp: " "Search string: "))
             (new-str (read-from-minibuffer search-prompt str))
             (new-dir (read-directory-name "Directory: " dir))
             (out-list
              (append (list new-str new-dir)
                      (if pfx (re-ag-flip-regexp kwargs) kwargs)))
             (cur-win (selected-window)))
        (kill-buffer)
        (let* ((current-prefix-arg nil)
               (res-buf (apply #'ag/search out-list)))
          (quit-windows-on res-buf)
          (with-selected-window cur-win
            (display-buffer-same-window res-buf nil)))))))

(defun re-ag-reset-args-and-recompile ()
  (interactive)
  (let ((args ag-args))
    (recompile)
    (setq ag-args args)))

(defun git-gutter:diff-and-switch ()
  (interactive)
  (select-window
   (get-buffer-window (call-interactively #'git-gutter:popup-hunk))))

(defun push-buffer-to-kill-ring ()
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "%s" "saved buffer to kill ring!"))

(defun remove-diff-errata (str)
  "Removes leading -/+/=, and removes conflict markers."
  (loop
   for reg-pair in '(("^\\(?:\\+\\+\\)?>\\{7,\\}[^\n]*\\(?:\n\\|\\'\\)" . "")
                     ("^\\(?:\\+\\+\\)?<\\{7,\\}[^\n]*\\(?:\n\\|\\'\\)" . "")
                     ("^\\(?:\\+\\+\\)?|\\{7,\\}[^\n]*\\(?:\n\\|\\'\\)" . "")
                     ("^\\(?:\\+\\+\\)?=\\{7,\\}[^\n]*\\(?:\n\\|\\'\\)" . "")
                     ("^@@[^\n]*\\(?:\n\\|\\'\\)" . "")
                     ("^[-+]" . " "))
   do (setq str (replace-regexp-in-string (car reg-pair) (cdr reg-pair) str))
   finally (return str)))

(defun diff-mode-copy ()
  (interactive)
  (let* ((text
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (buffer-substring-no-properties (point-min) (point-max))))
         (cleaned-text (remove-diff-errata text))
         (space-cleaned-text (replace-regexp-in-string "^ " "" cleaned-text)))
    (kill-new space-cleaned-text)
    (if (derived-mode-p 'magit-mode)
        (set-mark (point))
      (setq deactivate-mark t))))

(defun display-buffer-other-window (buf &optional no-switch)
  (let ((win (display-buffer buf)))
    (if no-switch win (select-window win))))

(defun pop-buf-or-global-mark (pfx)
  (interactive "P")
  (unless (pop-mark) (pop-global-mark)))

(defun my-magit-get-commits-in-current-branch (num)
  (magit-git-lines "log" "--format=%H" "-n"
                   (shell-quote-argument (number-to-string num))))

(defun my-magit-get-head-back (num)
  (cons "HEAD"
        (loop for n from 1 upto (1- num)
              collect (concat "HEAD~" (number-to-string n)))))

(defcustom my-magit-num-commits-back-to-search 50
  "Number of commits to search for `my-magit-read-branch-or-commit'."
  :group 'magit
  :type 'integer)

(defun my-magit-read-branch-or-commit (prompt)
  (or (magit-completing-read
       prompt (append (magit-list-refnames)
                      (my-magit-get-head-back
                       my-magit-num-commits-back-to-search)
                      (my-magit-get-commits-in-current-branch
                       (1+ my-magit-num-commits-back-to-search)))
       nil nil nil 'magit-revision-history "HEAD~1")
      (user-error "Nothing selected")))

(cl-defun resurrect-buffer-from-file (&optional (buf (current-buffer))
                                                (win (selected-window)))
  (interactive)
  (let* ((file (buffer-file-name buf))
         (win-pt (window-point win))
         (win-st (window-start win)))
    (if (and file (file-exists-p file))
        (progn
          (kill-buffer buf)
          (set-window-buffer win (find-file-noselect file))
          (set-window-point win win-pt)
          (set-window-start win win-st))
      (error (format "file for buffer %s doesn't exist!" (buffer-name buf))))))

(defun newline-and-comment ()
  (interactive)
  (call-interactively #'newline-and-indent)
  (call-interactively #'comment-dwim))

(defun factorial (n)
  "Make sure the result isn't greater than `most-positive-fixnum'!"
  (loop for i from 1 upto n
        with base = 1
        do (setq base (* base i))
        finally return base))

(defun eval-sexp-and-newline ()
  (interactive)
  (unless (looking-back "\n\\s-*") (newline-and-indent))
  (eval-last-sexp t)
  (newline-and-indent))

(defconst celsius-start 273.15)
(defun k-to-c (degrees-celsius)
  (- degrees-celsius celsius-start))
(defun c-to-k (degree-kelvin)
  (+ degree-kelvin celsius-start))

(defun run-shell (pfx)
  (interactive "P")
  (let ((buf
         (save-window-excursion (shell nil))))
    (if pfx (pop-to-buffer buf) (switch-to-buffer buf))))

(defun completing-read-multiple
    (prompt table &optional predicate require-match initial-input
            hist def inherit-input-method)
  (loop
   for i from 1
   for res =
   (condition-case nil
       (completing-read (format "(%d) %s" i prompt)
                        table predicate require-match initial-input
                        hist def inherit-input-method)
     (quit nil))
   while res
   collect res))

(defvar start-varname-regexp
  "\\(['[:alnum:]-+!$\",_./:;?<=>#%&*@\\\\|^~]\\)")
(defvar not-start-list-regexp
  "\\([^(\\[{]\\)")
(defvar not-end-list-regexp
  "\\([^)\\]}]\\)")

(defun right-sexp-or-camel ()
  (interactive)
  (let ((right-bound
         (save-excursion
           (call-interactively #'paredit-forward)
           (point))))
    (if (looking-at
         (concat "[[:space:]\n]*" start-varname-regexp not-start-list-regexp))
        (progn
          (call-interactively #'camel-case-right-word)
          (when (> (point) right-bound) (goto-char right-bound)))
      (goto-char right-bound))))

(defun left-sexp-or-camel ()
  (interactive)
  (let ((left-bound
         (save-excursion
           (call-interactively #'paredit-backward)
           (point))))
    (if (looking-back (concat start-varname-regexp "[[:space:]\n]*"))
        (progn
          (call-interactively #'camel-case-left-word)
          (when (< (point) left-bound) (goto-char left-bound)))
      (goto-char left-bound))))

(defadvice dired-rename-file (after revert-stuff activate)
  (revert-buffer))

(defun toggle-subtree-markdown ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (eq (get-char-property (point) 'invisible) 'outline)
        (markdown-show-subtree)
      (markdown-hide-subtree))))


(defun c-lang-insert-block (completions)
  (let* ((block (completing-read "block types: " completions nil t))
         (block-val (cdr (assoc block completions))))
    (insert block "   ")
    (backward-char)
    (save-excursion
      (insert "{}")
      (when block-val (insert ";") (backward-char))
      (backward-char)
      (newline-and-indent)
      (nuke-whitespace-except-this-line))
    (backward-char)))

(defconst c-block-completions '(("struct" . t)))
(defun c-insert-block ()
  (interactive)
  (c-lang-insert-block c-block-completions))

(defconst cxx-block-completions
  (append c-block-completions '(("class" . t) ("namespace" . nil))))
(defun cxx-insert-block ()
  (interactive)
  (c-lang-insert-block cxx-block-completions))

(defun repeat-string (count str)
  (cl-loop for i from 1 upto count
           with start = ""
           do (setq start (concat start str))
           finally return start))

(defun show-the-time ()
  (interactive)
  (let ((fmt (format-time-string "%H:%M:%S@%Y-%m-%d" (current-time))))
    (if (called-interactively-p 'any)
        (message "%s" fmt)
      fmt)))

(defun goto-file-line-at-rev-magit (file line rev &optional other-window)
  (interactive
   (let ((file (buffer-file-name
                (save-window-excursion (call-interactively #'ido-find-file)))))
     (list file
           (read-number "line number: ")
           (let ((default-directory (file-name-directory file)))
             (my-magit-read-branch-or-commit "revision"))
           current-prefix-arg)))
  (let* ((default-directory (file-name-directory file))
         (buf
          (if other-window
              (magit-find-file-other-window rev file)
            (magit-find-file rev file))))
    (with-current-buffer buf
      (goto-line line)
      (recenter))))

(defun make-pred-regexp (reg)
  (apply-partially #'string-match-p reg))

(defconst my-scratch-buffer-name "*scratch*")
(defun get-my-scratch-buffer ()
  (get-buffer-create my-scratch-buffer-name))

(defcustom important-buffers
  '(messages-buffer
    get-my-scratch-buffer
    helm-buffer)
  "A list of regexps matching names of buffers that shouldn't be killed in
`clean-all-buffers-to-deleted-files'.

Can be specified as a regexp, a function which produces a regexp, or a symbol
which evaluates to a regexp."
  :type '(repeat (choice (regexp function variable))))

(require 'rx)
(require 'dash)

(defun neg (fn &optional double-neg)
  (if double-neg
      (lambda (arg) (not (funcall fn arg)))
    (lambda (arg) (logify (funcall fn arg)))))

(defun thick-rx-pred (arg)
  `(pcase arg
     (`(thick-rx ,str)
      (cl-typep str 'regexp))
     (_ nil)))

;;; TODO: maybe accept some argument explaining how to interpret the regexp?
(cl-deftype thick-rx ()
  `(and list
        (satisfies thick-rx-pred)))

(defun eval-buf-spec (spec)
  (cl-etypecase spec
    (function (eval-buf-spec (funcall spec)))
    (symbol (eval-buf-spec (symbol-value spec)))
    (buffer (eval-buf-spec (buffer-name spec)))
    (string (eval-buf-spec `(thick-rx ,(rx-to-string spec t))))
    (thick-rx spec)))

(defconst important-buffer-names-regexp
  (let ((rxs (cl-mapcar (o #'eval-buf-spec #'cl-second) important-buffers)))
    (rx-to-string `(: bos (| ,@(--map `(regexp ,it) rxs)) eos) t)))

(defcustom on-clean-buffers-hook nil
  "Functions to run after `clean-all-buffers-to-deleted-files' for packages who
misbehave (e.g. `helm')."
  :type '(repeat function))

(defun clean-all-buffers-to-deleted-files ()
  (interactive)
  (cl-loop for buf in (buffer-list)
           for name = (buffer-name buf)
           for fname = (buffer-file-name buf)
           when (and (stringp name)
                     (buffer-live-p buf))
           unless (or
                   (string-match-p important-buffer-names-regexp name)
                   (process-live-p (get-buffer-process buf)))
           unless (and (stringp fname)
                     (file-exists-p fname))
           do (kill-buffer buf)))

(defun clean-nonvisiting-buffers ()
  (interactive)
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers)
  (clean-all-buffers-to-deleted-files))

(defun get-linewise-center (beg end)
  (unless (<= beg end) (error (format "beg (%d) is before end (%d)" beg end)))
  (save-excursion
    (goto-char beg)
    (catch 'done
      (cl-loop for i from 0 to (/ (count-lines beg end) 2)
               do (unless (re-search-forward "\n" end t)
                    (throw 'done nil))))
    (point)))

(defun center-function ()
  (interactive)
  (let ((beg (save-excursion (beginning-of-defun) (point)))
        (end (save-excursion (end-of-defun) (point))))
    (goto-char (get-linewise-center beg end))
    (recenter)))

(defun delete-other-windows-maybe-save (pfx)
  (interactive "P")
  (when pfx (save-current-window-configuration))
  (let ((current-prefix-arg nil))
    (delete-other-windows)))

(cl-defun make-keymap-from-bindings (alist &key compose parent)
  (let ((m (make-sparse-keymap)))
    (cl-loop for (key-seq . binding) in alist
             for key = (cl-typecase key-seq
                         (string (kbd key-seq))
                         (otherwise key-seq))
             do (define-key m key binding)
             finally return (make-composed-keymap (cons m compose) parent))))

(defcustom set-mark-end-delay 0.5
  "Delay for `set-mark-end-process-output-mode' to process with an idle timer."
  :type 'float)
(defcustom set-mark-end-quiet nil
  "Whether to message when changing the value of `set-mark-end-do-set'."
  :type 'boolean)
(defvar-local set-mark-end-do-set nil)
(defvar-local set-mark-end-idle-timer nil)
(defvar-local set-mark-end-process-output-mode nil)
(defun set-mark-end-flag (val)
  (setq set-mark-end-do-set val)
  (unless set-mark-end-quiet
    (message "flag `%s' set to '%S'"
             'set-mark-end-do-set set-mark-end-do-set)))
(defun set-mark-end-move (pt)
  (goto-char pt)
  (when (get-buffer-window nil t)
    (redisplay)))
(defun set-mark-end-when-on ()
  (when set-mark-end-do-set
    (set-mark-end-move (point-max))))
(defun set-mark-end-eob (&optional pfx)
  (interactive "P")
  (set-mark-end-flag t)
  (unless pfx
    (set-mark-end-move (point-max))))
(defun set-mark-end-bob (&optional pfx)
  (interactive "P")
  (set-mark-end-flag nil)
  (unless pfx
    (set-mark-end-move (point-min))))
(defun set-mark-end-unset-do-set ()
  (interactive)
  (set-mark-end-flag nil))
(defconst set-mark-end-keymap
  (make-keymap-from-bindings
   '(([remap end-of-buffer] . set-mark-end-eob)
     ("C-x C-x" . set-mark-end-unset-do-set)
     ([remap beginning-of-buffer] . set-mark-end-bob))))
(define-minor-mode set-mark-end-process-output-mode
  "Set point and mark to the end of the buffer every half a second, if already
at the end of the buffer."
  :lighter "M->"
  :keymap set-mark-end-keymap
  (if set-mark-end-process-output-mode
      (setq set-mark-end-idle-timer
            (run-with-idle-timer
             set-mark-end-delay t #'set-mark-end-when-on))
    (cancel-timer set-mark-end-idle-timer)
    (setq set-mark-end-idle-timer nil)))

(defun valid-comment-string-p (str)
  (not (string= str "")))

(defun search-replace-region-if-valid (beg end-mark regexp &optional replace)
  (when (valid-comment-string-p regexp)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward regexp (marker-position end-mark) t)
        (replace-match (or replace ""))))))

(defun region-probably-is-commented (beg end)
  (let ((search (regexp-quote comment-start)))
    (when (valid-comment-string-p search)
      (save-excursion
        (goto-char beg)
        (re-search-forward search end t)))))

(defconst space-non-newline-regexp "[ \t\r\f]")
(defconst not-space-regexp "[^ \t\n\r\f]")

(defun comment-fill-paragraph (beg end)
  (interactive "r")
  (if (not (region-probably-is-commented beg end))
      (fill-region-as-paragraph beg end)
    (let ((pt (point))
          (end-mark (make-marker)))
      (move-marker end-mark end)
      ;; handles case of comments having leading space before them due to bad
      ;;    previous fills (like this)
      (search-replace-region-if-valid
       beg end-mark
       (format "\\(%s\\)%s+\\(%s\\)"
               not-space-regexp space-non-newline-regexp not-space-regexp)
       "\\1 \\2")
      (search-replace-region-if-valid beg end-mark (regexp-quote comment-start))
      (search-replace-region-if-valid beg end-mark (regexp-quote comment-end))
      (goto-char beg)
      (let ((fill-column (- fill-column
                            (+ (length comment-start) (length comment-end)))))
        (fill-region-as-paragraph beg (marker-position end-mark)))
      ;; do this again, because filling the region as paragraph can introduce
      ;; new spaces. this isn't perfect (can be suboptimal fill), but it will be
      ;; correct.
      ;; TODO: may be a way to ensure this works in fill-region
      (search-replace-region-if-valid
       beg end-mark
       (format "\\(%s\\)%s+\\(%s\\)"
               not-space-regexp space-non-newline-regexp not-space-regexp)
       "\\1 \\2")
      (comment-region beg (marker-position end-mark))
      ;; TODO: make this shut up?
      (indent-region beg (marker-position end-mark))
      (if (= pt beg)
          (progn
            (goto-char beg)
            (push-mark (marker-position end-mark) t t))
        (goto-char (marker-position end-mark))
        (push-mark beg t t)))))

(defcustom convert-string-alist
  '((numberp "%c")
    (stringp "%s"))
  "Alist of predicates to format strings for `convert-char-string-or-other'."
  :type '(alist :key-type 'function :value-type 'string)
  :group 'my-customizations)

(defun convert-char-string-or-other (o)
  (or (second (cl-find-if (lambda (el) (funcall (car el) o))
                          convert-string-alist))
      "%S"))

(defun format-char-string-or-other (o)
  (format (convert-char-string-or-other o) o))

(defun switch-to-messages (pfx)
  (interactive "P")
  (let ((msg (messages-buffer)))
    (if pfx (display-buffer-same-window msg nil)
      (display-buffer-other-window msg))))

(defmacro make-fall-through-binding (condition alternative)
  (declare (indent 1))
  (pcase condition
    ((and `(,bindings . ,result) (guard (listp bindings)))
     `(if-let ,bindings (progn ,@result) ,alternative))
    (x (if alternative `(or ,x ,alternative) x))))

(defmacro bind-fall-through (&rest conditions)
  (declare (indent nil))
  (let* ((x (car conditions))
         (xs (cdr conditions)))
    (and x `(make-fall-through-binding ,x
              ,(and xs `(bind-fall-through ,@xs))))))

(defun macroexpand-all-except (expr &optional atomic-macros)
  (let ((hidden-macros-alist
         (cl-mapcar (lambda (mac) (cons mac nil)) atomic-macros)))
    (macroexpand-all expr hidden-macros-alist)))

(defmacro get-and-check (bindings test-form &rest body)
  (declare (indent 2))
  `(let ,bindings (and ,test-form ,@body)))

(cl-defmacro modify-list ((var expr &key in out) &rest body)
  (declare (indent 1))
  (let* ((input (if in `(cl-remove-if-not (lambda (,var) ,in) ,expr)
                  expr))
         (result `(cl-mapcar (lambda (,var) ,@(or body (list var))) ,input)))
    (if out `(cl-remove-if-not (lambda (,var) ,out) ,result)
      result)))

(defun quit-and-kill ()
  (interactive)
  (quit-windows-on (current-buffer) t))

(defvar dismiss-lisp-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map emacs-lisp-mode-map)
    (define-key map (kbd "q") #'quit-and-kill)
    map))
(define-derived-mode dismiss-lisp-mode emacs-lisp-mode "Dismissable"
  "Create a buffer to display emacs lisp which can be dismissed.")

(defun get-string-match (reg str &optional fmt)
  (when (string-match reg str)
    (if (not fmt) (match-string 0 str)
      (let ((data (match-data))
            (all (match-string 0 str)))
        (cl-loop with fmt-str = fmt
                 for i from 0 to 9
                 do (set-match-data data)
                 for group = (match-string i str)
                 while group
                 do (setq fmt-str
                          (replace-regexp-in-string
                           (format "\\(\\(?:\\\\\\)*\\)\\\\%d" i)
                           (concat "\\1" group)
                           fmt-str))
                 finally (return fmt-str))))))

(defun view-macro-expansion (pfx)
  (interactive "p")
  (let* ((sexp (pp-last-sexp))
         (macro-name
          (get-string-match
           "\\`(\\([^[:space:]\n]+\\)" (format "%S" sexp) "\\1"))
         (expanded (format "%S" (macroexpand-1 sexp)))
         (buf (generate-new-buffer
               (format "Macro-View: %s" macro-name))))
    (with-current-buffer buf
      (dismiss-lisp-mode)
      (insert expanded)
      (goto-char (point-min))
      (indent-pp-sexp t))
    (if pfx (display-buffer-other-window buf)
      (pop-to-buffer buf))))

(defmacro catch-nil (&rest body)
  `(condition-case nil
       (progn ,@body)
     (error nil)))

(defmacro do-point (&rest body)
  `(progn ,@body (point)))
(defmacro do-point-save (&rest body)
  `(save-excursion ,@body (point)))

(defun remove-multiple-buffer-copies-name (&optional buf)
  (replace-regexp-in-string
   "<[0-9]+>\\'" "" (if (bufferp buf) (buffer-name buf) buf)))

(defun non-nil-and-equal (&rest els)
  (and els
       (cl-reduce (lambda (a b)
                    (and (equal a b) a))
                  els)))

(defun switch-window-prep-fn (other fn pfx-spec invert nomark)
  (let (failure final-buf (orig-buf (current-buffer))
                win-pt win-st (start-pt (point)))
    (unless nomark
      (let ((mk (mark t)))
        (unless (and mk (= mk start-pt))
          (push-mark start-pt))))
    (save-excursion
      (save-window-excursion
        (let* ((prefix-arg
                (pcase pfx-spec
                  (`same other)
                  (`invert (not other))
                  (`t t)
                  (`nil nil)
                  (_ (user-error "invalid pfx-spec '%s'" pfx-spec))))
               ;; should only have to do one, but oh well
               (current-prefix-arg prefix-arg))
          (call-interactively fn))
        (setq final-buf (current-buffer)
              win-pt (window-point)
              win-st (window-start)
              failure (and (eq orig-buf final-buf)
                           (= start-pt win-pt)))))
    (unless failure
      (let ((win
             (if (xor invert other)
                 (if (eq orig-buf final-buf)
                     (progn
                       (other-window 1)
                       (switch-to-buffer orig-buf)
                       (selected-window))
                   (display-buffer-other-window final-buf))
               (display-buffer-same-window final-buf nil))))
        (with-selected-window win
          (set-window-point (selected-window) win-pt)
          (set-window-start (selected-window) win-st))
        (list win final-buf)))))

(defun operate-on-non-regexp-special (fn str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (let ((mk (make-marker)))
      (set-marker mk (point))
      (cl-loop
       for pt = (point)
       while (re-search-forward regexp-special-coalesced nil t)
       for beg = (match-beginning 0)
       for end = (match-end 0)
       do (set-marker mk end)
       do (goto-char pt)
       for fold-str = (buffer-substring pt beg)
       do (delete-region pt beg)
       do (insert (funcall fn fold-str))
       do (goto-char (marker-position mk))
       finally (progn
                 (let ((final-str (buffer-substring pt (point-max))))
                   (delete-region pt (point-max))
                   (insert (funcall fn final-str)))
                 (return (buffer-string)))))))

(defun not-whitespace-at-point ()
  (save-excursion
    (when (whitespacep (char-before))
      (re-search-backward "[^[:space:]]" nil t))
    (if (re-search-backward "[[:space:]]" nil t)
        (forward-char)
      (goto-char (point-min)))
    (let ((right (save-excursion
                   (re-search-forward "[[:space:]]" nil t)
                   (if (eobp) (point) (1- (point))))))
      (buffer-substring (point) right))))

(defun markdown-get-block-info ()
  (save-excursion
    (cond ((get-text-property (point) 'markdown-gfm-block-begin)
           (forward-line 1)
           (beginning-of-line)
           (unless (get-text-property (point) 'markdown-gfm-code)
             (error "empty block")))
          ((get-text-property (point) 'markdown-gfm-block-end)
           (forward-line -1)
           (beginning-of-line)
           (unless (get-text-property (point) 'markdown-gfm-code)
             (error "empty block")))
          ((not (get-text-property (point) 'markdown-gfm-code))
           (error "not in block")))
    (markdown-block-from-middle
     (get-text-property (point) 'markdown-gfm-code))))

(defun markdown-block-from-middle (val)
  (cl-destructuring-bind (beg end) val
    (list
     :lang (markdown-block-from-top (1- beg))
     :beg beg
     :end end)))

(defun markdown-block-from-top (st)
  (save-excursion
    (goto-char st)
    (beginning-of-line)
    (re-search-forward markdown-regex-gfm-code-block-open)
    (let ((all (replace-regexp-in-string "\\`{\\|}\\'" "" (match-string 2))))
      (replace-regexp-in-string "[[:space:]].*\\'" "" all))))

(defun ess-eval-region-rmd (info pfx)
  (save-window-excursion
    (ess-eval-region (plist-get info :beg) (plist-get info :end) nil))
  (when pfx (ess-switch-to-ESS nil)))

(defun python-eval-region-rmd (info pfx)
  (save-window-excursion (unless (python-shell-get-process) (run-python)))
  (python-shell-send-string
   (buffer-substring (plist-get info :beg) (plist-get info :end))
   nil t)
  (when pfx (python-shell-switch-to-shell t)))

(defvar-local rmd-bash-buffer nil)

(defun bash-eval-region-rmd (info pfx)
  (unless (buffer-live-p rmd-bash-buffer)
    (setq rmd-bash-buffer (save-window-excursion (shell nil))))
  (comint-send-region (get-buffer-process rmd-bash-buffer)
                      (plist-get info :beg) (plist-get info :end))
  (when pfx (pop-to-buffer rmd-bash-buffer)))

(defconst rmd-shell-alist
  '(("r" . ess-eval-region-rmd)
    ("python" . python-eval-region-rmd)
    ("bash" . bash-eval-region-rmd)))

(defun switch-ess-shell ()
  (ess-switch-to-ESS nil))

(defun switch-python-shell ()
  (python-shell-switch-to-shell t))

(defun switch-bash-shell ()
  (if rmd-bash-buffer (pop-to-buffer rmd-bash-buffer)
    (error "no bash buffer available!")))

(defconst rmd-switch-shell-alist
  '(("r" . switch-ess-shell)
    ("python" . switch-python-shell)
    ("bash" . switch-bash-shell)))

(defun markdown-send-to-shell (pfx)
  (interactive "P")
  (let* ((info (markdown-get-block-info))
         (send-fun (cdr (assoc (plist-get info :lang) rmd-shell-alist))))
    (funcall send-fun info pfx)))

(defun markdown-switch-shell (pfx)
  (interactive "P")
  (let* ((info (markdown-get-block-info))
         (switch-fun
          (cdr (assoc (plist-get info :lang) rmd-switch-shell-alist))))
    (funcall switch-fun)))

(defun count-chars-words-lines-buffer (beg end)
  (interactive
   (if current-prefix-arg (list (point-min) (point-max))
     (list (region-beginning) (region-end))))
  (let ((chars (- end beg))
        (words (count-words beg end))
        (lines (count-lines beg end)))
    (message "chars: %d, words: %d, lines: %d"
             chars words lines)))

(defun restart-shell (pfx)
  (interactive "P")
  (when (derived-mode-p 'shell-mode)
    (kill-buffer))
  (run-shell pfx))

(defconst paired-delimiters
  '(("double quotes" . ("\"" . "\""))
    ("single quotes" . ("'" . "'"))
    ("parentheses" . ("(" . ")"))
    ("curly braces" . ("{" . "}"))
    ("square brackets" . ("[" . "]"))))

(defvar paired-delimiter-history nil)

(defun read-paired-delim ()
  (when-let ((delim (completing-read "delimiter type: " paired-delimiters nil t
                                     nil 'paired-delimiter-history)))
    (cdr (assoc delim paired-delimiters))))

(defun search-fix-quotes (pair &optional pfx)
  (interactive (list (read-paired-delim) current-prefix-arg))
  (when pair
    (let ((char-fold-search t)
          (left (car pair))
          (right (cdr pair)))
      (when pfx (goto-char (point-min)))
      (cl-loop
       with lq = nil
       with rq = nil
       while (and (prog1 (re-search-forward left nil t)
                    (setq lq (match-beginning 0)))
                  (progn
                    (prog1 (re-search-forward right nil t)
                      (setq rq (match-end 0)))))
       do (let* ((str (buffer-substring lq rq))
                 (name (format "pair:\n%s\n" str)))
            (unless (x-popup-dialog t `(,name ("ok" . t) ("nah" . nil)))
              (error (format "bad pair:\n%s\n" str))))))))

;;; TODO: make this check pairs using stack!!! and also allow selections of!!!
(defun count-lines-even-char (char &optional pfx)
  (interactive (list (read-char "char to count on lines: ") current-prefix-arg))
  (let ((rx (rx-to-string char)))
    (when pfx (goto-char (point-min)))
    (while (re-search-forward rx nil t)
      (let ((nq (cl-count-if (lambda (ch) (char-equal ch char))
                             (buffer-substring
                              (line-beginning-position)
                              (line-end-position)))))
        (if (oddp nq)
            (user-error
             (format "%d instances of of '%c' at line %d."
                     nq char (line-number-at-pos)))
          (forward-line 1)
          (beginning-of-line))))))

(defconst remove-smart-map
  '(("\"" . (?“ ?”))
    ("'" . (?‘ ?’)))
  "maps replacement strings to dumb unicode characters they should replace")

(defun invert-alist (map)
  (cl-reduce #'append
   (cl-loop with out = nil
            for entry in map
            for str = (car entry)
            collecting (cl-loop for ch in (cdr entry) collect (cons ch str)))
   :initial-value nil))

(defun remove-smart-chars ()
  (interactive)
  (save-excursion
    (cl-loop for (str . chs) in remove-smart-map
             for re = (regexp-opt (cl-mapcar #'char-to-string chs))
             do (progn
                  (goto-char (point-min))
                  (while (re-search-forward re nil t)
                    (replace-match str nil t nil 0))))))

(defun open-if-not-already (file-unexpanded &optional already-progs)
  (let ((file (expand-file-name file-unexpanded)))
    (when (file-exists-p file)
      (let* ((progs (-flatten (cl-loop for prog in already-progs
                                       collect (list "-c" prog))))
             (lsof-args (if progs (append progs (list "-a" file)) file)))
        (with-temp-buffer
          (call-process "lsof" nil t nil lsof-args)
          (when (zerop (buffer-size))
            (call-process "xdg-open" nil t nil file)))))))

;;; TODO: use call-process for this instead???
(defmacro with-shell-command (cmd catch-error &rest body)
  (declare (indent 2))
  (let ((buf (cl-gensym)))
    `(with-temp-buffer
       ,(if catch-error
            `(let ((,buf (current-buffer)))
               (with-temp-buffer
                 (unless (zerop (shell-command ,cmd ,buf (current-buffer)))
                   (throw 'with-shell-command-error
                          (list :command ,cmd
                                :output (with-current-buffer ,buf
                                          (buffer-string))
                                :error (buffer-string))))))
          `(shell-command ,cmd t))
       (goto-char (point-min))
       ,@body)))

(defun get-nth-commit-from (n commit-hash)
  "N can be negative (or 0)."
  (with-temp-buffer
    (shell-command t)))

;; (defun my-magit-next-commit ()
;;   (interactive))

(defgroup my-loc-lib ()
  "Customization for the `my-loc-lib' command.")

(defvar my-loc-lib-choose-on-exit nil)

(defconst my-loc-lib-result-fun #'find-file)

(defcustom my-loc-lib-result-alternatives nil
  "List of functions to call on the result of `my-loc-lib'.

If minibuffer is exited normally, the first element of the list is called, but
if \\[universal-argument] is non-nil on exit, the user can choose among these
alternatives.

If this list is empty, the value of `my-loc-lib-result-fun' is called."
  :group 'my-loc-lib
  :type '(repeat function))

(defun my-loc-lib-exit-choose (pfx)
  (interactive "P")
  (setq my-loc-lib-choose-on-exit pfx)
  (let ((prefix-arg nil))
    (funcall-interactively #'minibuffer-complete-and-exit)))

(defconst my-loc-lib-keymap
  (make-keymap-from-bindings
   '(([remap minibuffer-complete-and-exit] . #'my-loc-lib-choose-on-exit))))

(cl-defmacro append-keymaps-to (orig-map (&rest keymaps) &rest body)
  (declare (indent 2))
  (unless (symbolp orig-map)
    (user-error "orig-map '%S' should be a symbol!"))
  `(let ((,orig-map (make-composed-keymap '(,@keymaps) ,orig-map)))
     ,@body))

(defun my-loc-lib-get-libraries ()
  (apply-partially 'locate-file-completion-table load-path (get-load-suffixes)))

(defvar my-loc-lib-hist nil
  "History for inputs to `my-loc-lib'.")

(defun my-loc-lib (library)
  (interactive
   (append-keymaps-to minibuffer-local-must-match-map (my-loc-lib-keymap)
     (completing-read
      "locate library named: " (my-loc-lib-get-libraries) nil t nil
      'my-loc-lib-hist))))

;; (cl-defun my-cl--transform-args (&rest args)
;;   "See `cl--transform-lambda'."
;;   (let ((arg (car args))
;;         (others (cdr args)))
;;     (pcase arg
;;       ())))

;; (cl-defmacro my-cl-defun (name args &rest body)
;;   )

(defun clean-init-screen ()
  ;;; if everything loaded correctly, clear the last message from minibuf
  (message "")
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (unless (eq (frame-parameter (selected-frame) 'fullscreen) 'fullboth)
    (toggle-frame-fullscreen))
  (garbage-collect))

(cl-defmacro msg-eval (sexp &key (pre "") (format "%S => '%s'") name)
  (declare (indent 1))
  (let ((res (cl-gensym))
        (fmt (cl-gensym)))
    `(let* ((,fmt (concat ,pre ,format))
            (,res ,sexp))
       (message ,fmt ,(or name `',sexp) ,res)
       ,res)))

;;; debugging macros!!!
(defconst my-error-fmt-str
  "error[%1]: %2

%3
data:
%3
%4")

(defun sym-or-key (spec)
  (pcase spec
    (`(,sexp ,name) (list sexp name))
    (_ (list spec spec))))

(cl-defmacro msg-evals
    ;; see `sym-or-key' to understand each SPEC
    ((&rest specs) &key (before "group") (format nil f?) (sep ": "))
  (declare (indent 1))
  (let ((bef (cl-gensym))
        (lead-spc (cl-gensym))
        (fmt (cl-gensym))
        (sp (cl-gensym)))
    `(let* ((,sp ,sep)
            (,bef (concat ,before ,sep))
            (,lead-spc (make-string (length ,bef) ?\ ))
            (,fmt ,format))
       ,@(cl-loop for (sexp name) in (cl-mapcar #'sym-or-key specs)
                  for b-str = bef then lead-spc
                  collect `(msg-eval ,sexp
                             ,@(append `(:pre ,b-str :name ',name)
                                       (when f? `(:format ,fmt))))))))

(cl-defmacro with-eval-after-spec (feature-spec &rest body)
  "Execute BODY after emacs loads features as specified with FEATURE-SPEC. BODY
is executed after loading each feature in the order provided in FEATURE-SPEC.

FEATURE-SPEC can be either a feature name (as an unquoted symbol or a string) or
a file path (as a string). If a file does not `provide' a feature, then its path
can be used instead."
  (declare (indent 1))
  (pcase feature-spec
    ((pred null) `'(progn ,@body))
    (`(,ft . ,others)
     `(eval-after-load (quote ,ft)
        (with-eval-after-spec ,others ,@body)))
    (_ `(with-eval-after-spec (,feature-spec) ,@body))))

(cl-defun expand-insert-macro (&optional (form (sexp-at-point)) full)
  (cl-prettyexpand form full))

(defun valid-regexp-p (str)
  (and (stringp str)
       (condition-case nil
           (prog1 t
             (string-match-p str ""))
         (invalid-regexp nil))))

(cl-deftype regexp ()
  `(and string (satisfies valid-regexp-p)))

(cl-defmacro a-cl-typecase ((keyform &optional do-error) &rest clauses)
  (declare (indent 1))
  `(let ((it ,keyform))
     (,(if do-error
           'cl-etypecase
         'cl-typecase)
      it
      ,@clauses)))

(defun get-rx-match-all (regexp str groups)
  (and (string-match regexp str)
       (cl-mapcar (l (match-string _ str)) groups)))

(defconst rx-match-no-match-sym 'no-match
  "Used in `get-rx-match' to indicate no matches were found.")

(defun get-rx-match (regexp str groups)
  (let ((matches (get-rx-match-all regexp str groups)))
    (--map
     (or it rx-match-no-match-sym)
     matches)))

(cl-defmacro if-rx-match (regexp (&rest names) str if-true &rest if-false)
  "Match REGEXP against STR. If match succeeds, bind NAMES to the matched groups
and evaluate IF-TRUE. Evaluate IF-FALSE (implicit progn) if match fails.

Notes:
- Group 0 is included in matches! Use the underscore `_' to avoid
binding a name."
  (declare (indent 4))
  (once-only (regexp str)
    `(-if-let* (((,@names)
                 (get-rx-match
                  ,regexp ,str ',(number-sequence 0 (1- (length names))))))
         ,if-true
       ,@if-false)))

(cl-defun list-fmt (fmt args)
  (declare (indent 2))
  (--tree-map
   (cl-typecase it
     (symbol
      (if-rx-match list-fmt-special-vars-regexp
          (_ idx-s suffix) (symbol-name it)
          (let* ((idx (unless (string-empty-p idx-s)
                        (cl-parse-integer idx-s)))
                 (arg (when (wholenump idx)
                        (if (< idx (length args))
                            (nth idx args)
                          (user-error
                           "argument at index %d was not provided" idx)))))
            (pcase suffix
              (`no-match arg)
              ((pred stringp)
               (let* ((sym (intern-soft
                            (replace-regexp-in-string "\\`%" "" suffix)))
                      (res (assoc-default sym list-fmt-suffix-actions-alist))
                      (pred (plist-get res :pred))
                      (fun (plist-get res :fun)))
                 (when (and pred (not (funcall pred arg)))
                   (user-error
                    "suffix '%s' cannot be used on argument %d: '%S'"
                    suffix idx arg))
                 (funcall fun arg)))
              (_ (user-error "suffix '%s' could not be parsed" suffix))))
        it))
     (otherwise it))
   fmt))

;; TODO: this!
;; (cl-defun pcase-opt-transform (spec &rest others)
;;   (let ((after (and others (apply #'pcase-opt-transform others))))
;;     (pcase spec
;;       ;; literal
;;       ((pred symbolp) (list 'quote spec))
;;       (`('\, ,(and (pred symbolp) sym) .
;;          ,(or `(,init . ,(or `(,(and (pred symbolp) svar) . ,_)
;;                              (let svar nil)))
;;               (let init nil)))
;;        `(or `('\, ,sym . ))))))

;; TODO: this!
;; (pcase-defmacro \? (&rest specs)
;;   ""
;;   (pcase specs
;;     ((pred null))
;;     (`(,ft) `(or ,()))))

;; (defun pcase-plist-sym-transform (sym)
;;   (let* ((name (symbol-name it)))
;;     (list (make-symbol (replace-regexp-in-string "\\`:" ":" name))
;;           (make-symbol (replace-regexp-in-string "\\`:" "" name)))))

;; (defun pcase-plist-match (spec)
;;   (pcase spec
;;     ((pred symbolp)
;;      (pcase-plist-match (pcase-plist-sym-transform spec)))
;;     (`(,keyw ,var . ,(or `(,initform ,svar))))))

;; TODO: this!
;; (pcase-defmacro \: (&rest args)
;;   ""
;;   (let ((clauses
;;          (--map (pcase it
;;                   ((pred symbolp)
;;                    `(let )))
;;                 args))))
;;   `(and ,@((let ))))

(defun iterate-from (init fn n)
  (cl-loop for i upto n
           for res = init then (funcall fn res)
           finally return res))

(defun add-transient-keys (keys-or-map)
  (let ((new-map
         (make-composed-keymap
          (pcase keys-or-map
            ((pred keymapp) keys-or-map)
            ((pred symbolp) (symbol-value keys-or-map))
            ((pred listp)
             (make-keymap-from-bindings keys-or-map))))))
    (set-transient-map
     new-map t (lambda () (message "unset")))))

;;; FIXME: override `eval-expression-print-format'! make its output more:
;;; 1. interesting -- affects or is correlated with a dis behavior
;;; 2. enlightening -- describe or hint at the "meaning" of an object
;;; 3. customizable -- make it easy to change the output for a type of input

(defun at-lisp-splice-p (_endp _delimiter)
  "Return non-nil when a lisp splice `,@' precedes point. See documentation for
`paredit-space-for-delimiter-predicates'."
  (not (looking-back ",@")))

(defun at-elisp-char-literal-p (_endp _delimiter)
  "Return non-nil when a lisp char literal marker ‘?’ precedes point. See
documentation for `paredit-space-for-delimiter-predicates'."
  (not (looking-back "\\?")))

;;; FIXME: do this!!!
;; (defcustom rx-build-interactive-search-fun #'helm-regexp
;;   "Function to use to perform interactive regexp search."
;;   :type 'function)

;; (defun rx-build (expr)
;;   "Input lisp expression EXPR, and evaluate it each time the input changes. If
;; the input evaluates to a valid regexp, use that regexp to search the current
;; buffer with `helm-regexp'."
;;   (interactive
;;    ()))

(provide 'functions)
