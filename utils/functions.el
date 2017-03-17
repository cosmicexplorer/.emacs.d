;;; -*- lexical-binding: t -*-

;;; some of these are mine, some are heavily adapted from emacswiki, some are
;;; copy/paste from emacswiki

(eval-when-compile (require 'cl))
(require 'utilities)

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

(defun load-display-time ()
  "load the display time upon invocation!"
  (setq display-time-day-and-date t)
  (display-time))

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
               (if (and (string-match-p "[^[:space:]]" cur-line)
                        (string-match
                         "^\\([^:]+\\):\"\\([^\"]+\\)\":\\([[:digit:]]+\\)$"
                         cur-line))
                   (let ((active-filetype
                          (match-string 1 cur-line))
                         (active-filename
                          (match-string 2 cur-line))
                         (active-point
                          (match-string 3 cur-line)))
                     (unless (or (string= cur-line "")
                                 (string= active-filename saved-files))
                       (cond ((string= active-filetype "visit")
                              (when (file-exists-p
                                     (file-truename active-filename))
                                (with-demoted-errors "save-session: %S"
                                  (with-current-buffer
                                      (find-file-noselect
                                       (file-truename active-filename))
                                    (goto-char
                                     (string-to-number active-point))))))
                             ((string= active-filetype "directory")
                              (with-demoted-errors "save-session: %S"
                                (with-current-buffer
                                    (find-file-noselect
                                     (file-truename active-filename))
                                  (goto-char (string-to-number active-point))
                                  (message
                                   "%s" (concat "Opened " (buffer-name))))))
                             (t (throw 'no-such-active-filetype
                                       (concat "i don't recognize "
                                               active-filetype "!"))))
                       (message "")))
                 (unless (string= cur-line "")
                   (with-current-buffer "*scratch*"
                     (insert (concat "couldn't parse this line of "
                                     saved-files ": \"" cur-line "\""))
                     (newline))))
               (forward-line)))
    (kill-buffer))
  (clean-nonvisiting-buffers))

(defun compose-helper (arg quoted-funs)
  (if (null quoted-funs) arg
    (compose-helper (list #'funcall (car quoted-funs) arg) (cdr quoted-funs))))

(defmacro compose (&rest args)
  `(lambda (&rest args) ,(apply #'compose-helper (list 'arg args))))

(defvar init-loaded-fully nil
  "Set to t after init loads fully.")
(defun save-visiting-files-to-buffer (&optional no-clean-bufs)
  (interactive "P")
  ;; used so that init failures (which do not load any files from the
  ;; saved-files file) do not delete all history
  (when init-loaded-fully
    (unless no-clean-bufs (clean-nonvisiting-buffers))
    ;; TODO: make this more error-resistant, somehow. having to send emacs a
    ;; sigterm because this function fails on quit is annoying.
    (with-current-buffer (find-file saved-files)
      (erase-buffer)
      (loop for buf in
            (sort (buffer-list)
                  (lambda (a b) (string-lessp (buffer-name a) (buffer-name b))))
            do (let ((bufname (and (buffer-file-name buf)
                                   (file-truename (buffer-file-name buf))))
                     (dired-dir (with-current-buffer buf
                                  (and (eq major-mode 'dired-mode)
                                       dired-directory)))
                     (buf-pt (with-current-buffer buf (point))))
                 (if dired-dir (message "dired-dir: %s" dired-dir))
                 (cond ((not (or (not bufname)
                                 (string-equal bufname saved-files)))
                        (insert (concat "visit" ":\""
                                        bufname "\":"
                                        (number-to-string buf-pt)))
                        (newline))
                       (dired-dir
                        (insert (concat "directory" ":\""
                                        (expand-file-name dired-dir) "\":"
                                        (number-to-string buf-pt)))
                        (newline)))))
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
  ;; unless is emacs-internal buffer or markdown-live-preview buffer
  (unless (or (char-equal (aref (buffer-name) 0) ? )
              (string= (buffer-name) "*markdown-live-preview-output*"))
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

;;; TODO: fix csharp-maybe-insert-codedoc and the indentation of attributes
;;; (get/set) and methods in c# and see if there's a way to hook into
;;; ido-find-file so that it doesn't fail completely???

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
  (and (zerop
        (call-process
         "git" nil submodule-out-buf nil
         "submodule" "update" "--init" "--recursive"))
       (zerop
        (call-process
         "git" nil submodule-out-buf nil
         "submodule" "foreach" "git" "checkout" "master"))
       (zerop
        (call-process
         "git" nil submodule-out-buf nil
         "submodule" "foreach" "git" "pull" "origin"
         "master"))))

(defun actual-setup-submodules (&optional cb)
  (unless dont-ask-about-git
    (with-internet-connection
     (if (not (executable-find "git"))
         (send-message-to-scratch
          "git not installed! some features will be unavailable.")
       (let ((git-submodule-buf-name "*git-submodule-errors*")
             (prev-wd default-directory))
         (cd init-home-folder-dir)
         (unwind-protect
             (let ((submodule-out-buf
                    (get-buffer-create git-submodule-buf-name)))
               (unless (run-git-updates submodule-out-buf)
                 (throw 'submodule-failure "init failed")))
           (cd prev-wd))
         (kill-buffer git-submodule-buf-name)))))
  (when cb (funcall cb)))

(defvar submodules-to-make nil)
(defun make-submodule (folder-name make-cmd onfinish timeout &rest make-args)
  (add-to-list 'submodules-to-make
               (list folder-name make-cmd onfinish timeout make-args)))
(defvar-local process-should-be-killed nil)
(defun actual-make-submodule (submodule-args)
  (destructuring-bind (folder-name make-cmd cb timeout make-args) submodule-args
    (unless (member folder-name submodule-makes-to-ignore)
      (if (not (executable-find make-cmd))
          (with-current-buffer "*scratch*"
            (insert "couldn't find " make-cmd " to build " folder-name "!"))
        (let ((make-output-buf (get-buffer-create
                                (concat "*" folder-name "-make-errors*")))
              (prev-wd default-directory))
          (cd (concat init-home-folder-dir folder-name))
          (let ((proc
                 (apply #'start-process
                        (append
                         (list (concat "make-" folder-name)
                               (buffer-name make-output-buf) make-cmd)
                         make-args))))
            (set-process-sentinel
             proc
             (lambda (proc ev)
               (if (or (and (stringp ev) (string= ev "finished\n"))
                       (and (with-current-buffer (process-buffer proc)
                              process-should-be-killed)
                            (not (process-live-p proc))))
                   (kill-buffer (process-buffer proc))
                 (when (process-live-p proc) (kill-process proc))
                 (switch-to-buffer (process-buffer proc))
                 (throw 'submodule-make-failure ev))))
            (when timeout
              (run-at-time
               timeout nil
               (lambda (proc)
                 (when (process-live-p proc)
                   (with-current-buffer (process-buffer proc)
                     (setq process-should-be-killed t))
                   (delete-process proc)))
               proc)))
          (cd prev-wd)
          (if cb (funcall cb)))))))
(defun actual-make-all-submodules (&optional cb)
  (mapcar #'actual-make-submodule submodules-to-make)
  (if cb (funcall cb)))

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

(eval-after-load 'helm
  '(defun cleanup-helm-buffers ()
     (interactive)
     (loop for buf in (buffer-list)
           do (with-current-buffer buf
                (when (string-match-p "^\\*[hH]elm[[:space:]\\-]" (buffer-name))
                  (kill-buffer))))))
(eval-after-load 'magit
  '(defun cleanup-magit-buffers ()
     (interactive)
     (loop for buf in (buffer-list)
           do (with-current-buffer buf
                (when (string-match-p "^magit\\-" (symbol-name major-mode))
                  (kill-buffer))))))
(eval-after-load 'dired
  '(defun cleanup-dired-buffers ()
     (interactive)
     (loop for buf in (buffer-list)
           do (with-current-buffer buf
                (when (eq major-mode 'dired-mode)
                  (kill-buffer))))))

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
                                   (when prefix-given (current-buffer)))
                (eval-buffer nil (when prefix-given (current-buffer))))))
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

(defun test-fun (a)
  (interactive (list current-prefix-arg))
  (message "%S" a))

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

(other-window-prefix-wrapper #'ag my-ag)
(other-window-prefix-wrapper #'ag-regexp my-ag-regexp)

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

(defun resurrect-buffer-from-file (&optional buf win)
  (interactive)
  (let* ((buf (or buf (current-buffer)))
         (file (buffer-file-name buf))
         (win (or win (selected-window)))
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

(defconst important-buffer-names '("*Messages*" "*scratch*"))
(defconst important-buffer-names-regexp (regexp-opt important-buffer-names))

(defun clean-all-buffers-to-deleted-files ()
  (interactive)
  (loop for buf in (buffer-list)
        unless (or (and (buffer-name buf)
                        (string-match-p
                         important-buffer-names-regexp (buffer-name buf)))
                   (when-let ((proc (get-buffer-process buf)))
                     (process-live-p proc))
                   (when-let ((fname (buffer-file-name buf)))
                     (file-exists-p fname)))
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

(defcustom set-mark-end-delay 0.5
  "Delay for `set-mark-end-process-output-mode' to process with an idle timer.")
(defvar-local set-mark-end-do-set nil)
(defvar-local set-mark-end-idle-timer nil)
(defvar-local set-mark-end-process-output-mode nil)
(defun set-mark-end-when-at-end ()
  (with-temp-message ""
    (when set-mark-end-do-set
      (let ((inhibit-message t))
        (call-interactively #'end-of-buffer)
        (redisplay)))))
(defun set-mark-end-set-do-set ()
  (interactive)
  (call-interactively #'end-of-buffer)
  (setq set-mark-end-do-set t))
(defun set-mark-end-unset-do-set ()
  (interactive)
  (call-interactively #'beginning-of-buffer)
  (setq set-mark-end-do-set nil))
(define-minor-mode set-mark-end-process-output-mode
  "Set point and mark to the end of the buffer every half a second, if already
at the end of the buffer."
  :lighter "M->"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M->") #'set-mark-end-set-do-set)
            (define-key map (kbd "M-<") #'set-mark-end-unset-do-set)
            map)
  (if set-mark-end-process-output-mode
      (setq set-mark-end-idle-timer
            (run-with-idle-timer
             set-mark-end-delay t #'set-mark-end-when-at-end))
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

(defun switch-window-prep-fn (other fn &optional invert nomark)
  (let (failure final-buf (orig-buf (current-buffer))
                win-pt win-st (start-pt (point)))
    (unless nomark
      (let ((mk (mark t)))
        (unless (and mk (= mk start-pt))
          (push-mark start-pt))))
    (save-excursion
      (save-window-excursion
        (funcall fn)
        (setq final-buf (current-buffer)
              win-pt (window-point)
              win-st (window-start)
              failure (and (eq orig-buf final-buf)
                           (= start-pt win-pt)))))
    (unless failure
      (let ((win
             (if (org-xor invert other)
                 (if (eq orig-buf final-buf)
                     (progn
                       (other-window 1)
                       (switch-to-buffer orig-buf)
                       (selected-window))
                   (display-buffer-other-window final-buf))
               (display-buffer-same-window final-buf nil)
               (selected-window))))
        (with-selected-window win
          (set-window-point (selected-window) win-pt)
          (set-window-start (selected-window) win-st))))))

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

(provide 'functions)
