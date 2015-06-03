;;; fun stuff that i might release on melpa if anyone cares

;;; setf stuff that *should* work already lol
(gv-define-simple-setter plist-get plist-put t)
(gv-define-simple-setter getenv setenv t)

(defmacro nconcat (&rest sequences)
  "Destructively modify all of SEQUENCES except for the last. Analogous to
`nconc'."
  (cond ((= (length sequences) 0)
         nil)
        ((= (length sequences) 1)
         `(if (sequencep ,(car sequences))
              ,(car sequences)
            (throw 'not-a-sequence ,(car sequences))))
        (t
         `(setf ,(car sequences)
                (concat ,(car sequences)
                        ,(macroexpand-1 (cons 'nconcat (cdr sequences))))))))

(defun do-for-line (fun &optional buffer-or-string is-buffer-name)
  "Execute FUN on every line of given input (except the final empty line, if
one exists!). If BUFFER-OR-STRING is nil, then use contents of current buffer.
If BUFFER-OR-STRING is a string, then if IS-BUFFER-NAME is non-nil, use contents
of BUFFER-OR-STRING; else, use contents of buffer named BUFFER-OR-STRING."
  (loop for line in
        (split-string (cond ((not buffer-or-string)
                             (buffer-string))
                            ((stringp buffer-or-string)
                             (if is-buffer-name
                                 buffer-or-string
                               (with-current-buffer buffer-or-string
                                 (buffer-string))))
                            ((bufferp buffer-or-string)
                             (with-current-buffer buffer-or-string
                               (buffer-string)))
                            (t (throw 'invalid-argument buffer-or-string)))
                      "\n")
        do (unless (string= line "")
             (funcall fun line))))

(defmacro check-types (exception types-vars-list)
  "Throws EXCEPTION if variables don't match types specified in
TYPES-VARS-LIST. TYPES-VARS-LIST is an alist of type-checking functions and
variable names, in the format:

'(((fun1 fun2 ...) var1 var2 ..) ((fun1 fun2 ...) var1 var2 ...) ...)

A representative example might be:

'(check-types
 (((`numberp' `markerp') a b) ((`stringp' `charp') b c))
 'invalid-argument)."
  `(progn
     ,@(mapcar
        (lambda (type-var-pair)
          `(progn
             ,@(mapcar (lambda (var)
                         `(unless (or
                                   ,@(mapcar (lambda (checker-fun)
                                               `(,checker-fun ,var))
                                             (first type-var-pair)))
                            (throw ,exception ,var)))
                     (rest type-var-pair))))
        types-vars-list)))
(put 'check-types 'lisp-indent-function 1)

;;; this already exists in emacs, but may be removed, according to the
;;; documentation
(defun str2char (str) (aref str 0))

(defun line-before-you ()
  (buffer-substring-no-properties (line-beginning-position) (point)))

(defun rest-of-line ()
  (buffer-substring-no-properties (point) (line-end-position)))

(defun trim-whitespace-in-region (beg end)
  (goto-char beg)
  (loop while (< (point) end)
        with num-cut = 0
        do (if (and (whitespacep (char-after))
                    (string-match-p "^\s*$" (line-before-you)))
               (progn
                 (delete-char 1)
                 (incf num-cut))
             (forward-char))
        finally (return num-cut)))
