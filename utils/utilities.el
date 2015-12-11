;;; -*- lexical-binding: t -*-

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

(defun str2num (str-or-num)
  (if (stringp str-or-num)
      (string-to-number str-or-num)
    str-or-num))

(defun line-before-you ()
  (buffer-substring-no-properties (line-beginning-position) (point)))

(defun line-after-you ()
  (buffer-substring-no-properties (point) (line-end-position)))

(defun trim-whitespace-in-region (beg end)
  (goto-char beg)
  (loop while (< (point) end)
        with num-cut = 0
        do (if (and (whitespacep (char-after))
                    (string-match-p "^[[:space:]]*$" (line-before-you)))
               (progn
                 (delete-char 1)
                 (incf num-cut))
             (forward-char))
        finally (return num-cut)))

(defun buffer-binary-p (&optional buffer-or-name byte-check-limit)
  "If BUFFER-OR-NAME (defaults to current buffer) contains a null char within
the first BYTE-CHECK-LIMIT (defaults to 2000) characters, returns the point of
the null char, else nil. It should be noted that this is a *HEURISTIC*."
  (let ((buf (or buffer-or-name (current-buffer)))
        ;; number of bytes to check for null chars
        (lim (or byte-check-limit 2000)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (search-forward (char-to-string 0) lim t)))))

(defun file-binary-p (file-path &optional byte-check-limit)
  "See documentation for `buffer-binary-p'. Does not affect whether or not emacs
is visiting the specified file."
  (unless (file-directory-p file-path)
    (with-temp-buffer
      (insert-file-contents-literally file-path nil 0 byte-check-limit)
      (buffer-binary-p nil byte-check-limit))))

(defmacro cdrf (arg)
  "Destructive cdr."
  (let ((arg-val (gensym)))
    `(let ((,arg-val ,arg))
       (setf ,arg (cdr ,arg-val)))))

(defmacro cdraf (arg)
  "Destructively cdrs ARG, and returns the previous value."
  (let ((arg-val (gensym))
        (first-val (gensym)))
    `(let* ((,arg-val ,arg)
            (,first-val (car ,arg-val)))
       (setf ,arg (cdr ,arg-val))
       ,first-val)))

(defmacro nthcdraf (n arg)
  "Sets ARG to the Nth cdr of itself, and returns a list from ARG to (n-1)th cdr
of ARG. (>= n 0), and if the list runs out before n does, this terminates."
  (let ((n-val (gensym))
        (arg-val (gensym))
        (counter (gensym))
        (res (gensym)))
    `(let ((,n-val ,n)
           (,arg-val ,arg)
           (,counter 0)
           (,res nil))
       (loop while (and ,arg-val (< ,counter ,n-val))
             do (progn
                  (setq ,res (cons (cdraf ,arg-val) ,res))
                  (incf ,counter))
             finally (progn
                       (setf ,arg ,arg-val)
                       (return (reverse ,res)))))))

(defmacro prependn (val arg)
  "Destructively prepends VAL to ARG."
  (let ((arg-val (gensym))
        (val-val (gensym)))
    `(let ((,arg-val ,arg)
           (,val-val ,val))
       (setf ,arg (cons ,val-val ,arg-val)))))

(defmacro apply-logic-op-to-predicate (logic-op val pred &rest args)
  `(,logic-op ,@(mapcar (lambda (arg) `(,pred ,val ,arg)) args)))
(put 'apply-logic-op-to-predicate 'lisp-indent-function 3)
;;; same as above, accepting list instead of varargs
(defmacro apply-log-op-to-pred-list (logic-op val pred args)
  `(,logic-op ,@(mapcar (lambda (arg) `(,pred ,val ,arg)) args)))

(defmacro make-lambda-maybe-not (arg-name not-p &rest body)
  `(lambda (,arg-name)
     ,(if not-p
          `(not (progn ,@body))
        `(progn ,@body))))
(put 'make-lambda-maybe-not 'lisp-indent-function 2)

(defun concat-n (str n &optional sep final-sep)
  (if (zerop n) ""
    (concat
     (reduce (lambda (a b) (concat a (or sep "") b))
             (loop for i from 1 to n
                   collect str))
     (or final-sep ""))))

(defun and-fun (a b)
  (and a b))

(defun or-fun (a b)
  (or a b))

(defun trim-whitespace (str)
  (replace-regexp-in-string
   "[[:space:]\r\n]+\\'" ""
   (replace-regexp-in-string "\\`[[:space:]\r\n]+" "" str)))

(provide 'utilities)
