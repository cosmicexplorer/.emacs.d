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

(defun line-before-you ()
  (buffer-substring-no-properties (line-beginning-position) (point)))

(defun line-after-you ()
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
  (with-temp-buffer
    (insert-file-contents-literally file-path nil 0 byte-check-limit)
    (buffer-binary-p nil byte-check-limit)))

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
       (loop while (or (null ,arg-val) (< ,counter ,n-val))
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

(defun unix-find-group-args (args)
  "Groups arguments for `unix-find'. Consumed by `unix-find-argparse'."
  (loop with cur-arg-ptr = nil
        and arg-group-list = nil
        until (null cur-arg-ptr)
        do (cond ((eq cur-arg-ptr :not)
                  (prependn (nthcdraf 3 cur-arg-ptr) arg-group-list))
                 ((apply-logic-op-to-predicate or cur-arg-ptr eq
                    :name :regex :max-depth :min-depth :type :perm :binary
                    :size)
                  (prependn (nthcdraf 2 cur-arg-ptr) arg-group-list)))
        finally (return (reverse arg-group-list))))

;;; matching functions
(defun unix-find-name-matcher-regexp (match-str)
  "Creates shell-like wildcard semantics for MATCH-STR by transforming into the
appropriate regular expression."
  (loop for char across match-str
        with result-str = ""
        do (cond ((char-equal char (str2char "*"))
                  (setq result-str (concat result-str ".*")))
                 ((char-equal char (str2char "{"))
                  (setq result-str (concat result-str "\\(")))
                 ((char-equal char (str2char "}"))
                  (setq result-str (concat result-str "\\)")))
                 ((char-equal char (str2char ","))
                  (setq result-str (concat result-str "\\|")))
                 (t
                  (setq result-str (concat result-str (char-to-string char)))))
        finally (return result-str)))

(defun unix-find-regex-matcher (match-regexp str-to-match)
  (string-match-p match-regexp str-to-match))

;;; TODO: this closes over not-p in unix-find-create-matcher for some reason!
;; (defmacro make-lambda-maybe-not (arg-name not-p &rest body)
;;   `(lambda (,arg-name)
;;      ,(if not-p
;;           `(not (progn ,@body))
;;         `(progn ,@body))))
;; (put 'make-lambda-maybe-not 'lisp-indent-function 2)
;; (setq a "foo")
;; (macroexpand-1 '(make-lambda-maybe-not arg nil (concat a arg))) ; works??

(defun unix-find-get-filetype-string (filename)
  (substring (nth 8 (file-attributes filename)) 0 1))

(defun unix-find-get-file-size (filename)
  (nth 7 (file-attributes filename)))

(defun unix-find-create-matcher-helper (arg-group)
  (let ((type (car arg-group))
        (arg (second arg-group)))
    (cond ((eq type :name)
           (lambda (name)
             (unix-find-regex-matcher
              (unix-find-name-matcher-regexp arg) name)))
          ((eq type :regex)
           (lambda (name)
             (unix-find-regex-matcher arg name)))
          ((eq type :type)
           (lambda (name)
             (char-equal (unix-find-get-filetype-char name)
                         (if (string-equal arg "f" "-") arg))))
          ((eq type :perm)
           (lambda (name)
             t))
          ((eq type :binary)
           (lambda (name)
             (and arg (file-binary-p name))))
          ((eq type :size)
           (cond ((char-equal (str2char ">") (aref arg 0))
                  (lambda (name)
                    (> (unix-find-get-file-size name) (substring arg 1))))
                 ((char-equal (str2char "<") (aref arg 0))
                  (lambda (name)
                    (< (unix-find-get-file-size name) (substring arg 1))))
                 (t (lambda (name) (= (unix-find-get-file-size name) arg))))))))

(defun unix-find-create-matcher (arg-group &optional is-not)
  "Turns an argument group from `unix-find-group-args' into a matching
function, returning a negation of the function if IS-NOT is non-nil."
  (let ((fun (unix-find-create-matcher-helper arg-group)))
    (if is-not
        ;; annoying, but macros aren't working, so this is the best we can do
        (lambda (arg-group)
          (not (funcall fun)))
      fun)))

(defun unix-find-argparse (args)
  "Parses arguments for `unix-find'. Returns list of matching functions."
  (loop for arg in (unix-find-group-args args)
        with max-min-depths = nil
        and lambdas = nil
        do (if (apply-logic-op-to-predicate or (car arg) eq
                 :max-depth :min-depth)
               (prependn arg max-min-depths)
             (prependn (unix-find-create-matcher arg) lambdas))
        finally (return (list (reverse lambdas) (reverse max-min-depths)))))

(defun unix-find-get-max-min-depths (max-min-depths-list)
  (list
   (car (reverse (remove-if-not
                  (lambda (arg) (eq (car arg) :max-depth))
                  max-min-depths-list)))
   (car (reverse (remove-if-not
                  (lambda (arg) (eq (car arg) :min-depth)))))))

(defun unix-find-helper
    (dir checker-lambdas cur-depth &optional max-depth min-depth)
  (flet ((passes (name)
                 (reduce (lambda (a b) (and a b))
                         (mapcar (lambda (fun) (funcall fun name))
                                 checker-lambdas))))
    (if (and (or (not max-depth) (>= max-depth)))
        (let* ((files (directory-files dir))
               (results nil)
               (next-folders
                (remove-if-not #'file-directory-p files)))
          (when (or (not min-depth) (>= cur-depth min-depth))
            (apply #'append (cons (remove-if
                                   #'null
                                   (mapcar (lambda (filename)
                                             (when (passes filename)
                                               filename))
                                           files))
                                  (mapcar
                                   (lambda (directory)
                                     (unix-find-helper
                                      directory checker-lambdas
                                      (1+ cur-depth) max-depth min-depth))
                 )))))
      nil)))

(defun unix-find (dir &rest args)
  "Recognizes :name, :regex, :not, :max-depth, :min-depth, :type, :perm,
:binary (which uses `file-binary-p'), and :size. Doesn't care about the
positioning of :max-depth and :min-depth. :type recognizes 'd', 'f', 'p', 'l',
and 's', and :size only accepts a number of bytes. Performs breadth-first
search. Probably pretty slow."
  (let* ((cur-directory-list (list dir)) ; current level of bfs
         ;; list of functions saying whether to match the current file
         (parse-results (unix-find-argparse args))
         (checker-lambdas (first parse-results))
         (max-min-depths
          (unix-find-get-max-min-depths (second parse-results))))
    (unix-find-helper dir checker-lambdas 1
                      (first max-min-depths) (second max-min-depths))))
