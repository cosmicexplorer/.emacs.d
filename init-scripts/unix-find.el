;;; -*- lexical-binding: t -*-

;;; implementation of unix find in pure elisp
;;; might put this on melpa?? idk why anyone would care lol

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
        finally (return (concat "^" result-str "$"))))

(defun unix-find-get-filetype-string (filename)
  (substring (nth 8 (file-attributes filename)) 0 1))

(defun unix-find-get-file-size (filename)
  (nth 7 (file-attributes filename)))

(defun str2num (str-or-num)
  (if (stringp str-or-num)
      (string-to-number str-or-num)
    str-or-num))

(defun unix-find-regex-matcher (match-regexp str-to-match)
  (string-match-p match-regexp str-to-match))

(defun unix-find-create-matcher-helper (arg-group)
  (let ((type (car arg-group))
        (arg (second arg-group)))
    (cond ((eq type :name)
           (let ((reg (unix-find-name-matcher-regexp arg)))
             (lambda (name)
               (let ((case-fold-search nil))
                 (unix-find-regex-matcher reg (file-name-nondirectory name))))))
          ((eq type :iname)
           (let ((reg (unix-find-name-matcher-regexp arg)))
             (lambda (name)
               (let ((case-fold-search t))
                 (unix-find-regex-matcher reg (file-name-nondirectory name))))))
          ((eq type :wholename)
           (let ((reg (unix-find-name-matcher-regexp arg)))
             (lambda (name)
               (let ((case-fold-search nil))
                 (unix-find-regex-matcher reg name)))))
          ((eq type :iwholename)
           (let ((reg (unix-find-name-matcher-regexp arg)))
             (lambda (name)
               (let ((case-fold-search t))
                 (unix-find-regex-matcher reg name)))))
          ((eq type :regex)
           (lambda (name)
             (let ((case-fold-search nil))
               (unix-find-regex-matcher arg name))))
          ((eq type :iregex)
           (lambda (name)
             (let ((case-fold-search t))
               (unix-find-regex-matcher arg name))))
          ((eq type :type)
           (lambda (name)
             (string-equal (unix-find-get-filetype-string name)
                           (if (string-equal arg "f") "-" arg))))
          ((eq type :perm)
           (lambda (name)
             t))
          ((eq type :binary)
           (lambda (name)
             (and arg (file-binary-p name))))
          ((eq type :size)
           (cond ((char-equal (str2char ">") (aref arg 0))
                  (lambda (name)
                    (> (unix-find-get-file-size name)
                       (str2num (substring arg 1)))))
                 ((char-equal (str2char "<") (aref arg 0))
                  (lambda (name)
                    (< (unix-find-get-file-size name)
                       (str2num (substring arg 1)))))
                 (t (lambda (name) (= (unix-find-get-file-size name)
                                      (str2num arg))))))
          (t (throw 'unix-find-unrecognized-arg arg-group)))))

(defun unix-find-group-args (args)
  "Groups arguments for `unix-find'. Consumed by `unix-find-argparse'."
  (loop with cur-arg-ptr = args
        and arg-group-list = nil
        until (null cur-arg-ptr)
        do (let ((item (car cur-arg-ptr)))
             (if (eq item :not)
                 (prependn (nthcdraf 3 cur-arg-ptr) arg-group-list)
               (prependn (nthcdraf 2 cur-arg-ptr) arg-group-list)))
        finally (return (reverse arg-group-list))))

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
  (let* ((depths-list
         (list
          (car (reverse (remove-if-not
                         (lambda (arg) (eq (car arg) :max-depth))
                         max-min-depths-list)))
          (car (reverse (remove-if-not
                         (lambda (arg) (eq (car arg) :min-depth))
                         max-min-depths-list)))))
         (max-dep (second (first depths-list)))
         (min-dep (second (second depths-list))))
    (when (and (numberp max-dep) (numberp min-dep) (< max-dep min-dep))
      (throw 'unix-find-depth-failure
             (concat ":max-depth (" (number-to-string max-dep) ") must be "
                     "greater than or equal to :min-depth ("
                     (number-to-string min-dep) ")")))
    (when (and (numberp max-dep) (< max-dep 1))
      (throw 'unix-find-depth-failure
             (concat ":max-depth (" (number-to-string max-dep)
                     ") must be >= 1")))
    (when (and (numberp min-dep) (< min-dep 1))
      (throw 'unix-find-depth-failure
             (concat ":min-depth (" (number-to-string min-dep)
                     ") must be >= 1")))
    depths-list))

(defun files-except-tree (dir)
  (mapcar
   (lambda (file) (concat dir "/" file))
   (remove-if
    (lambda (file) (or (string-equal file ".") (string-equal file "..")))
    (directory-files dir))))

(defun unix-find (dir &rest args)
  "Recognizes :name, :whole-name, :regex, :not, :max-depth, :min-depth, :type,
:perm, :binary (which uses `file-binary-p'), and :size. Doesn't care about the
positioning of :max-depth and :min-depth. :type recognizes 'd', 'f', 'p', 'l',
and 's', and :size only accepts a number of bytes, as well as a > or < sign in
front. Performs breadth-first search. Probably pretty slow."
  (let* ((parse-results (unix-find-argparse args))
         (checker-lambdas (first parse-results))
         (max-min-depths
          (unix-find-get-max-min-depths (second parse-results)))
         (max-depth (first max-min-depths))
         (min-depth (second max-min-depths)))
    (cl-flet ((passes (name)
                      (reduce (lambda (a b) (and a b))
                              (mapcar (lambda (fun) (funcall fun name))
                                      checker-lambdas)
                              :initial-value t)))
      (loop with dir-list = (list dir)
            and cur-depth = 1
            and res = nil
            while (and dir-list (if max-depth (<= cur-depth max-depth) t))
            do (let ((next-files (mapcan #'files-except-tree dir-list))
                     (prev-dir-list dir-list))
                 (setq dir-list (remove-if-not #'file-directory-p next-files))
                 (unless (and min-depth (< cur-depth min-depth))
                   (setq
                    res (append (remove-if-not
                                 #'passes (append prev-dir-list next-files))
                                res)))
                 (incf cur-depth))
            finally (return res)))))
