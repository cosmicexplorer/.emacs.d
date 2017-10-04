;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dash)

(defun adapters--sym-seq-p (x) (cl-every #'symbolp x))

(defun adapters--non-empty-p (x) (> (length x) 0))

(cl-deftype adapters--type-tuple ()
  `(and list
        (satisfies adapters--non-empty-p)
        (satisfies adapters--sym-seq-p)))

(cl-defmacro adapters--checked-struct (name (&rest slot-names))
  (declare (indent 1))
  `(cl-defstruct ,name
     ,@(cl-mapcar (lambda (slot-name) `(,slot-name nil :read-only t))
                  slot-names)))

(defun adapters--arg-spec-p (x)
  (pcase-exhaustive x
    (`(,(pred symbolp) ,(pred symbolp))
     t)
    (_ nil)))

(cl-deftype adapters--arg-spec ()
  `(and cons (satisfies adapters--arg-spec-p)))

(defun adapters--args-spec-seq-p ()
  (cl-every (lambda (x) (cl-typep x 'adapters--arg-spec))))

(cl-deftype adapters--arg-specs ()
  `(and list
        (satisfies adapters--non-empty-p)
        (satisfies adapters--args-spec-seq-p)))

(defun adapters--arg-specs-names ())

;;; FIXME: make this into a function, elisp nested backquote eval order is weird
(cl-defmacro adapters--type-wrapper (return-type (&rest arg-specs))
  (declare (indent 1))
  (let ((ret-type (cl-gensym))
        (arg-types (cl-gensym))
        (arg-names (cl-gensym))
        (arg-checks (cl-gensym))
        (fun (cl-gensym))
        (ret-fun (cl-gensym))
        (ret-val (cl-gensym)))
    `(let ((,ret-type ,return-type)
           (,arg-types ,arg-specs))
       (cl-check-type ,ret-type symbol)
       (cl-check-type ,arg-types adapters--arg-specs)
       (let ((,arg-names ,`(cl-mapcar #'car ,arg-types))
             (,arg-checks ,`(-map ,(-lambda ((name type))
                                     `(cl-check-type ,name ,type))
                                  ,arg-types)))
         (lambda (,fun)
           ,`(lambda ((,@arg-names))
               ,arg-checks
               (let ((,ret-val (apply ,fun ,arg-names)))
                 (cl-check-type ,ret-val ,ret-type)
                 ,ret-val)))))))

(cl-defun adapters--typed-function (return-type (&rest in-types) fun)
  (declare (indent 2))
  (cl-check-type return-type symbol)
  (cl-check-type in-types adapters--type-tuple)
  (cl-check-type fun function)
  (let ((num-types (length in-types)))
    (lambda (&rest args)
      ())))

;;; TODO: would like to require a value to be provided, but also specify
;;; read-only -- this is not possible with `cl-defstruct'
(cl-defstruct adapters--type
  (tuple nil :read-only t)
  (pred nil :read-only t))

;;; TODO: calling a predicate may fail, if the function provided does not accept
;;; the right number of arguments -- how to handle this? it's hard to check that
;;; the predicate can accept a single argument unless you set restrictions on
;;; its creation
;; (defun adapters--make-type (tuple pred)
;;   (cl-check-type tuple adapters--type-tuple)
;;   (cl-check-type pred function)
;;   (make-adapters--type :tuple tuple :pred pred))

(cl-defstruct adapters--transition
  (from-class nil :read-only t)
  (to-class nil :read-only t)
  (transformation nil :read-only t))

;; (cl-deftype adapters--type-table)

(cl-defstruct adapters--resolver
  ;; obarray
  (type-table nil :read-only t)
  ;; list of
  (transitions nil :read-only t))

(provide 'adapters)
