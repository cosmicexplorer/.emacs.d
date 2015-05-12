;;; fun stuff that i might release on melpa if anyone cares

(gv-define-simple-setter plist-get plist-put t)

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
