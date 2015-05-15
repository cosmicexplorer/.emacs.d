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
