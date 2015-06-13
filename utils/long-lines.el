;;; finds long lines in buffers

(defcustom long-lines-length 80
  "Maximum length of source code lines."
  :group 'long-lines)

(defun long-line-test ()
  (> (current-column) long-lines-length))

(defun long-line-message-found (fin-col)
  (message "%s %d %s" "moved to long line of"
           fin-col "characters"))

(defun long-line-message-not-found (is-first)
  (message "%s%s%s %d %s" "no " (if is-first "" "succeeding ")
           "line of greater than" long-lines-length "characters found"))

(defun long-line-goto-next ()
  (interactive)
  (let ((prev-pt (point))
        (fin-col nil))
    (forward-line)
    (while (not (or (eobp) (long-line-test)))
      (forward-line)
      (end-of-line))
    (setq fin-col (if (eobp) nil (current-column)))
    (if fin-col
        (progn
          (when (called-interactively-p)
            (long-line-message-found fin-col))
          (move-to-column long-lines-length))
      (when (called-interactively-p)
        (long-line-message-not-found nil))
      (goto-char prev-pt))
    fin-col))

(defun long-line-goto-first ()
  (interactive)
  (let ((prev (point)))
    (goto-char (point-min))
    (let ((res (long-line-goto-next)))
      (if res
          (when (called-interactively-p)
            (long-line-message-found res))
          (when (called-interactively-p)
            (long-line-message-not-found t))
          (goto-char prev))
      res)))

(defun long-line-get-long-lines-alist ()
  (interactive)
  (let ((res
         (save-excursion
           (loop with cur-col = (long-line-goto-first)
                 while cur-col
                 with results = (list (cons (line-number-at-pos) cur-col))
                 do (let ((res (setq cur-col (long-line-goto-next))))
                      (prependn (cons (line-number-at-pos) res) results))
                 finally (return (reverse (cdr results)))))))
    (if (called-interactively-p)
        (message "%S" res)
      res)))

(defun long-line-get-lines ()
  (interactive)
  (let ((res (length (long-line-get-long-lines-alist))))
    (if (called-interactively-p)
        (message "%s %d %s" "the buffer has" res "long lines")
      res)))

(provide 'long-lines)
