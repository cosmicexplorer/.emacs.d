;;; -*- lexical-binding: t -*-

(require 'functions)

(defvar-local book-txt-view nil)
(defvar book-txt-view-hook nil)
(defvar-local book-txt-view-previous-window-cfg nil)
(defvar-local book-txt-view-underlying nil)
(defvar-local book-txt-view-left nil)
(defvar-local book-txt-view-right nil)
(defvar-local book-txt-view-last-start nil)
(defvar-local book-txt-view-last-end nil)
(defvar-local book-txt-view-prev-font nil)

(defcustom book-txt-view-buffer-contents 'fill
  "How to fill each buffer displayed with `book-txt-view'. Used in
`book-txt-view-move-page'."
  :type '(radio (const fill) (const page)))

(defcustom book-txt-view-font "Times New Roman"
  "Which font to use when displaying a book with `book-txt-view'."
  :type 'string)

(defcustom book-txt-view-font-size 16
  "Font size to use when displaying a book with `book-txt-view'."
  :type 'integer)

(defun book-txt-get-percent-done ()
  (with-current-buffer book-txt-view-underlying
    (let* ((pt book-txt-view-last-end)
           (pcnt (/ (float (- pt (point-min))) (- (point-max) (point-min)))))
      (car (cl-round (* 100 pcnt))))))

(defun book-txt-view-setup-locals ()
  (-let* ((name (buffer-name))
          (underlying (current-buffer))
          (mode major-mode)
          (prev-font (face-attribute 'default :font))
          (cfg (current-window-configuration))
          (left-buf (generate-new-buffer
                     (format "*%s<left>*" name)))
          (right-buf (generate-new-buffer
                      (format "*%s<right>*" name))))
    (cl-mapc (l (with-current-buffer _
                  (funcall mode)
                  (visual-line-mode 1)
                  ;; this ensures we go back the same number of lines as we did
                  ;; before when `book-txt-view-buffer-contents' is 'fill
                  (setq mode-line-format
                        '((:eval
                           (format "  %d%%%%" (book-txt-get-percent-done)))))))
             (list left-buf right-buf))
    (cl-mapc
     (l (with-current-buffer _
          (setq book-txt-view t
                book-txt-view-previous-window-cfg cfg
                book-txt-view-underlying underlying
                book-txt-view-left left-buf
                book-txt-view-right right-buf
                book-txt-view-last-start (line-beginning-position)
                book-txt-view-last-end (line-beginning-position)
                book-txt-view-prev-font prev-font
                ;; don't display a cursor in this mode
                cursor-type nil
                cursor-in-non-selected-windows nil)
          (run-hooks 'book-txt-view-hook)))
     (list left-buf right-buf underlying))
    (book-txt-view-move-page 1)))

(defun book-txt-view-kill-locals ()
  (kill-buffer book-txt-view-left)
  (kill-buffer book-txt-view-right)
  (let ((underlying book-txt-view-underlying)
        (cfg book-txt-view-previous-window-cfg)
        (font book-txt-view-prev-font))
    (set-frame-font font)
    (when (buffer-live-p underlying)
      (with-current-buffer underlying
        (goto-char book-txt-view-last-start)
        (setq book-txt-view nil
              book-txt-view-previous-window-cfg nil
              book-txt-view-underlying nil
              book-txt-view-left nil
              book-txt-view-right nil
              book-txt-view-last-start nil
              book-txt-view-last-end nil
              book-txt-view-prev-font nil
              cursor-type t
              cursor-in-non-selected-windows t
              mode-line-format (default-toplevel-value 'mode-line-format))))
    (set-window-configuration cfg)))

(defun book-txt-view-next-point (window-lines &optional count)
  ;; TODO: some way to avoid reliance on `cl-ecase', `pcase', and other
  ;; conditional constructs that don't rely on a config
  ;; variable. `book-txt-view-buffer-contents' ALREADY specifies which values
  ;; are valid -- we shouldn't have to do it this way. we can make an alist with
  ;; a function value, but that seems silly if the function is only one or two
  ;; exprs like here
  (let ((num (or count 1)))
    (cl-ecase book-txt-view-buffer-contents
      (fill
       (forward-line (* num window-lines))
       (line-beginning-position))
      (page
       (forward-page num)
       (line-beginning-position)))))

(defun book-txt-view-move-page (count &optional no-double)
  (cl-check-type count integer)
  (delete-other-windows)
  (when (member book-txt-view-font (font-family-list))
    (set-frame-font
     (format "%s %d" book-txt-view-font book-txt-view-font-size)))
  ;; display left and right buffers in two windows, filling the frame
  (-let* ((to-move (1- count))
          (left-win (selected-window))
          (right-win (split-window-horizontally))
          ((left-lines right-lines)
           (--map
            (with-selected-window it
              (car (cl-round (window-screen-lines))))
            (list left-win right-win)))
          (left-buf book-txt-view-left)
          (right-buf book-txt-view-right)
          (underlying book-txt-view-underlying))
    (cl-mapc (l (with-current-buffer _
                  (read-only-mode -1)))
             (list left-buf right-buf))
    ;; give left and right buffers the current and next page
    (with-current-buffer underlying
      (save-excursion
        (goto-char book-txt-view-last-end)
        (book-txt-view-next-point left-lines to-move)
        (book-txt-view-next-point right-lines to-move)
        (let* ((st-1 (point))
               (st-2 (book-txt-view-next-point left-lines))
               (st-3 (book-txt-view-next-point right-lines))
               (txt-1
                (buffer-substring st-1 st-2))
               (txt-2
                (buffer-substring st-2 st-3)))
          (cl-mapc
           (-lambda ((buf txt))
             (with-current-buffer buf
               (erase-buffer)
               (insert (replace-regexp-in-string "" "" txt))))
           `((,left-buf ,txt-1)
             (,right-buf ,txt-2)))
          (cl-mapc
           (l (with-current-buffer _
                (setq book-txt-view-last-start st-1
                      book-txt-view-last-end st-3)))
           (list left-buf right-buf underlying)))))
    ;; move point to beginning
    (cl-mapc
     (-lambda ((buf win))
       (set-window-buffer-start-and-point win buf 1 1)
       (with-current-buffer buf
         (read-only-mode 1)))
     `((,left-buf ,left-win)
       (,right-buf ,right-win)))
    ;; select the left buffer's window
    (select-window left-win))
  (redisplay))

(defun book-txt-view-next-page (&optional count)
  (interactive "p")
  (book-txt-view-move-page count))

(defun book-txt-view-previous-page (&optional count)
  (interactive "p")
  (book-txt-view-move-page (- count)))

(defun book-txt-view-show-percent-done ()
  (interactive)
  (message "percent done: %d%%" (book-txt-get-percent-done)))

(defconst book-txt-view-keymap
  (make-keymap-from-bindings
   `(("n" . book-txt-view-next-page)
     ("p" . book-txt-view-previous-page)
     ("q" . book-txt-view)
     ("?" . book-txt-view-show-percent-done))))

(define-minor-mode book-txt-view
  "Minor mode for reading text files with two side-by-side windows."
  nil " book-view" book-txt-view-keymap
  (if book-txt-view
      (book-txt-view-setup-locals)
    (book-txt-view-kill-locals)))

(provide 'book-txt-view)
