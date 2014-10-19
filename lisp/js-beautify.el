;;; js-beautify integration for use with Javascript/HTML/CSS

;; authored by Danny McClanahan, 2014 <danieldmcclanahan@gmail.com>

;; This defines a function js-beautify-region that you can bind to a key.
;; A minimal .emacs would contain:
;;
;;     (load "<path-to-file>/js-beautify.el")
;;     (global-set-key [C-M-tab] 'js-beautify-region)
;;
;; Depending on your configuration and coding style, you might need to uncomment
;; and modify '<path-to-config-file>' in js-beautify, below.

;;; obviously requires jsbeautify, located (currently) at
;;; https://github.com/beautify-web/js-beautify

;;; Configurables
;; Location of the js-beautify binary. If it is on your PATH, a full path name
;; need not be specified.
(defvar js-beautify-binary "js-beautify")
(defvar js-beautify-config-file-location nil)
(defcustom number-of-extra-newlines-to-preserve-js-mode 1
  "Number of blank newlines in a row to preserve when running
js-mode-js-beautify-buffer. A 'newline' here is used to mean a line which
contains only the \n, or newline character."
  :type 'integer)

(defun set-beautify-newlines (num)
  "Modifies the defcustom number-of-extra-newlines-to-preserve-js-mode. Caution:
this function's effect is session-local."
  ;; "nnumber" is not a typo! look at emacs interaction codes
  (interactive "nnumber of free newlines: ")
  (setq number-of-extra-newlines-to-preserve-js-mode num))

;;; js-beautify is great since clang-format doesn't work for formatting js, but
;;; it's kinda dumb sometimes; most significantly, when allowing multiple
;;; consecutive newlines in the outputted data. it doesn't allow them, even when
;;; explicitly set to allow via command-line options. what follows (beyond the
;;; relatively straightforward call-process-region bit) is a bit of a hack to
;;; insert special characters on all blank newlines that js-beautify parses in a
;;; regular way, so that they can be removed and fit back with their newlines
;;; (up to the user-specified maximum).

;;; extremely important that all control chars are unique in this file
(defvar js-base-control-char 0)
(defvar html-string "--html")
(defvar css-string "--css")
(defun js-mode-js-beautify-buffer-base (&optional language-string)
  "Use js-beautify to format the code in the current buffer according to the
specified language string."
  (delete-trailing-whitespace)
  (let ((control-char js-base-control-char))
    (save-excursion
      (goto-char (point-min))
      (loop for line-index from 1 upto (count-num-lines-in-buffer)
            do (progn
                 ;; assumes no trailing whitespace
                 (if (string-equal (get-current-line-as-string) "")
                     (if (not language-string)
                         (progn
                           (insert 97)
                           ;; control character to find and move to later
                           (insert control-char)
                           ;; semicolon so js-beautify indents correctly
                           (insert 59))
                       (if (string-equal language-string html-string)
                           (progn
                             (insert "<style>")
                             (insert control-char)
                             (insert "</style>"))
                         ;; if --css
                         (progn
                           (insert ".a")
                           (insert control-char)
                           (insert "a;")))))
                 (forward-line 1))))
    (let ((orig-point (point)))
      (if language-string
          (call-process-region (point-min) (point-max) js-beautify-binary
                               t (current-buffer) t
                               "-f" "-"
                               language-string)
        (call-process-region (point-min) (point-max) js-beautify-binary
                             t (current-buffer) t
                             "-f" "-"))
      (goto-char orig-point))
    (goto-char (line-end-position))
    ;; cleanup ^A left lying around by putting newlines back in
    (let ((current-num-succeeding-newlines 0)
          (index-of-previous-newline -1)
          (is-over-limit nil))
      (save-excursion
        (loop while (move-point-to-first-occurrence-of-char control-char)
              do (progn
                   (if (if is-over-limit
                           (= (line-number-at-pos) index-of-previous-newline)
                         (= (1- (line-number-at-pos))
                            index-of-previous-newline))
                       (incf current-num-succeeding-newlines)
                     (progn
                       (setq current-num-succeeding-newlines 1)
                       (setq is-over-limit nil)))
                   (setq index-of-previous-newline (line-number-at-pos))
                   (if (and language-string
                            (string-equal language-string html-string))
                       (progn
                         (if (not (char-equal (preceding-char) 62))
                             (progn
                               (forward-line -1)
                               (delete-lines 1)))
                         (progn
                           (delete-lines 1))))
                   (delete-lines 1)
                   (beginning-of-line)
                   (if (<= current-num-succeeding-newlines
                           number-of-extra-newlines-to-preserve-js-mode)
                       (newline)
                     (setq is-over-limit t))))))))

;;; javascript function
(defun js-mode-js-beautify-buffer ()
  "Use js-beautify to format the JavaScript code in the current buffer."
  (interactive)
  (js-mode-js-beautify-buffer-base))

;;; html function
(defun html-mode-js-beautify-buffer ()
  "Use js-beautify to format the HTML in the current buffer."
  (interactive)
  (js-mode-js-beautify-buffer-base html-string))

;;; css function
(defun css-mode-js-beautify-buffer ()
  "Use js-beautify to format the CSS in the current buffer."
  (interactive)
  (js-mode-js-beautify-buffer-base css-string))


;;; inserts and removes special characters as before, in a more limited manner
(defvar js-newline-indent-control-char 1)
(defun js-mode-newline-and-indent-js-beautify ()
  "Creates a newline and indents according to the js-beautify rules, along with
re-beautifying the buffer."
  (interactive)
  (let ((control-char js-newline-indent-control-char))
    ;; insert null character to get point back after js-beautify operation
    (newline)
    ;; insert a
    (insert 97)
    (insert control-char)
    ;; insert semicolon
    (insert 59)
    (js-mode-js-beautify-buffer)
    (if (move-point-to-first-occurrence-of-char control-char)
        (progn
          (forward-char 2)
          (delete-backward-char 3)))))

;;; in case you wanna do something that js-mode-newline-and-indent-js-beautify doesn't allow
(defun javascript-newline-and-indent-ctrl-j-override ()
  "Creates a newline and indents according to js-mode rules. Used in conjunction
with js-mode-newline-and-indent-js-beautify for more fine-grained control."
  (interactive)
  (let ((chars-from-end-of-line (- (line-end-position) (point))))
    (newline-and-indent)
    (insert-char 97)                    ; insert a
    (insert-char 59)                    ; insert semicolon
    (move-point-to-beginning-of-line)
    (js-indent-line)
    (move-point-to-end-of-line)
    (backward-char chars-from-end-of-line)
    (if (/= chars-from-end-of-line 0)
        (progn
          (newline-and-indent)
          (move-point-to-beginning-of-line)
          (backward-char)))
    (delete-backward-char 2)))

(defun html-mode-newline-and-indent-js-beautify ()
  "Creates a newline and indents according to the js-beautify rules, along with
re-beautifying the buffer."
  (interactive)
  (let ((control-char js-newline-indent-control-char))
    (newline-and-indent)
    (insert 60)                         ; <
    (insert 97)                         ; a
    (insert 62)                         ; >
    (insert control-char)               ; ^A
    (insert 60)                         ; <
    (insert 47)                         ; /
    (insert 97)                         ; a
    (insert 62)                         ; >
    (html-mode-js-beautify-buffer)
    (if (move-point-to-first-occurrence-of-char control-char)
        (progn
          (forward-char 5)
          (delete-backward-char 8)))))

(defun css-mode-newline-and-indent-js-beautify ()
  "Creates a newline and indents according to the js-beautify rules, along with
re-beautifying the buffer."
  (interactive)
  (let ((control-char js-newline-indent-control-char))
    (newline-and-indent)
    (insert 97)    ; a
    (insert control-char) ; ^@
    (insert 97)    ; a
    (insert 44)    ; ,
    (css-mode-js-beautify-buffer)
    (if (move-point-to-first-occurrence-of-char control-char)
        (progn
          (forward-char 3)
          (delete-backward-char 4)))))

;;; Used for all other beautify functions
(defun move-point-to-first-occurrence-of-char (char)
  (let ((is-found nil) (index-found))
    (loop for index from (point-min) upto (1- (point-max))
          do (if
                 (and (not is-found)
                      (char-equal (char-after index) char))
                 (setq index-found index
                       is-found t)))
    (if is-found
        (goto-char index-found))
    is-found))

(provide 'js-beautify)
