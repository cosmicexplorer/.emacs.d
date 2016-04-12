(defun jison-between-newlines ()
  (and (not (= (point) (point-min)))
       (not (= (point) (point-max)))
       (char-equal (char-before) ?\n)
       (char-equal (char-after) ?\n)))

(defun jison-extend-region-function (start end _)
  (let ((beg (save-excursion
               (goto-char start)
               (unless (or (looking-back "\n\n") (jison-between-newlines))
                 (save-excursion
                   (if (re-search-backward "\n\n" nil t)
                       (progn (forward-char 2) (point))
                     (point-min))))))
        (end (save-excursion
               (goto-char end)
               (unless (or (looking-at-p "\n\n") (jison-between-newlines))
                 (if (re-search-forward "\n\n" nil t)
                     (progn (backward-char 2) (point))
                   (point-max))))))
    (when beg (setq jit-lock-start beg))
    (when end (setq jit-lock-end end))))

(defvar jison-common-keywords
  '(("^\\(%\\)\\({\\)\\(.*?\\)\n\\(\\(?:.\\|\n\\)*\\)\n\\(%\\)\\(}\\)\\(.*?\\)"
     . ((1 font-lock-preprocessor-face)
        (2 font-lock-keyword-face)
        (3 font-lock-warning-face)
        (4 font-lock-preprocessor-face)
        (5 font-lock-preprocessor-face)
        (6 font-lock-keyword-face)
        (7 font-lock-warning-face)))
    ("/\\*\\(?:.\\|\n\\)*?\\*/" . font-lock-comment-face)
    ("^\\(%\\)\\([_a-zA-Z][_a-zA-Z0-9]*\\)[ \t\r]*\\(.*\\)$" .
     ((1 font-lock-preprocessor-face)
      (2 font-lock-keyword-face)
      (3 font-lock-reference-face)))
    ("^\\(%%\\)\\(.*\\)$" .
     ((1 font-lock-preprocessor-face)
      (2 font-lock-warning-face)))))

(defvar jison-flex-keywords
  (append
   jison-common-keywords
   '(("\\({\\)\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\(}\\)" .
      ((1 font-lock-preprocessor-face)
       (2 font-lock-variable-name-face)
       (3 font-lock-preprocessor-face)))
     ("^[_a-zA-Z][_a-zA-Z0-9]*" . ((0 font-lock-function-name-face)))
     ("[^\\\\]\\(\\\\\\(?:\\\\\\\\\\)*\\)\\(\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)\\)"
      . ((1 font-lock-type-face)
         (2 font-lock-type-face)))
     ("\\(?:\\`\\|[^\\]\\)\\(\\(?:\\\\\\\\\\)*\\)\\(\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)+\\)"
      . ((1 font-lock-preprocessor-face)
         (2 font-lock-keyword-face)))
     ("\\(\\(?:\\\\\\\\\\)+\\)\\(\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)+\\)"
      . ((1 font-lock-preprocessor-face)
         (2 font-lock-keyword-face)))
     ("\\(?:\\\\\\)+." . ((0 font-lock-type-face)))
     ("\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)"
      . ((0 font-lock-keyword-face)))
     ("\\<return\\>" . ((0 font-lock-type-face)))
     ("\\<yytext\\>" . ((0 font-lock-variable-name-face))))))

(define-derived-mode jison-flex-mode fundamental-mode "JFLex"
  "A mode to highlight Flex keywords for use in Jison lex files."
  (setq-local font-lock-defaults '(jison-flex-keywords))
  (setq-local font-lock-string-face nil)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'jison-extend-region-function t t)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/"))

(defface jison-string-face
  '((((class grayscale) (background light)) :foreground "DimGray" :slant italic)
    (((class grayscale) (background dark))  :foreground "LightGray" :slant italic)
    (((class color) (min-colors 88) (background light)) :foreground "VioletRed4")
    (((class color) (min-colors 88) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 16) (background light)) :foreground "RosyBrown")
    (((class color) (min-colors 16) (background dark))  :foreground "LightSalmon")
    (((class color) (min-colors 8)) :foreground "green")
    (t :slant italic))
  "Face mirroring `font-lock-string-face'.")
(defvar jison-string-face 'jison-string-face)

(defvar jison-bison-keywords
  (append
   jison-common-keywords
   '(("^[ \t\r]*\\(;\\)\\(.*\\)" .
      ((1 font-lock-keyword-face)
       (2 font-lock-warning-face)))
     ("^[ \t\r]*\\(:\\||\\)\\(.*?\\)\\(->\\)\\(.*\\)" .
      ((1 font-lock-keyword-face)
       (2 font-lock-type-face)
       (3 font-lock-keyword-face)
       (4 font-lock-preprocessor-face)))
     ("^[ \t\r]*\\(:\\||\\)\\([^{|;]*?\\)\\s-+\\({\\)\\(\\(?:.\\|\n\\)*?\\)\\(}\\)[ \t\r]*$" .
      ((1 font-lock-keyword-face)
       (2 font-lock-type-face)
       (3 font-lock-keyword-face)
       (4 font-lock-preprocessor-face)
       (5 font-lock-keyword-face)))
     ("^[ \t\r]*\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\(.*\\)" .
      ((1 font-lock-variable-name-face)
       (2 font-lock-warning-face)))
     ("^[ \t\r]*\\(:\\||\\)\\(.*\\)" .
      ((1 font-lock-keyword-face)
       (2 font-lock-type-face))))))

(define-derived-mode jison-bison-mode fundamental-mode "JBison"
  "A mode to highlight Bison keywords for use in Jison grammar files."
  (setq-local font-lock-defaults '(jison-bison-keywords))
  (setq-local font-lock-string-face nil)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'jison-extend-region-function t t)
  (setq-local comment-start "/*")
  (setq-local comment-end "*/")
  (local-set-key
   (kbd "(") (lambda () (interactive) (insert "()") (backward-char))))

(provide 'jison-mode)
