(defvar jison-flex-keywords
  '(("/\\*.*?\\*/" . font-lock-comment-face)
    ("\\({\\)\\([a-zA-Z][a-zA-Z0-9]*\\)\\(}\\)" .
     ((1 font-lock-preprocessor-face)
      (2 font-lock-variable-name-face)
      (3 font-lock-preprocessor-face)))
    ("^\\(%\\)\\([a-zA-Z][_a-zA-Z0-9]*\\)\\s-+?\\(.*\\)$" .
     ((1 font-lock-preprocessor-face)
      (2 font-lock-keyword-face)
      (3 font-lock-reference-face)))
    ("^\\(%%\\)\\(.*\\)$" .
     ((1 font-lock-preprocessor-face)
      (2 font-lock-warning-face)))
    ("^[a-zA-Z][a-zA-Z0-9]*" . ((0 font-lock-function-name-face)))
    ("\\(?:\\`\\|[^\\]\\)\\(\\(?:\\\\\\\\\\)*\\)\\(\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)+\\)"
     . ((1 font-lock-preprocessor-face)
        (2 font-lock-keyword-face)))
    ("\\(\\(?:\\\\\\\\\\)+\\)\\(\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)+\\)"
     . ((1 font-lock-preprocessor-face)
        (2 font-lock-keyword-face)))
    ("\\\\+." . ((0 font-lock-type-face)))
    ("\\(?:{\\|}\\|\\[\\|\\]\\|(\\|)\\|\"\\|'\\|\\*\\|\\+\\|\\^\\|,\\|\\-\\||\\|\\.\\|\\?\\)"
     . ((0 font-lock-keyword-face)))
    ("\\<return\\>" . ((0 font-lock-type-face)))
    ("\\<yytext\\>" . ((0 font-lock-variable-name-face)))))

(define-derived-mode jison-flex-mode fundamental-mode "JFLex"
  "A mode to highlight flex keywords for use in Jison."
  (setq-local font-lock-defaults '(jison-flex-keywords))
  (setq-local font-lock-string-face nil))

(provide 'jison-mode)
