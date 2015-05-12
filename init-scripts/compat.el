;;; modifications done for compatibility across various interfaces
;;; this will see some changes when i start working with windows this summer

;; fix input issues in xterm (can't hold down shift and up arrow to
;; highlight stuff)
(when (string-match "xterm" (if (tty-type) (tty-type) ""))
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\033[4~" [end]))

;; for like real scrolling in xterm
(xterm-mouse-mode)
;; for interacting with clipboard
(setq x-select-enable-clipboard t)
(with-system gnu/linux
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))
