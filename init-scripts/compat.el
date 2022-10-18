;;; modifications done for compatibility across various interfaces
;;; this will see some changes when i start working with windows this summer

;; fix input issues in xterm (can't hold down shift and up arrow to
;; highlight stuff)
(when (string-match-p "xterm" (or (tty-type) ""))
  (define-key input-decode-map "\e[1;2A" [S-up])
  (define-key input-decode-map "\033[4~" [end]))

;; for like real scrolling in xterm
(xterm-mouse-mode)
;; for interacting with clipboard
(setq x-select-enable-clipboard t)

(with-system 'windows-nt
  (setq explicit-shell-file-name (or (executable-find "zsh")
                                     (executable-find "bash")
                                     (executable-find "sh"))
        shell-file-name explicit-shell-file-name))

(defvar is-cygwin
  (and (executable-find "uname")
       (let ((name-start
              (upcase (substring (shell-command-to-string "uname -a") 0 5))))
         (cond ((string-equal name-start "MINGW") t)
               ((string-equal name-start "CYGWI") t)
               (t nil)))))

;;; This directory is necessary for many system functions on macOS.
(when (and (eq system-type 'darwin) (file-directory-p "/usr/local/bin"))
  (add-to-list 'exec-path "/usr/local/bin"))


;;; TODO: expose these methods like a normal package!
;; (provide 'compat)
