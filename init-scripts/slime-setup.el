(require 'languages)

;;; slime sux (jk)
(when-let ((slime-helper-file
            (expand-file-name
             (concat "~/quicklisp/slime-helper.el")))
           (this-directory (if load-file-name (file-name-directory load-file-name)
                             default-directory)))
  (when sbcl-binary
    (setq inferior-lisp-program sbcl-binary)
    (setq slime-contribs '(slime-fancy))
    (if (file-exists-p slime-helper-file)
        (load-file slime-helper-file)
      (if (eq system-type 'gnu/linux)
          (let* ((quicklisp-url "http://beta.quicklisp.org/quicklisp.lisp")
                 (quicklisp-file "quicklisp.lisp"))
            (if (/= 0 (call-process "wget" nil nil nil quicklisp-url
                                    "-qO" quicklisp-file))
                (throw 'setup-failure "quicklisp setup failed!")
              (if (/= 0 (shell-command
                         ;; the echo is because it waits for a newline
                         (concat "echo | " sbcl-binary " --script "
                                 this-directory "/slime-setup.lisp > "
                                 this-directory "/error-log")))
                  (throw 'setup-failure "quicklisp extended setup failed")
                (load-file slime-helper-file)
                (slime-setup '(slime-company))
                (call-process "rm" nil nil nil "-f"
                              (concat this-directory "quicklisp.lisp")
                              (concat this-directory "error-log")))))
        (with-current-buffer "*scratch*"
          (insert (concat "sbcl was found, but you'll have to set it up "
                          "manually since this is not a gnu/linux system. "
                          "Check out "
                "http://www.mohiji.org/2011/01/31/modern-common-lisp-on-linux/."
                     "\nFeel free to contribute your changes back (lol)!")))))))

(provide 'slime-setup)
