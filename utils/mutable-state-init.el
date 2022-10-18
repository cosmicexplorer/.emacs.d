;;; -*- lexical-binding: t -*-


;;;;; Execute various setup scripts needed to make emacs work correctly, but that aren't deeply
;;;;; important enough to define directly in .emacs.


;;;;; Funky emacs-specific logic in implementation methods.

(defun do-not-offer-to-save-this-buffer ()
  "Switch off the local `buffer-offer-save' flag in the current buffer."
  (setq-local buffer-offer-save nil))

(defconst buffer-disable-prompt-regexp
  (rx (: bos (| space
                "magit-process"
                (: "*" (* anything) "*" eos))))
  "Regexp matching buffer names. It should not match against any real files.")

(defun buffer-should-die-on-exit-p (buf)
  "Test whether we want BUF to be quietly killed at emacs exit.

Well-behaved process buffers *should* set this themselves, so this covers the ones that currently
don't.

The name matching is HIGHLY SUBJECTIVE and YMMV!"
  ;; Only necessary to check if this buffer has *not* had its buffer prompt flag switched off.
  (when (with-current-buffer buf
          buffer-offer-save)
    (let ((name (buffer-name buf)))
      (string-match-p buffer-disable-prompt-regexp name))))

(defun do-not-offer-to-save-any-special-buffers ()
  "Call `do-not-offer-to-save-this-buffer' on anything matching `buffer-should-die-on-exit-p'.

The input to iterate over is always the entire `buffer-list'."
  (cl-loop for buf in (buffer-list)
           when (buffer-should-die-on-exit-p buf)
           do (with-current-buffer buf
                (do-not-offer-to-save-this-buffer))))


;;;;; Autoloaded methods!

;;;###autoload
(defun setup-buffer-save-prompts ()
  "Try to unset `buffer-offer-save' for all relevant buffers, then set hooks to keep it around.

We re-run these check upon `buffer-list-update-hook'."
  ;; (1) Set buffer prompt flags once immediately.
  (do-not-offer-to-save-any-special-buffers)

  ;; (2) Ensure the `*Messages*' buffer is killed without protest on exit.
  (with-current-buffer (messages-buffer)
    (do-not-offer-to-save-this-buffer))

  ;; (3) Set buffer prompt flags upon any change to the buffer list.
  (add-hook 'buffer-list-update-hook #'do-not-offer-to-save-any-special-buffers))

;;;###autoload
(defun double-checked-server-init ()
  "Start emacs in server form so i can use emacsclient to edit files on the CLI."
  ;; Checking whether a specific function is bound is how emacs signals the server is online.
  (when (fboundp 'server-running-p)
    (unless (server-running-p)
      (server-start))))

(provide 'mutable-state-init)
