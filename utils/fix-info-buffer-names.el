
(require 'info)
(require 'info-rename-buffer)


(defun fix-info--validate-and-remove-last-string-character (s c)
  (cl-check-type s string)
  (cl-check-type c character)
  (let* ((last-index (1- (length s)))
         (last-value (aref s last-index)))
    (cl-assert (char-equal last-value c) t)
    (substring s 0 last-index)))

(defun fix-info-rename-buffer ()
  "Rename current Info buffer to match the `Info-breadcrumbs' it uses to orient itself."
  (interactive)
  (unless (derived-mode-p 'Info-mode)
    (error "`fix-info-rename-buffer' expects to be run within an `Info-mode' buffer"))
  (let* ((crumbs (Info-breadcrumbs))
         (joined (substring-no-properties crumbs))
         (trimmed-header
          (fix-info--validate-and-remove-last-string-character
           joined ?\n)))
    (rename-buffer (generate-new-buffer-name trimmed-header))))


;;;###autoload
(define-minor-mode fix-info-rename-buffer-mode
  "Toggle Fix-Info-Rename-Buffer mode on or off.
With a prefix argument ARG, enable Fix-Info-Rename-Buffer mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil, and toggle it if ARG is
‘toggle’.

When Fix-Info-Rename-Buffer is enabled, all Info buffers' names are
automatically changed to include the current header.
See the command \\[fix-info-rename-buffer]."
  :group 'info
  :init-value nil
  :global tc
  :keymap '(("R" . fix-info-rename-buffer))
  (if fix-info-rename-buffer-mode
      (add-hook 'Info-selection-hook #'fix-info-rename-buffer)
    (remove-hook 'Info-selection-hook #'fix-info-rename-buffer)))

;;;###autoload
(advice-add 'Info-set-mode-line :override (lambda () mode-line-buffer-identification))


(provide 'fix-info-buffer-names)
