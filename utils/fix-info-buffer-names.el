
(require 'info)
(require 'info-rename-buffer)


(defun fix-info--check-is-top-node-p ()
  (string-equal "Top" Info-current-node))

(defconst fix-info--default-tmp-top-node-name "INDEX"
  "The name to give to `(dir)Top.'")

(defun fix-info--validate-and-remove-last-string-character (s c)
  (cl-check-type s string)
  (cl-check-type c character)

  (if (string-empty-p s)
      (if (fix-info--check-is-top-node-p)
          fix-info--default-tmp-top-node-name
        (error "`Info-breadcrumbs' are weird now? See %S I guess"
               (ignore-errors (Info-breadcrumbs))))
    (let* ((last-index (1- (length s)))
           (last-value (aref s last-index)))
      (cl-assert (char-equal last-value c) t)
      (substring s 0 last-index))))

(defun fix-info--normalize-breadcrumbs ()
  (save-excursion
    (goto-char (point-min))
    (Info-breadcrumbs)))

(defun fix-info-rename-buffer ()
  "Rename current Info buffer to match the `Info-breadcrumbs' it uses to orient itself."
  (interactive)
  (unless (derived-mode-p 'Info-mode)
    (error "`fix-info-rename-buffer' expects to be run within an `Info-mode' buffer"))
  (let* ((crumbs (fix-info--normalize-breadcrumbs))
         (joined (substring-no-properties crumbs))
         (trimmed-header (fix-info--validate-and-remove-last-string-character joined ?\n)))
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
  :global t
  :keymap '(("R" . fix-info-rename-buffer))
  (when info-rename-buffer-mode
    (info-rename-buffer-mode 0))
  (if fix-info-rename-buffer-mode
      (add-hook 'Info-selection-hook #'fix-info-rename-buffer)
    (remove-hook 'Info-selection-hook #'fix-info-rename-buffer)))

;;;###autoload
(advice-add 'Info-set-mode-line
            :override (lambda ()
                        (setq mode-line-buffer-identification
                              (propertized-buffer-identification "%b"))))


(provide 'fix-info-buffer-names)
