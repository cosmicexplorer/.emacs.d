;;; -*- lexical-binding: t -*-

(defvar internet-check-url "google.com")

(defun internet-connected-p ()
  (zerop
   (call-process "ping" nil nil nil
                 internet-check-url "-c1")))

;;; add package lists
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(defcustom installed-packages-file
  (expand-file-name "installed-packages" init-home-folder-dir)
  "File path to write a list of installed packages to on shutdown. Used in
`write-packages-to-file' and `install-packages-from-file'."
  :type 'file)

(defun write-packages-to-file ()
  (with-temp-buffer
    (let* ((names (cl-mapcar #'symbol-name package-selected-packages))
           (sorted (cl-sort names #'string-lessp)))
      (cl-loop for s in sorted
               do (insert s "\n"))
      (write-file installed-packages-file))))

;;; do the install (slow upon startup, but only for the first time)
(defun install-packages-from-file ()
  (save-window-excursion
    (let* ((inst-buf (find-file-existing installed-packages-file))
           (inst-file (with-current-buffer inst-buf (buffer-string)))
           (names (split-string inst-file "\n" t))
           (sorted (cl-sort names #'string-lessp))
           (syms (cl-mapcar #'intern sorted))
           (real-syms (cl-remove-if #'package-installed-p syms)))
      (cl-mapc #'package-install real-syms)
      (kill-buffer inst-buf))))

(when (internet-connected-p)
  (package-refresh-contents)
  (install-packages-from-file))

(add-hook 'kill-emacs-hook #'write-packages-to-file)
