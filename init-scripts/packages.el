;;; -*- lexical-binding: t -*-


;;;;; Set up which package repos we connect to, and make all third-party packages visible to
;;;;; e.g. `require'.

;;; Make the concept of packages available!
(package-initialize)

(defgroup package-connections nil
  "Package listing and internet connection settings."
  :group 'my-customizations)

(defcustom internet-check-url "8.8.8.8"
  "Defaults to Google's DNS server. They might track me, but it will definitely be online."
  :type 'string
  :group 'package-connections)

(defun internet-connected-p ()
  "Ping `internet-check-url' to quickly gauge whether we have any internet.

This is expected to fail much faster than trying to ping the package server itself, so it's a good
early exit mechanism e.g. when traveling without wifi."
  (zerop
   (call-process "ping" nil nil nil
                 internet-check-url "-c1")))

;;; Add ELPA and MELPA.
(defcustom ping-melpa t
  "Whether to add MELPA to `package-archives' when searching for new packages."
  :type 'boolean
  :group 'package-connections)

(when ping-melpa
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(defcustom ping-org-elpa t
  "Whether to add org elpa to `package-archives' when searching for new packages."
  :type 'boolean
  :group 'package-connections)

(when ping-org-elpa
  ;; This archive will occasionally time out! >='[
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))


;;;;; Persist the installed packages data to a customizable file path.

(defcustom installed-packages-file (home-dir-path "installed-packages")
  "File path to write a list of installed packages to on shutdown. Used in
`write-packages-to-file' and `install-packages-from-file'."
  :type 'file
  :group 'my-customizations)

(defun write-packages-to-file ()
  "Extract the names of `package-selected-packages' and write them to `installed-packages-file'"
  (with-temp-buffer
    (let* ((names (cl-mapcar #'symbol-name package-selected-packages))
           (sorted (cl-sort names #'string-lessp)))
      (cl-loop for s in sorted
               do (insert s "\n"))
      (write-file installed-packages-file))))

;;; do the install (slow upon startup, but only for the first time)
(defun install-packages-from-file ()
  "Install everything in the `installed-packages-file' that we don't already have."
  (save-window-excursion
    (let* ((inst-buf (find-file-existing installed-packages-file))
           (inst-file (with-current-buffer inst-buf (buffer-string)))
           (names (split-string inst-file "\n" t))
           (sorted (cl-sort names #'string-lessp))
           (syms (cl-mapcar #'intern sorted))
           (real-syms (cl-remove-if #'package-installed-p syms)))
      (cl-mapc #'package-install real-syms)
      (kill-buffer inst-buf))))


;;;;; Do a package refresh, if possible!

(defcustom package-sync-on-init t
  "Whether to try to refresh packages when starting up emacs."
  :type 'boolean
  :group 'package-connections)

;;; When we initialize, ensure we have all the packages from this file installed.
(when (and package-sync-on-init (internet-connected-p))
  (package-refresh-contents)
  (install-packages-from-file))

;;; When we die, ensure we note all the packages we used.
(add-hook 'kill-emacs-hook #'write-packages-to-file)
