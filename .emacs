;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014-2015

;;; let's not hardcode everyhing like back in 9th grade
(defvar init-home-folder-dir "~/.emacs.d/")

;;; added up here cause a lot of packages depend on it being defined without
;;; defining it themselves
(require 'cl)
(require 'json)

;; starts emacs in server form so i can use emacsclient to add files
;; but only if server isn't already started
(require 'server)
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;; automate everythingggggggggg
(defun load-my-init-script (file-name)
  "Loads script located in init-scripts directory."
  (load-file (concat init-home-folder-dir "init-scripts/" file-name ".el")))

;;; load the packages i like
(load-my-init-script "packages")

;;; byte-compile everything: slow on first startup, but /significantly/ faster
;;; during normal usage
(byte-recompile-directory
 ;; needs expand-file-name for some reason i don't understand
 (expand-file-name init-home-folder-dir) 0)
(when (get-buffer "*Compile-Log*")
  ;; nobody wants to see that
  (delete-windows-on "*Compile-Log*"))

;;; load elisp
;;; should be /after/ byte-recompilation
(load-my-init-script "requires")
;;; functions used throughout
(load-my-init-script "functions")
;;; for compatibility between different operating environments
(load-my-init-script "compat")
;;; enforce my strong opinions on the default emacs ui
(load-my-init-script "interface")
;;; do some additional work to setup packages
(load-my-init-script "package-setup")
;;; load (programming) language-specific settings
(load-my-init-script "languages")
;;; cause what else is emacs for
(load-my-init-script "keybindings")

;;; TODO: document no-beautify and saved-files!

;;; save visited files
(defvar save-visited-files t)
(defvar saved-files (concat init-home-folder-dir "saved-files"))
(when save-visited-files
  ;; TODO: restore from death
  (with-current-buffer (find-file (expand-file-name saved-files))
    (goto-char (point-min))
    (loop while (not (eobp))
          with cur-line
          do (progn
               (setq cur-line
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
               (unless (string-equal cur-line (expand-file-name saved-files))
                 (find-file-noselect cur-line))
               (forward-line)))
    (kill-buffer))
  ;; save visiting files
  (defun save-visiting-files-to-buffer ()
    (interactive)
    (with-current-buffer (find-file (expand-file-name saved-files))
         (erase-buffer)
         (loop for buf in (buffer-list)
               do (unless (or
                           (not (buffer-file-name buf))
                           (string-equal (buffer-file-name buf)
                                         (expand-file-name saved-files)))
                    (insert (buffer-file-name buf))
                    (newline)))
         (save-buffer)
         (kill-buffer)))
  (add-hook
   'kill-emacs-hook
   'save-visiting-files-to-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(safe-local-variable-values
   (quote ((TeX-master . "proposal")
           (add-log-time-format
            lambda nil
            (progn
              (setq tz
                    (getenv "TZ"))
              (set-time-zone-rule "UTC")
              (setq time
                    (format-time-string "%a %b %e %H:%M:%S %Z %Y"
                                        (current-time)))
              (set-time-zone-rule tz) time)))))
 '(asm-comment-char 35)
 '(coffee-tab-width 2)
 '(fill-column 80)
 '(gud-key-prefix "")
 '(org-support-shift-select 'always)
 '(server-delete-tty t)
 '(yank-pop-change-selection t))
