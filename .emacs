;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014-2015

;;; IF YOU ARE HAVING CRASHES UPON OPENING A PARTICULAR FILE, TRY DELETING THAT
;;; FILE'S UNDO-TREE-HISTORY in ~/.emacs.d/undo-tree-history/!!!!!!!!!!!

;;; .....let's begin
(package-initialize)

;;; let's not hardcode everything like back in 9th grade
(defvar init-home-folder-dir (if load-file-name
                                 (file-name-directory
                                  (file-truename load-file-name))
                               (file-truename default-directory))
  "Location of this home directory.")

;;; CUSTOM VARS
;;; the below are custom variables that are "more custom" than defcustom; i.e.,
;;; they may differ for the same person on different systems (things like
;;; setting up sbcl, the location of the home folder, and other nonsense not
;;; related to actually using emacs). therefore i have placed them in a separate
;;; file which below is created for the user if it doesn't exist. This
;;; "custom-vars.el" file is gitignored so that it may vary easily. meaningful
;;; defaults are provided below with each variable.
(defvar warning-words-file nil
  "Path to file defining words to highlight specially. An example file would
contain:

todo
fixme
hack
broken
deprecated

Set in custom-vars.el, and used in init-scripts/interface.el.")
(defvar sbcl-special-command nil
  "Sometimes required because some versions of sbcl are difficult to wire up
correctly. Set in custom-vars.el")
(defvar do-ssh-agent-command-on-start t
  "Whether or not to run an ssh-agent command on starting up emacs.")
(defvar id-rsa-path (let ((id-rsa-path (concat (getenv "HOME") "/.ssh/id_rsa")))
                      (if (file-exists-p id-rsa-path) id-rsa-path nil))
  "Path to desired id_rsa file for ssh. Used in init-scripts/interface.el to
stop ssh from prompting you every time you run git.")
(defvar save-visited-files t
  "Whether or not to restore all files that were visited during the previous
session. Used later in this file.")
(defvar saved-files (concat init-home-folder-dir "saved-files")
  "File path to save visited-files. Used later in this file.")

;;; load custom values for these variables (this file is .gitignored)
(let ((custom-var-file
       (concat
        (file-name-directory (file-truename load-file-name))
        "custom-vars.el"))
      (msg-string
       "Make a custom-vars.el! Only if you want, though.
Check out your .emacs."))
  (unless (file-exists-p custom-var-file)
    (with-temp-buffer
      (insert (concat "(with-current-buffer \"*scratch*\"
  (insert \"" msg-string "\\n\"))"))
      (write-region nil nil custom-var-file)))
  (load-file custom-var-file))

(unless (file-exists-p init-home-folder-dir)
  (throw 'no-home-holder "no emacs home directory found (check your .emacs)!"))

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
(when save-visited-files
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
    ;; TODO: make this more error-resistant, somehow. having to send emacs a
    ;; sigterm because this function fails on quit is annoying.
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
   #'save-visiting-files-to-buffer))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(asm-comment-char 35)
 '(coffee-tab-width 2)
 '(fill-column 80)
 '(gud-key-prefix "")
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (xterm-color cloc package-build flycheck-package web-beautify w3m smartrep slime rainbow-mode rainbow-delimiters php-mode paredit multiple-cursors misc-cmds minimap markdown-mode magit literate-coffee-mode linum-relative less-css-mode js2-mode helm-swoop go-mode evil espuds ein company color-theme cider better-defaults auto-complete auctex 2048-game)))
 '(safe-local-variable-values
   (quote
    ((TeX-master . "proposal")
     (add-log-time-format lambda nil
                          (progn
                            (setq tz
                                  (getenv "TZ"))
                            (set-time-zone-rule "UTC")
                            (setq time
                                  (format-time-string "%a %b %e %H:%M:%S %Z %Y"
                                                      (current-time)))
                            (set-time-zone-rule tz)
                            time)))))
 '(yank-pop-change-selection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
