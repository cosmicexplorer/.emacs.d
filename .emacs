;;; .....let's begin
(package-initialize)

;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014-2015

;;; IF YOU ARE HAVING CRASHES UPON OPENING A PARTICULAR FILE, TRY DELETING THAT
;;; FILE'S UNDO-TREE-HISTORY in ~/.emacs.d/undo-tree-history/!!!!!!!!!!!

;;; let's not hardcode everything like back in 9th grade
(defvar init-home-folder-dir (if load-file-name
                                 (file-name-directory
                                  (file-truename load-file-name))
                               (file-truename default-directory))
  "Location of this home directory.")

;;; CUSTOM VARS
;;; the below variables should be defcustoms, but i don't enjoy that setup
;;; because when you click "apply and save" on a "customize" prompt, it saves
;;; the choice directly to your .emacs file. after every such operation, i would
;;; have to then move the choice of defcustom from this .emacs file to a
;;; separate file if i wanted it to be gitignored. as a result, I am creating
;;; essentially my own more low-level version of defcustoms so that a single
;;; file can be modified without affecting version control. this also allows for
;;; storing personal information such as directory structure, or irc nicks,
;;; which is not really great for sharing on github. this "custom-vars.el" file
;;; is gitignored so that it may vary easily. meaningful defaults are provided
;;; below with each variable.
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
(defvar saved-files (file-truename (concat init-home-folder-dir "saved-files"))
  "File path to save visited-files. Used later in this file.")
(defvar save-eshell-history t
  "Whether or not to save eshell history to disk. Used in
init-scripts/interface.el.")
(defvar save-nonvisiting-files t
  "Whether or not to persist all buffers not visiting files to disk. Used in
init-scripts/interface.el.")
(defvar save-tramp-bufs t
  "Whether or not to revisit tramp buffers opened in a previous session. Used in
init-scripts/interface.el.")

;;; used in user customizations
(defmacro with-internet-connection (&rest body)
  "Perform BODY only if we can grab a url in a short period of time."
  `(progn
     (unless (featurep 'url-queue)
       (require 'url-queue))
     ;; url-queue-retrieve used because of built-in timeout
     (url-queue-retrieve
      ;; arbitrary url, chosen because github's uptime is ridiculous (remember
      ;; when all of china ddos'd them? incredible)
      "http://github.com"
      (lambda (status)
        (let ((err (plist-get status :error)))
          (unless err ,@body)))
      nil t t)))

(defvar after-load-init-hook nil
  "Hook to run whatever after loading packages n functions n whatever.")

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
  (insert \"" msg-string "\")
  (newline))"))
      (write-region nil nil custom-var-file)))
  (load-file custom-var-file))

;;; get rid of annoying erc stuff everywhere
(switch-to-buffer "*scratch*")
(delete-other-windows)

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
;;; should be /before/ everything in case they rely on it
(load-my-init-script "utilities")
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

;;; let's do it
(run-hooks 'after-load-init-hook)

;;; TODO: document no-beautify and saved-files!

;;; save visited files
(when save-visited-files
  (reread-visited-files-from-disk)
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
 '(dabbrev-case-replace nil)
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-fill-mode t)
 '(erc-highlight-nicknames-mode t)
 '(erc-irccontrols-mode t)
 '(erc-join-buffer (quote bury))
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode nil)
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-stamp-mode t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line nil)
 '(fill-column 80)
 '(gud-key-prefix "")
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (csharp-mode xterm-color cloc package-build flycheck-package web-beautify w3m smartrep slime rainbow-mode rainbow-delimiters php-mode paredit multiple-cursors misc-cmds minimap markdown-mode magit literate-coffee-mode linum-relative less-css-mode js2-mode helm-swoop go-mode evil espuds ein company color-theme cider better-defaults auto-complete auctex 2048-game)))
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
 '(smart-tab-using-hippie-expand t)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(yank-pop-change-selection t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; if everything loaded correctly, clear that last message
(message "")
