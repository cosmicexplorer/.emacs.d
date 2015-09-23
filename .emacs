;;; -*lexical-binding: t -*-

;;; .....let's begin
(package-initialize)

(defgroup my-customizations nil
  "all `defcustom' forms in my own init scripts")

;;; add wherever emacs was invoked from to path
;;; done at top so we know we're not changing any directories
(defvar emacs-start-command (car command-line-args)
  "Command used to start emacs.")

;;; emacs config, aka the root node of a massively unbalanced configuration tree
;;; by Danny McClanahan, <danieldmcclanahan@gmail.com>, 2014-2015

;;; IF YOU ARE HAVING CRASHES UPON OPENING A PARTICULAR FILE, TRY DELETING THAT
;;; FILE'S UNDO-TREE-HISTORY in ~/.emacs.d/undo-tree-history/!!!!!!!!!!!

;;; let's not hardcode everything like back in 9th grade
(defvar init-home-folder-dir (file-truename user-emacs-directory)
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
;;; below with each variable. it should be noted that this file is especially
;;; useful to store variables such as org-agenda-files.
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
(defvar ssh-pass nil
  "Default password to use for ssh-agent. Keep this secure.")
(defvar save-visited-files t
  "Whether or not to restore all files that were visited during the previous
session. Used later in this file.")
(defvar saved-files (file-truename (concat init-home-folder-dir "saved-files"))
  "File path to save visited-files. Used later in this file.")
(defvar save-eshell-history t
  "Whether or not to save eshell history to disk. Used in
init-scripts/interface.el.")
(defvar save-shell-history t
  "Whether or not to save shell history to disk. Used in
init-scripts/interface.el.")
(defvar save-nonvisiting-files t
  "Whether or not to persist all buffers not visiting files to disk. Used in
init-scripts/interface.el.")
(defvar save-tramp-bufs t
  "Whether or not to revisit tramp buffers opened in a previous session. Used in
init-scripts/interface.el.")
(defvar erc-nick nil
  "Nick to use for erc.")
(defvar erc-port 6667
  "Default port to use for erc.")
(defvar erc-server-pass-alist nil
  "Alist of passwords and ports for servers to connect with erc.")
(defvar submodule-makes-to-ignore nil
  "List of submodule makes to ignore compilation for.")
(defvar dont-ask-about-git nil
  "If git not installed, don't worry about it.")

;;; used in user customizations
(defmacro with-internet-connection (&rest body)
  "Perform BODY only if we can grab a url in a short period of time."
  `(progn
     (unless (featurep 'url-queue)
       (require 'url-queue))
     ;; url-queue-retrieve used because of built-in timeout
     (url-queue-retrieve
      ;; arbitrary url, chosen because github's uptime is ridiculous (remember
      ;; when all of china ddos'd them? incredible) and they probably aren't
      ;; tracking my browsing info
      "https://github.com"
      (lambda (status)
        (let ((err (plist-get status :error)))
          (unless err ,@body)))
      nil t t)))

(defvar after-load-init-hook nil
  "Hook to run whatever after loading packages n functions n whatever.")
(defvar use-omnisharp t "C#!!!!!!!")

;;; load custom values for these variables (this file is .gitignored)
(let ((custom-var-file
       (concat
        (if load-file-name
            (file-name-directory (file-truename load-file-name))
          default-directory)
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
(when (fboundp 'server-running-p)
  (if (not (eq (server-running-p) t))
      (server-start)
    (setq do-ssh-agent-command-on-start nil)
    (setq save-visited-files nil)))

(defun load-my-init-script (file-name)
  "Loads script located in init-scripts directory."
  (load-file (concat init-home-folder-dir "init-scripts/" file-name ".el")))

(add-hook 'after-load-init-hook
          (lambda () (load-my-init-script "visuals")))

;;; load the packages i like
(load-my-init-script "packages")

;;; load elisp
;;; should be /after/ byte-recompilation
(load-my-init-script "requires")

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

;;; byte-compile everything: slow on first startup, but /significantly/ faster
;;; during normal usage
(async-start
 (lambda ()
   (byte-recompile-directory user-emacs-directory 0))
 'ignore)

;;; sometimes fails on 'require call
(load-file (expand-file-name
            (concat user-emacs-directory "lisp/smart-compile.el")))

;;; let's do it
(run-hooks 'after-load-init-hook)

;;; if everything loaded correctly, clear that last message
(message "")
(setq init-loaded-fully t)
(put 'upcase-region 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-engine (quote luatex))
 '(TeX-newline-function (quote reindent-then-newline-and-indent))
 '(asm-comment-char 35)
 '(cloc-use-3rd-gen nil)
 '(coffee-args-compile (quote ("-c" "-b" "--no-header")))
 '(coffee-tab-width 2)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode)))
 '(compilation-scroll-output t)
 '(compile-command "make")
 '(dabbrev-case-replace nil)
 '(dired-auto-revert-buffer t)
 '(dired-clean-up-buffers-too nil)
 '(dired-guess-shell-alist-user
   (quote
    (("\\.tar\\.xz\\'" "sudo pacman -U")
     ("\\.exe\\'" "wine")
     ("\\.\\(?:jpe?g\\|png\\|gif\\|bmp\\)\\'" "gimp")
     ("\\.\\(?:pdf\\|ps\\)\\'" "evince")
     ("\\.coffee\\'" "coffee")
     ("\\.pl\\'" "perl")
     ("\\.jar\\'" "java -jar")
     ("^[^\\.]+\\'"
      (concat user-emacs-directory "exec-file.sh"))
     ("\\.bash\\'" "bash")
     ("\\.zsh\\'" "zsh")
     ("\\.sh\\'" "sh")
     ("\\.py\\'" "python" "python2" "python3")
     ("\\..+x\\'" "libreoffice"))))
 '(dired-no-confirm t)
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote always))
 '(ecb-options-version "2.40")
 '(ecb-tip-of-the-day nil)
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
 '(erc-prompt (quote get-erc-prompt))
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-stamp-mode t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(fill-column 80)
 '(git-gutter:update-interval 1)
 '(global-rainbow-delimiters-mode t)
 '(grep-command (concat "\"" init-home-folder-dir "switch-grep.sh\" "))
 '(grep-highlight-matches (quote auto))
 '(grep-use-null-device nil)
 '(gud-key-prefix "")
 '(haskell-check-command "hlint")
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(hippie-expand-try-functions-list
   (quote
    (try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-lisp-symbol-partially try-complete-lisp-symbol)))
 '(initial-buffer-choice t)
 '(kill-buffer-trash-alist
   (quote
    ((markdown-mode lambda
                    (f)
                    (concat
                     (file-name-sans-extension f)
                     ".html"))
     (coffee-mode lambda
                  (f)
                  (concat
                   (file-name-sans-extension f)
                   ".js"))
     (java-mode lambda
                (f)
                (concat
                 (file-name-sans-extension f)
                 ".class"))
     (csharp-mode lambda
                  (f)
                  (file-name-sans-extension f))
     (c++-mode lambda
               (f)
               (file-name-sans-extension f))
     (c-mode lambda
             (f)
             (file-name-sans-extension f)))))
 '(linum-relative-plusp-offset 1)
 '(lua-indent-level 2)
 '(magit-no-confirm
   (quote
    (kill-process stage-all-changes unstage-all-changes)))
 '(magit-push-always-verify nil)
 '(magit-revert-buffers 5)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files nil)
 '(org-catch-invisible-edits (quote smart))
 '(org-confirm-babel-evaluate nil)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-from-is-user-regexp nil)
 '(org-src-fontify-natively t)
 '(org-support-shift-select (quote always))
 '(package-selected-packages
   (quote
    (sml-mode skewer-mode magit org csv-mode git-gutter matlab-mode speech-tagger lua-mode ensime scala-mode2 company-ghc company-ghci ghc epresent helm-gtags ggtags xterm-color web-beautify w3m smartrep rainbow-mode rainbow-delimiters php-mode paredit package-build omnisharp multiple-cursors misc-cmds minimap markdown-mode literate-coffee-mode linum-relative less-css-mode js2-mode helm-swoop go-mode flycheck-package evil espuds ein company color-theme cloc cider better-defaults auctex 2048-game)))
 '(rainbow-ansi-colors t)
 '(rainbow-html-colors t)
 '(rainbow-latex-colors t)
 '(rainbow-r-colors t)
 '(rainbow-x-colors t)
 '(safe-local-variable-values
   (quote
    ((major-mode . sh-mode)
     (TeX-master . "proposal")
     (add-log-time-format lambda nil
                          (progn
                            (setq tz
                                  (getenv "TZ"))
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
 '(italic ((t (:foreground "magenta" :slant italic))))
 '(underline ((t (:foreground "yellow" :underline t)))))
