;;; modifications to default ui

(require 'helm)

;;; prompts for (yes/no) -> (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;;; use my shell aliases in shell commands run from emacs
(unless (eq system-type 'windows-nt)
  (setq shell-command-switch "-ic"))

;; stop the intro to emacs buffer
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;;; c-h a -> apropos
(define-key help-map "a" 'apropos)      ; get useful help for once

;;; remove toolbars
(menu-bar-mode 0)                       ;  remove menu bar for a line of space
(tool-bar-mode 0)                       ; and tool bar for graphical

;;; configure scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(scroll-bar-mode 0)
(setq scroll-preserve-screen-position t)

;;; set font size and type
(set-face-attribute 'default nil :height 100)
(when (member "Telegrama" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Telegrama 10"))
  (set-face-attribute 'default t :font "Telegrama 10")
  (set-frame-font "Telegrama 10"))

;;; the mark is stupid as a ui concept even if it's great in scripts
(transient-mark-mode 0)
(setq shift-select-mode t)

;;; i don't want to have to guess which version of my file is the right one
(global-auto-revert-mode)

;; Remove trailing whitespace from a line
(setq-default nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'nuke-whitespace-except-this-line)

;;; only show whitespace sometimes
(add-hook 'after-change-major-mode-hook
          '(lambda ()
             (unless (or (eq major-mode 'w3m-mode)
                         (eq major-mode 'eshell-mode)
                         (eq major-mode 'ibuffer-mode)
                         (eq major-mode 'undo-tree-visualizer-mode))
               (setq show-trailing-whitespace t))))

;;; have normal delete/selection (type over selected text to delete)
(delete-selection-mode 1)

;; do backups well and put them into a separate folder
(setq backup-directory-alist
      `(("." . ,(concat init-home-folder-dir "autosaved-files"))))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; do the same thing for undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,(concat init-home-folder-dir "undo-tree-history"))))
(setq undo-tree-visualizer-timestamps t)

;; load w3m web browser
(when (executable-find "w3m")
  (require 'w3m)
  (setq w3m-use-cookies t)
  (setq w3m-coding-system 'utf-8
        w3m-file-coding-system 'utf-8
        w3m-file-name-coding-system 'utf-8
        w3m-input-coding-system 'utf-8
        w3m-output-coding-system 'utf-8
        w3m-terminal-coding-system 'utf-8)
  (add-hook 'w3m-mode-hook
            '(lambda ()
               (w3m-turnoff-inline-images)
               (w3m-toggle-inline-images))))


;;; org-mode
(setq-default org-startup-folded "showeverything")
(add-hook 'org-mode-hook
          (lambda ()
            (highlight-80+-mode -1)
            (auto-fill-mode -1)))

;;; see docs for funcs n stuff
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; show line numbers
(global-linum-mode 1)
;; make them relative
(setq linum-format 'fix-linum-relative)
;;; make it weird
(defvar linum-relative-symbols
  ">"                                   ; i like this but it's super distracting
  ;; "+-~>"
  ;; "->X☢☣☠⚠☤⚕⚚†☯⚖☮⚘⚔☭⚒⚓⚛⚜⚡⚶☥✠✙✞✟✧⋆★☆✪✫✬✭✮✯✰⚝✡☫☬☸✵❂☘♡♥❤⚘❀❃❁✼☀✌♫♪☃❄❅❆☕☂❦✈♕♛♖♜☁☾"
  "A vector of strings to represent the marker on the current line. Used in
`get-linum-relative-symbol'.")
(make-variable-buffer-local 'linum-relative-current-symbol)
(add-hook 'after-change-major-mode-hook 'get-linum-relative-symbol)

;;; RAINBOW
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'text-mode-hook #'rainbow-mode)

;;; get and update the current number of lines within the buffer
(defvar my-mode-line-buffer-line-count nil)
(make-variable-buffer-local 'my-mode-line-buffer-line-count)
(defvar integer-buffer-line-count nil)
(make-variable-buffer-local 'integer-buffer-line-count)
(setq-default mode-line-format
              '("  " mode-line-modified
                (list 'line-number-mode "  ")
                (:eval (when line-number-mode
                         (let ((str "L%l"))
                           (when (and (not (buffer-modified-p))
                                      my-mode-line-buffer-line-count)
                             (setq str (concat str "/"
                                               my-mode-line-buffer-line-count)))
                           str)))
                "  %p"
                (list 'column-number-mode "      C%c")
                "  " mode-line-buffer-identification
                "  " mode-line-modes))
(defun output-lines-in-buffer ()
  (setq integer-buffer-line-count (count-lines (point-min) (point-max)))
  (setq my-mode-line-buffer-line-count (int-to-string
                                        integer-buffer-line-count)))

;;; convenience hooks
(add-hook 'find-file-hook 'output-lines-in-buffer)
(add-hook 'after-save-hook 'output-lines-in-buffer)
(add-hook 'after-revert-hook 'output-lines-in-buffer)
(add-hook 'dired-after-readin-hook 'output-lines-in-buffer)
;;; turn off linum mode for pdf/ps whatever files because it takes like a
;;; million years to scroll through it otherwise
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (when (eq major-mode 'doc-view-mode)
              (linum-mode 0))))


;;; misc
(load-display-time)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq auto-save-interval 600)     ; half as often as default
(setq gc-cons-threshold 2000000)
(setq lpr-switches '("-Pps"))
(setq text-mode-hook 'turn-on-auto-fill)
(setq default-truncate-lines nil)
(setq visible-bell t)
(setq require-final-newline t)
(setq version-control t)
(setq abbrev-all-caps t)
(put 'eval-expression 'disabled nil)
(autoload 'html-helper-mode "html-helper-mode" "HTML rules!" t)
(autoload 'ispell-word "ispell" "Check spelling of word at or before point" t)
(autoload 'ispell-complete-word "ispell" "Complete word at or before point" t)
(autoload 'ispell-region "ispell" "Check spelling of region" t)
(autoload 'ispell-buffer "ispell" "Check spelling of buffer" t)
;;; i'm not sure what these do
(setq trim-versions-without-asking t)
(setq mode-line-inverse-video t)


;;; setup syntax highlighting for keywords i care about
(defvar warning-words
  '("todo"
    "fixme"
    "fix"
    "note"
    "nb"
    "n.b."
    "deprecated"
    "xxx"
    "hack"
    "iffy"
    "changed"
    "optimization"
    "broken")
  "Used if no warning_words file exists.")

(defun make-string-init-caps (str)
  "Takes string with arbitrary capitalization and forces it to Initial Caps
case."
  (let ((downstr (downcase str)))
    (loop for i from 0 upto (1- (length downstr))
          do (if (/= i 0)
                 (when (and (not (wordp (aref downstr (1- i))))
                            (wordp (aref downstr i)))
                   (setf (aref downstr i)
                         (upcase (aref downstr i))))
               (setf (aref downstr i)
                     (upcase (aref downstr i)))))
    downstr))

(defun read-words-from-list-with-caps (list)
  "Read words from list, transformed into ALLCAPS, lowercase, and Init Caps."
  (let ((final-word-list nil))
    (loop for line in list
          do (unless (string= "" line)
               (add-to-list 'final-word-list (downcase line))
               (add-to-list 'final-word-list (upcase line))
               (add-to-list 'final-word-list (make-string-init-caps line))))
    final-word-list))

(defun read-words-from-file-as-list-with-caps (filename)
  "Reads words from file, delimited by newlines, as a list. Reads in ALLCAPS,
lowercase, and Initial Caps versions."
  (if (or (not filename) (not (file-exists-p filename)))
      (throw 'no-warning-words-file t)
    (read-words-from-list-with-caps
     (split-string
      (with-temp-buffer
        (insert-file-contents filename)
        (buffer-string))
      "\n"))))

(defvar warning-words-list
  (mapcar #'regexp-quote
          (if (and warning-words-file (file-exists-p warning-words-file))
              (read-words-from-file-as-list-with-caps warning-words-file)
            (read-words-from-list-with-caps warning-words))))

(defvar warning-highlights-regex
  (concat
   "\\(^\\|[^[:word:]]\\)"
   "\\("
   (regexp-opt warning-words-list)
   "\\)"
   "\\($\\|[^[:word:]]\\)"))

(defvar warning-highlights-keywords
  ;; the 'words option to regexp-opt surrounds the output with \<...\>, which
  ;; doesn't work with "warning" words that have non-word characters in them
  ;; (for example, n.b., or words with spaces). this is a workaround.
  `((,warning-highlights-regex
     ;; highlights the second subexpression: the warning-word expression
     2 font-lock-warning-face t))
  "Keywords to apply extra highlights to.")

(defun warning-highlights-turn-on ()
  "Turn on warning-highlights-mode."
  (font-lock-add-keywords nil warning-highlights-keywords t))

(defun warning-highlights-turn-off ()
  "Turn off warning-highlights-mode."
  (font-lock-remove-keywords nil `(,@warning-highlights-keywords)))

(define-minor-mode warning-highlights-mode
  "Highlight words of warning."
  :lighter " !!"
  (progn
    (if warning-highlights-mode
        (warning-highlights-turn-on)
      (warning-highlights-turn-off))
    (font-lock-mode 1)))
;;; add it to programming modes!
(add-hook 'prog-mode-hook #'warning-highlights-mode)

(defun find-warning-words ()
  (interactive)
  (helm-multi-swoop-all
   warning-highlights-regex))

(defvar warning-words-grep-regex
  (reduce (lambda (a b) (concat a "|" b)) warning-words-list))

(defun find-warnings-in-dir (dir)
  (interactive "Mdirectory: ")
  (when (or current-prefix-arg (string-equal dir "")) (setq dir "."))
  (grep (concat init-home-folder-dir "switch-grep.sh" " -E "
                "\"(^|[^a-zA-Z])(" warning-words-grep-regex
                ")([^a-zA-Z]|$)\" \"" dir "\"")))


;;; wrap lines in org-mode, mostly, but also other places
(visual-line-mode)


;;; defadvice used here because even recent emacs (the default version for
;;; ubuntu, for example) don't support advice-add yet (which is /so/ much
;;; better). oh well.
;;; advice used below because for some reason when using add-hook
;;; generate-new-buffer-name doesn't respect its "ignore" argument
;;; mark eshell buffers with their current directory
(defadvice eshell (after eshell-set-pwd-name)
  (rename-buffer
   (generate-new-buffer-name (concat "eshell: " default-directory))))
(ad-activate 'eshell)

;;; resets name on every input send to every command, not just cd. the overhead
;;; is negligible. the bigger issue is that if "exit" is used to quit eshell
;;; instead of kill-buffer, the buffer switched to after eshell exits is renamed
;;; as described below. this is fixed by the "when" statement.
(defadvice eshell-send-input (after eshell-set-pwd-name-on-cd)
  (when (eq major-mode 'eshell-mode)
    (rename-buffer
     (generate-new-buffer-name
      (concat "eshell: " default-directory)
      (buffer-name)))))
(ad-activate 'eshell-send-input)

;;; output eshell buffers to file
(when save-eshell-history
  (defvar eshell-user-output-file (concat init-home-folder-dir "eshell-output")
    "File containing all eshell I/O from all eshell buffers.")
  (add-hook 'eshell-pre-command-hook #'eshell-send-input-to-history)
  (add-hook 'eshell-post-command-hook #'eshell-send-output-to-history))

;;; now let's do the same for shell-mode!
(defun sh-power-ranger ()
  (interactive)
  (shell (generate-new-buffer (concat "shell: " default-directory))))

(defadvice comint-send-input (after shell-set-pwd-name-on-cd)
  (when (eq major-mode 'shell-mode)
    (rename-buffer
     (generate-new-buffer-name
      (concat "shell: " default-directory)
      (buffer-name)))))
(ad-activate 'comint-send-input)

;;; helm-swoop sends me angry error messages if i don't do this
;;; this is a super hack but it works reliably, and since recentering is more of
;;; an aesthetic action than anything else (i.e. won't affect correct execution
;;; of lisp code), i don't think it's too harmful
;; (defadvice recenter (around ignore-recenter-errors)
;;   (ignore-errors ad-do-it))
;; (ad-activate 'recenter)


;;; save and reset window configuration to ring
(defvar window-configuration-ring nil
  "List of saved window configurations; only stays present within a session.")


;;; do ssh-agent stuff
(defun setup-ssh-agent ()
  (interactive)
  (when (eq system-type 'gnu/linux)
    (if id-rsa-path
        (when (and (executable-find "ssh-agent")
                   (executable-find "ssh-add"))
          (let ((command-results (shell-command-to-string "ssh-agent -s"))
                (ssh-auth-sock-regex "SSH_AUTH_SOCK=\\([^;]+\\);")
                (ssh-agent-pid-regex "SSH_AGENT_PID=\\([^;]+\\);"))
            ;; setup required environment vars
            (if (string-match ssh-auth-sock-regex command-results)
                (setenv "SSH_AUTH_SOCK" (match-string 1 command-results))
              (throw 'ssh-agent-err "SOCK output can't be parsed!"))
            (if (string-match ssh-agent-pid-regex command-results)
                (setenv "SSH_AGENT_PID" (match-string 1 command-results))
              (throw 'ssh-agent-err "PID output can't be parsed!"))
            ;; make it ask for a password using our script
            (let ((prev-display (getenv "DISPLAY"))
                  (prev-ssh-askpass (getenv "SSH_ASKPASS")))
              (setenv "DISPLAY" ":0")
              (setenv "SSH_ASKPASS" (local-file-path "read-ssh-pass.sh"))
              (with-temp-buffer
                (loop with ssh-add-success = nil
                      with ssh-did-fail = nil
                      while (not ssh-add-success)
                      do (progn
                           (insert (read-passwd
                                    (if ssh-did-fail
                                        "incorrect password. ssh password: "
                                      "ssh password: ")))
                           (if (zerop (call-process-region
                                       (point-min) (point-max)
                                       "ssh-add" t nil nil id-rsa-path))
                               (setq ssh-add-success t)
                             (erase-buffer)
                             (setq ssh-did-fail t)))))
              (setenv "DISPLAY" prev-display)
              (setenv "SSH_ASKPASS" prev-ssh-askpass))
            (add-hook 'kill-emacs-hook
                      (lambda ()
                        (call-process "kill" nil nil nil
                                      (getenv "SSH_AGENT_PID"))))))
      (with-current-buffer "*scratch*"
        (insert "Set up an id-rsa-path! Only if you want to, though.
Check out your .emacs.\n")))))

(when do-ssh-agent-command-on-start
  (setup-ssh-agent))


;;; TODO: persist buffers not visiting files to disk as well because apparently
;;; sublime does this by default and we can't let sublime beat us; let's also do
;;; the same for tramp buffers (this requires logging in; "luckily" emacs is
;;; synchronous so that should still work. may get annoying for people who tramp
;;; around a lot. let's make that an option)


;;; erc setup
;; (add-hook 'erc-mode-hook 'erc-nicklist)
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)
(defun destroy-all-erc-stuff ()
  (interactive)
  (loop for buf in (buffer-list)
        do (with-current-buffer buf
             (when (eq major-mode 'erc-mode)
               (kill-this-buffer)))))
(defun message-erc-modded-chans ()
  (interactive)
  (message "%s" erc-modified-channels-object))
(defun message-erc-modded-chans-when-erc-mode ()
  (when (eq major-mode 'erc-mode)
    (message-erc-modded-chans)))
;;; update me with irc activity, but only if i'm in an erc buffer
(defadvice erc-server-filter-function (after update-erc-status-on-erc-bufs)
  (message-erc-modded-chans-when-erc-mode))
(ad-activate 'erc-server-filter-function)
;;; update me with activity when i switch to an erc buffer
(defadvice switch-to-buffer (after update-erc-status-on-switch-to-erc-buf)
  (message-erc-modded-chans-when-erc-mode))
(ad-activate 'switch-to-buffer)
(defadvice windmove-do-window-select (after update-erc-status-on-windmove)
  (message-erc-modded-chans-when-erc-mode))
(ad-activate 'windmove-do-window-select)
;;; don't open up windows randomly!!!!
(defadvice erc-cmd-JOIN (around erc-bufs-do-not-explode)
  (let ((prev-win-conf (current-window-configuration)))
    ad-do-it
    (set-window-configuration prev-win-conf)))
(ad-activate 'erc-cmd-JOIN)

;;; save visited files to buffer
(when save-visited-files
  (add-hook 'after-load-init-hook #'reread-visited-files-from-disk)
  (add-hook 'kill-emacs-hook #'save-visiting-files-to-buffer))

;;; see what i just killed
(add-hook 'kill-buffer-hook #'kill-message-buffer-id)

;;; don't like seeing wraparound
(add-hook 'text-mode-hook #'visual-line-mode)

;;; i like being able to search for w3m buffers
;;; TODO: doesn't work, let's fix
;; (add-hook 'w3m-select-buffer-hook #'w3m-rename-buf)

;;; compilation-mode
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; so i can manually git commit from within eshell on windows
(setenv "EDITOR" "emacsclient")

;;; setup submodules and make them

(add-hook 'after-load-init-hook
          (lambda ()
            (actual-setup-submodules)
            (actual-make-all-submodules)))
;; (add-hook 'after-load-init-hook #'update-all-packages)

;;; shell-mode echoes commands lol
(add-hook 'comint-mode-hook (lambda () (setq comint-process-echoes t)))
(add-hook 'shell-mode-hook
          (lambda () (set-process-query-on-exit-flag
                      (get-buffer-process (current-buffer))
                      nil)))
