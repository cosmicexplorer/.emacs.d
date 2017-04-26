;; -*- lexical-binding: t -*-

;;; just a grab bag of stuff to change lol
;;; in general, functions go in `functions.el', and `interface.el' calls them in
;;; some way, shape, or form

(require 'helm)

;;; prompts for (yes/no) -> (y/n)
(fset 'yes-or-no-p 'y-or-n-p)

;;; use my shell aliases in shell commands run from emacs
(unless (eq system-type 'windows-nt)
  (setq shell-command-switch "-ic"))

;; stop the intro to emacs buffer
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)

;;; the mark is stupid as a ui concept even if it's great in scripts
(transient-mark-mode 1)
(setq shift-select-mode t)

;;; i don't want to have to guess which version of my file is the right one
(global-auto-revert-mode)

;; Remove trailing whitespace from a line
(setq-default nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'nuke-whitespace-except-this-line)

;;; only show whitespace sometimes
(defvar no-show-whitespace-modes
  '(w3m-mode special-mode
    eww-mode eshell-mode ibuffer-mode undo-tree-visualizer-mode
    magit-mode magit-status-mode magit-log-mode))
(defun set-correct-trailing-whitespace ()
  (setq show-trailing-whitespace
        (not (apply #'derived-mode-p no-show-whitespace-modes))))
(defun set-correct-trailing-whitespace-all-buffers ()
  (loop for buf in (buffer-list)
        do (with-current-buffer buf (set-correct-trailing-whitespace))))
(add-hook 'buffer-list-update-hook #'set-correct-trailing-whitespace-all-buffers)
(add-hook 'after-change-major-mode-hook #'set-correct-trailing-whitespace)

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
  ">"
  ;; i like this but it's super distracting
  ;; "+-~>"
  ;; "->X☢☣☠⚠☤⚕⚚†☯⚖☮⚘⚔☭⚒⚓⚛⚜⚡⚶☥✠✙✞✟✧⋆★☆✪✫✬✭✮✯✰⚝✡☫☬☸✵❂☘♡♥❤⚘❀❃❁✼☀✌♫♪☃❄❅❆☕☂❦✈♕♛♖♜☁☾"
  "A vector of strings to represent the marker on the current line. Used in
`get-linum-relative-symbol'.")
;; (make-variable-buffer-local 'linum-relative-current-symbol)
;; (add-hook 'after-change-major-mode-hook 'get-linum-relative-symbol)

;;; RAINBOW
(add-hook 'text-mode-hook #'rainbow-mode)
(add-hook 'prog-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)

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
            (when (derived-mode-p 'doc-view-mode)
              (linum-mode 0))))
(add-hook 'help-mode-hook #'turn-off-linum)
(add-hook 'Info-mode-hook #'turn-off-linum)
(add-hook 'Man-mode-hook #'turn-off-linum)
(add-hook 'org-mode-hook #'turn-off-linum)
(defvar image-mode-hook nil)
(add-hook 'image-mode-hook #'turn-off-linum)

(defadvice dired-do-shell-command (after refresh activate)
  (revert-buffer))


;;; misc
(add-hook 'after-load-init-hook #'load-display-time)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq auto-save-interval 600)     ; half as often as default
(setq gc-cons-threshold 2000000)
(setq lpr-switches '("-Pps"))
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

(defun only-upcase-first-char (str)
  (let* ((res (downcase str))
         (init (aref res 0)))
    (setf (aref res 0) (upcase init))
    res))

(defun read-words-from-list-with-caps (list)
  "Read words from list, transformed into ALLCAPS, lowercase, and Init Caps."
  (let (final-word-list)
    (loop for line in list
          do (unless (string= "" line)
               (add-to-list 'final-word-list (downcase line))
               (add-to-list 'final-word-list (upcase line))
               (add-to-list 'final-word-list (upcase-initials line))
               (add-to-list 'final-word-list (only-upcase-first-char line))))
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
  (if (and warning-words-file (file-exists-p warning-words-file))
      (read-words-from-file-as-list-with-caps warning-words-file)
    (read-words-from-list-with-caps warning-words)))

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
  (if (not warning-highlights-mode)
      (warning-highlights-turn-off)
    (warning-highlights-turn-on)
    (font-lock-mode 1)))

(defun warning-highlights-mode-activate ()
  (warning-highlights-mode 1))

;;; add it to programming modes!
(add-hook 'prog-mode-hook #'warning-highlights-mode-activate)

(defun find-warning-words (pfx)
  (interactive "P")
  (if pfx (helm-multi-swoop-all warning-highlights-regex)
    (helm-swoop :$query warning-highlights-regex)))

(defvar warning-words-grep-regex
  (reduce (lambda (a b) (concat a "|" b)) warning-words-list))

(defun find-warnings-in-dir (dir)
  (interactive "Mdirectory: ")
  (when (or current-prefix-arg (string-equal dir "")) (setq dir "."))
  (grep (concat init-home-folder-dir "switch-grep.sh" " -E "
                "\"(^|[^a-zA-Z])(" warning-words-grep-regex
                ")([^a-zA-Z]|$)\" \"" dir "\"")))



(defvar mode-fun-regex "\\-mode\\'"
  "Regex at the end of all modes.")

(defun rename-shell-buffer ()
  (rename-buffer
   (format "%s: %s"
           (replace-regexp-in-string
            mode-fun-regex ""
            (symbol-name major-mode))
           default-directory)
   t))

;;; output eshell buffers to file
(when save-eshell-history
  (defvar eshell-user-output-file (concat init-home-folder-dir "eshell-output")
    "File containing all eshell I/O from all eshell buffers.")
  (add-hook 'eshell-pre-command-hook #'eshell-send-input-to-history)
  (add-hook 'eshell-post-command-hook #'eshell-send-output-to-history))

(defun shell-record-history-filters ()
  (add-hook 'comint-input-filter-functions
            #'shell-send-input-to-history nil t)
  (add-hook 'comint-output-filter-functions
            #'shell-send-output-to-history nil t))

(when save-shell-history
  (defvar shell-user-output-file (concat init-home-folder-dir "shell-output"))
  (add-hook 'shell-mode-hook #'shell-record-history-filters))

;;; TODO: would be nice if we could split these cases off into an alist -- this
;;; seems to be a lot easier to follow for now
;;; same for info and help
(defun help-rename-buffer ()
  (when-let ((name
              (pcase help-xref-stack-item
                (`(describe-bindings nil ,buf)
                 (let ((buf-mode (with-current-buffer buf major-mode)))
                   (format "help(describe-bindings): %s<%S>"
                           (buffer-name buf) buf-mode)))
                ;; multiple types of help use this stack structure
                ((and `(,_ ,item . ,_)
                      ;; set `name' and `desc' for each case and use in format
                      ;; string below
                      (or (and (guard (and (symbolp item)
                                           (fboundp item)))
                               (let desc "function")
                               (let name item))
                          (and (guard (and (symbolp item)
                                           (boundp item)))
                               (let desc "variable")
                               (let name item))
                          (and (let (cl-struct package-desc (name name))
                                 item)
                               (let desc "elisp-package"))
                          (let desc "something")))
                 (format "help(describe-%s): %s" desc name))
                (`(,x)
                 (format "help(%s)" x)))))
    (rename-buffer name t)))

(defun info-rename-buffer ()
  (when-let ((file Info-current-file)
             (node Info-current-node))
    (rename-buffer
     (format
      "Info: %s->%s"
      (file-name-nondirectory Info-current-file)
      Info-current-node)
     t)))

(defun cider-doc-rename-buf ()
  (rename-buffer
   (format "cider-doc: %s (%s)"
           cider-docview-symbol
           (let ((info (cider-var-info cider-docview-symbol)))
             (reduce
              (lambda (a b) (if (and a b) (concat a ":" b) a))
              (mapcar
               (lambda (str-or-list)
                 (if (listp str-or-list)
                     (when (nrepl-dict-get info (car str-or-list))
                       (car str-or-list))
                   (nrepl-dict-get info str-or-list)))
               (list '("macro") '("special-form") "class" "member"
                     "super" "interfaces" "forms-str"))
              :initial-value "")))
   t))

;;; TODO: this for eww!
(defun eww-get-buffer-name (&optional my-mode)
  (setq prev-special-buffer (cons "*eww*" (current-buffer)))
  (format "%s: %s (%s)"
          "eww" (plist-get eww-data :title)
          (cdr (memq 'href (car (second (plist-get eww-data :dom)))))))

(defadvice run-python (around no-stinkin-errors activate)
  (with-current-buffer (process-buffer ad-do-it)
    (rename-buffer "*Python*")))

(defadvice cider-doc-lookup (after reread-name activate)
  (cider-doc-rename-buf))

(defvar prev-help-buf nil)

(defadvice help-setup-xref (before make-prev-buf activate)
  (setq prev-help-buf (get-buffer-create "~Help~")))

(defadvice help-buffer (around use-prev activate)
  (setq ad-return-value prev-help-buf))

(defun set-help-prev-buf ()
  (push (current-buffer) prev-help-bufs))

(add-hook 'help-mode-hook #'help-rename-buffer)

(defun make-new-help (help-fn)
  (lambda ()
    (interactive)
    (call-interactively help-fn)
    (if (buffer-live-p prev-help-buf)
        (switch-to-buffer prev-help-buf)
      (error "help kbd failed: buffer '%s' is dead" prev-help-buf))))

(defvar info-prev-file nil)
(defvar info-prev-node nil)

(defun info-make-backup ()
  (setq info-prev-file Info-current-file
        info-prev-node Info-current-node))
(defun info-copy-from-backup ()
  (when-let ((file info-prev-file)
             (node info-prev-node))
    (save-window-excursion
      (info (format "(%s)%s" file node)))
    (setq info-prev-file nil
          info-prev-node nil)))

(defadvice Info-directory (after rename-info-buffer activate)
  (info-rename-buffer))
(defadvice Info-goto-node (before make-copy-info-buffer activate)
  (info-make-backup))
(defadvice Info-goto-node (after rename-new-info-buffer activate)
  (info-rename-buffer)
  (info-copy-from-backup))

(add-hook 'eshell-mode-hook #'rename-shell-buffer)
(add-hook 'eshell-directory-change-hook #'rename-shell-buffer)

;;; shell-mode echoes commands lol
(defun setup-shell-mode ()
  (setq comint-process-echoes t)
  (set-process-query-on-exit-flag
   (get-buffer-process (current-buffer)) nil)
  (add-hook 'comint-output-filter-functions
            (lambda (&rest args) (rename-shell-buffer))
            nil t)
  (linum-mode -1)
  (rename-shell-buffer))

(add-hook 'shell-mode-hook #'setup-shell-mode)

(add-hook 'comint-preoutput-filter-functions #'xterm-color-filter)

;; (add-hook 'help-mode-hook (lambda () (help-info-get-buffer-name 'help-mode)))


;;; save and reset window configuration to ring
(defvar window-configuration-ring nil
  "List of saved window configurations; only stays present within a session.")


;;; do ssh-agent stuff
(defun setup-ssh-agent ()
  (interactive)
  (when (and (eq system-type 'gnu/linux))
    (if (and id-rsa-path (file-exists-p id-rsa-path))
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
            (setenv "DISPLAY" ":0")
            (setenv "SSH_ASKPASS"
                    (expand-file-name
                     (concat init-home-folder-dir
                             "init-scripts/read-ssh-pass.sh")))
            (with-temp-buffer
              (loop with ssh-add-success = nil
                    with ssh-did-fail = nil
                    while (not ssh-add-success)
                    do (progn
                         (insert
                          (or (and (not ssh-did-fail) ssh-pass)
                              (read-passwd
                               (if ssh-did-fail
                                   "incorrect password. ssh password: "
                                 "ssh password: "))))
                         (if (zerop (shell-command-on-region
                                     (point-min) (point-max)
                                     (concat "ssh-add \"" id-rsa-path "\"")))
                             (setq ssh-add-success t)
                           (erase-buffer)
                           (setq ssh-did-fail t)))))
            (add-hook 'kill-emacs-hook
                      (lambda ()
                        (call-process "kill" nil nil nil
                                      (getenv "SSH_AGENT_PID"))))))
      (with-current-buffer "*scratch*"
        (insert "Set up an id-rsa-path! Only if you want to, though.
Check out your .emacs.\n")))))


;;; TODO: persist buffers not visiting files to disk as well because apparently
;;; sublime does this by default and we can't let sublime beat us; let's also do
;;; the same for tramp buffers (this requires logging in; "luckily" emacs is
;;; synchronous so that should still work. may get annoying for people who tramp
;;; around a lot. let's make that an option)


;;; erc setup
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)
(defun destroy-all-erc-stuff ()
  (interactive)
  (loop for buf in (buffer-list)
        do (with-current-buffer buf
             (when (eq major-mode 'erc-mode)
               (kill-this-buffer)))))
(defun create-erc-modded-chans-string (erc-modded-chans-alist)
  (if (null erc-modded-chans-alist) "No erc channels open."
    (concat
     "["
     (substring
      (reduce
       (lambda (a b) (concat a " " b))
       (remove-if
        #'null
        (mapcar
         (lambda (el)
           (let ((face (if (listp (nthcdr 2 el)) (fourth el) (nthcdr 2 el))))
             (and (buffer-name (first el)) face
                  (propertize (buffer-name (first el)) 'face face))))
         erc-modded-chans-alist))
       :initial-value " ")
      2)
     "]")))
(defun message-erc-modded-chans (prefix-given)
  (interactive "P")
  (message
   "%s"
   (if prefix-given erc-modified-channels-object
     (create-erc-modded-chans-string erc-modified-channels-alist))))
(defun message-erc-modded-chans-when-erc-mode ()
  (when (eq major-mode 'erc-mode)
    (message-erc-modded-chans nil)))
;;; update me with irc activity, but only if i'm in an erc buffer
(setq mode-line-modes
      (remove '(t erc-modified-channels-object) mode-line-modes))
(setq prev-modes mode-line-modes)
(setq mode-line-modes
      (loop with l = mode-line-modes
            for el in '("%[" "(" ")" "%]")
            do (setq l (remove el l))
            finally (return l)))
(defun when-list (el) (let ((obj el)) (if (listp obj) obj nil)))
(defun apply-list-funs-to-el (el &rest funs)
  (loop for fun in funs
        with res = el
        do (setq res (funcall fun (when-list res)))
        finally (return res)))
(setq mode-line-modes
      (remove-if
       (lambda (el)
         (and (listp el)
              (eq (apply-list-funs-to-el el #'cdr #'car #'second)
                  'minor-mode-alist)))
                 mode-line-modes))
(defadvice erc-server-filter-function
    (after update-erc-status-on-erc-bufs activate)
  (setq mode-line-modes
        (remove '(t erc-modified-channels-object) mode-line-modes))
  (message-erc-modded-chans-when-erc-mode))
;;; update me with activity when i switch to an erc buffer
(defadvice switch-to-buffer
    (after update-erc-status-on-switch-to-erc-buf activate)
  (message-erc-modded-chans-when-erc-mode))
 (defadvice windmove-do-window-select
    (after update-erc-status-on-windmove activate)
  (message-erc-modded-chans-when-erc-mode))
;;; don't open up windows randomly!!!!
(defadvice erc-cmd-JOIN (around erc-bufs-do-not-explode activate)
  (let ((prev-win-conf (current-window-configuration)))
    ad-do-it
    (set-window-configuration prev-win-conf)))

;;; see what i just killed
(add-hook 'kill-buffer-hook #'kill-message-buffer-id)

;;; don't like seeing wraparound
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'eww-after-render-hook #'refresh-visual-line-mode)
(add-hook 'litcoffee-mode-hook #'visual-line-mode)

;;; compilation-mode
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; so i can manually git commit from within eshell on windows
(setenv "EDITOR" "emacsclient")

;;; setup submodules and make them
(add-hook 'after-load-init-hook
          (lambda ()
            (actual-setup-submodules
             (lambda ()
               (add-to-list
                'load-path (concat init-home-folder-dir "emacs-color-themes"))
               (require 'color-theme-danny)))
            (actual-make-all-submodules)))

;;; ibuffer moves things around when i mark things and this scares me
(defadvice ibuffer-mark-interactive (after re-recenter activate) (recenter))

(defvar-local is-find-dired-buffer nil)

;;; dired doesn't color the bottom file in a listing upon pressing
;;; TODO: submit fix for this
(defadvice find-dired (after tell-me-if-find-dired activate)
  (setq is-find-dired-buffer t))
(defadvice dired-mark (after dont-lie-to-me activate)
  (unless is-find-dired-buffer (revert-buffer)))

;;; i like living on the edge
(setq dired-deletion-confirmer (lambda (&rest args) t))
(defun dired-find-marked-files-no-show ()
  "Better than `dired-do-find-marked-files'."
  (interactive)
  (cl-mapc #'find-file-noselect (dired-get-marked-files)))

;;; keep dired in sync
(add-hook 'dired-mode-hook #'auto-revert-mode)

(defadvice ansi-color-filter-apply (around no-errors activate)
  (let ((res (ignore-errors ad-do-it)))
    (ignore-errors (setq ad-return-value (or res (ad-get-arg 0))))))

(defadvice shell (around choose-shell activate)
  (let ((explicit-shell-file-name
         (if (string-match-p "^/ssh:" default-directory) "/bin/bash"
           explicit-shell-file-name)))
    ad-do-it))

(make-submodule
 "org-mode" "make"
 (lambda ()
   (add-to-list 'load-path (concat init-home-folder-dir "org-mode/lisp/"))
   (autoload #'org-element-update-syntax "org-element.el")
   (autoload #'org-define-error "org-compat.el")
   (require 'org))
 nil)

(defadvice eww-follow-link (after revis activate) (refresh-visual-line-mode))

(add-hook #'prog-mode-hook #'highlight-parentheses-mode)

(defadvice browse-url-chromium (around no-error activate)
  (ignore-errors ad-do-it))
(defadvice browse-url-chrome (around no-error activate)
  (ignore-errors ad-do-it))

(defadvice man (around no-switch-buf activate)
  (let* ((win (selected-window))
         (buf ad-do-it))
    (unless current-prefix-arg
      (quit-windows-on buf)
      (with-selected-window win
        (display-buffer-same-window buf nil)))))

(defadvice woman (around no-switch-buf activate)
  (let* ((win (selected-window))
         (win-pt (window-point win))
         (win-st (window-start win))
         (prev-buf (current-buffer)))
    ad-do-it
    (let ((buf (get-buffer (cdr (first woman-buffer-alist)))))
      (unless current-prefix-arg
        (quit-windows-on buf)
        (with-selected-window win
          (set-window-buffer win prev-buf)
          (set-window-point win win-pt)
          (set-window-start win win-st)
          (display-buffer-same-window buf nil))))))

(defadvice list-processes (around no-switch-buf activate)
  (let* ((prev-config (current-window-configuration)))
    ad-do-it
    (unless current-prefix-arg
      (set-window-configuration prev-config)
      (switch-to-buffer "*Process List*"))))

(defadvice dired-async-after-file-create (after revert-bufs activate)
  (run-with-timer 0 nil #'revert-buffer nil t))

;;; make sure we get any custom paths we add to the shell
(let ((true-path
       (concat
        (shell-command-to-string "echo -n \"$PATH\"") ":" (getenv "PATH"))))
  (setenv "PATH" true-path)
  (setq exec-path (append (split-string true-path ":") exec-path)))

(setq ring-bell-function (lambda ()))

;;; TODO: make this work, then add these; they get kind of annoying, though
;; (add-hook 'shell-mode-hook #'set-mark-end-process-output-mode)
;; (add-hook 'compilation-mode-hook #'set-mark-end-process-output-mode)

(defvar window-before-compilation nil)
(defvar buffer-before-compilation nil)
(defvar compilation-window nil)
(defadvice smart-compile (before save-buffer activate)
  (setq buffer-before-compilation (current-buffer)))
(defadvice smart-compile (after switch-to-buffer activate)
  (setq window-before-compilation (selected-window))
  (pop-to-buffer "*compilation*")
  (setq compilation-window (selected-window)))
(defun back-to-window-before-compilation ()
  (interactive)
  (cond ((and
          (window-live-p window-before-compilation)
          (not (string= (buffer-name (window-buffer window-before-compilation))
                        "*compilation*")))
         (select-window window-before-compilation))
        ((buffer-live-p buffer-before-compilation)
         (pop-to-buffer buffer-before-compilation)
         (setq window-before-compilation
               (get-buffer-window buffer-before-compilation)))
        (t (message "%s" "no window before compilation!"))))
(defun back-to-compilation-window ()
  (interactive)
  (cond ((and
          (window-live-p compilation-window)
          (string= (buffer-name (window-buffer compilation-window))
                   "*compilation*"))
         (select-window compilation-window))
        ((buffer-live-p (get-buffer "*compilation*"))
         (pop-to-buffer "*compilation*")
         (setq compilation-window (get-buffer-window "*compilation*")))
        (t (message "%s" "no compilation window!"))))
(defun reset-window-before-compilation ()
  (interactive)
  (if (string= (buffer-name) "*compilation*")
      (message "%s" "already in compilation buffer!")
    (setq buffer-before-compilation (current-buffer)
          window-before-compilation (selected-window))))

;;; char folding
;; add minus sign − to equivalence for - (hyphen)
(require 'char-fold)
(defvar orig-char-fold-table (let ((tab (make-char-table 'backup)))
                               (set-char-table-parent tab char-fold-table)
                               tab))

(defun add-char-fold-chars (base added)
  (let* ((entry (char-table-range orig-char-fold-table base))
         (chars (cl-loop for i from 0 upto (- (length entry) 1)
                         for s = (substring entry i (1+ i))
                         when (string-match-p
                               (char-fold-to-regexp (char-to-string base))
                               s)
                         collect s))
         (opt-reg
          (regexp-opt (append chars (cl-mapcar #'char-to-string added)))))
    (set-char-table-range orig-char-fold-table base entry)
    (set-char-table-range char-fold-table base opt-reg)))

;;; minus signs
(defun add-minus-eq-char-folds ()
  (with-eval-after-load 'dash
    (let ((dash-sym ?-)
          (eq-sym ?=)
          (added-dash-syms '(?− ?‐)))
      (add-char-fold-chars dash-sym added-dash-syms)
      (-if-let*
          ((dash-fold (char-table-range char-fold-table dash-sym))
           (dash-matched (string-match "\\`\\[\\([^]]+?\\)\\]\\'" dash-fold))
           (dash-chars (append (match-string 1 dash-fold) nil)))
          (add-char-fold-chars eq-sym dash-chars)
        (throw 'init-fail "char fold customizations failed!")))))
(add-minus-eq-char-folds)

(with-eval-after-load 'misearch
  (remove-hook 'isearch-mode-hook #'multi-isearch-setup))

(defconst regexp-special '("\\(?:" "\\(" "\\|" "\\)" "." "[" "]" "?" "??"
                           "*" "*?" "+" "+?" "-"))
(defconst regexp-special-impure '("\\(?:\\`\\|\\\\(\\|\\\\(?:\\|\\[\\)\\^"
                                  "\\$\\(?:\\'\\|\\\\)\\)"
                                  "\\[:[a-z]+:\\]"
                                  "\\\\{[0-9]+\\(?:,[0-9]+\\)?\\\\}"
                                  "\\\\[a-zA-Z=`'<>]"))
(defconst backslash-regexp-special "\\(?:\\\\\\\\\\)*")
(defconst regexp-special-coalesced
  (let ((concd
         (format "\\(?:%s\\|%s\\)"
                 (mapconcat (lambda (str) (format "\\(?:%s\\)" str))
                            regexp-special-impure "\\|")
                 (regexp-opt regexp-special))))
    (concat backslash-regexp-special concd)))

(defun char-fold-non-special (str)
  (operate-on-non-regexp-special #'char-fold-to-regexp str))

(defun word-boundary-fold (str)
  (replace-regexp-in-string "[[:space:]\n]+" "\\(?:.\\|\n\\)*?"
                            (trim-whitespace str) nil t))

(defun do-complete-char-word-fold (re)
  (word-boundary-fold (char-fold-non-special re)))

(defun do-normal-isearch (str &optional bound noerror count)
  (do-isearch str str bound noerror count))

(defun do-isearch (real-regexp string bound noerror count)
  (let ((fun (if isearch-forward #'re-search-forward #'re-search-backward)))
    (condition-case er
        (funcall fun real-regexp bound noerror count)
      (search-failed
       (signal (car er)
               (if-let ((prefix (get isearch-regexp-function
                                     'isearch-message-prefix)))
                   (and (stringp prefix)
                        (list (format "%s   [using %ssearch]" string prefix)))
                 (cdr er)))))))

(defun isearch-fast-and-loose (string &optional bound noerror count)
  (do-isearch
   (if isearch-normal-search string (do-complete-char-word-fold string))
   string bound noerror count))

(defvar isearch-normal-search nil)

(isearch-define-mode-toggle normal "l" nil "\
Turning on normal search turns off fast-and-loose mode."
  (setq isearch-normal-search (not isearch-normal-search))
  (isearch--momentary-message
   (if isearch-normal-search "normal search" "abnormal search")))

(defcustom my-isearch-search-fun #'isearch-fast-and-loose
  "`defcustom' because it deserves one."
  :type 'function
  :group 'isearch)
(defun isearch-get-fun () (symbol-function my-isearch-search-fun))
(setq isearch-search-fun-function #'isearch-get-fun)

(defface cperl-no-trailing-whitespace-face
  '((((class color))))
  "NO" :group 'cperl-mode)


;;; TODO: make this into a MELPA package
(defvar my-rw-process nil)
(defvar my-rw-process-tmp-dir nil)
(defvar my-rw-process-fifo nil)
(defvar my-rw-process-output-buf nil)
(defvar my-rw-process-err-buf nil)

(defconst my-rw-process-output-actions-alist
  '((nil #'current-buffer)
    (discard)))

(defcustom my-rw-process-only-me t
  "If non-nil, `my-rw-process-tmp-dir' and `my-rw-process-fifo' are only
accessible to the user who started the current emacs process."
  :type 'boolean)
(defcustom my-rw-process-dir-prefix "emacs-rw-process"
  "Prefix for `my-rw-process-tmp-dir'."
  :type 'string)
(defcustom my-rw-process-dir-suffix ""
  "Suffix for `my-rw-process-tmp-dir'."
  :type 'string)
(defcustom my-rw-named-pipe-filename "emacs-rw-pipe"
  "Filename used when creating `my-rw-process-fifo'."
  :type 'string)
(defcustom my-rw-process-base-name "emacs-rw-proc"
  "Process name to use when creating `my-rw-process'."
  :type 'string)
(defcustom my-rw-process-buffer-base-name "emacs-rw-proc-buf"
  "Buffer name to use when creating `my-rw-process'."
  :type 'string)
(defcustom my-rw-process-query-on-exit nil
  "Whether to ask before killing `my-rw-process'."
  :type 'boolean)

(defconst all-rw-mode-bits (file-modes-symbolic-to-number "a=rw"))
(defconst user-rw-mode-bits (file-modes-symbolic-to-number "u=rw"))

(defun make-named-pipe (path mode)
  (unless (stringp path)
    (error "PATH must be a string: ('%S')" path))
  (with-temp-buffer
    (let ((code
           (cl-case system-type
             (windows-nt
              (error "%s" "`make-named-pipe' must be implemented for windows!"))
             (t (call-process "mkfifo" nil t nil
                              "-m" (number-to-string mode) path)))))
      (unless (zerop code)
        (error "making pipe failed with code '%d'. stdout/stderr:\n%s"
               code (buffer-string))
        path))))

(defun make-rw-proc-dir ()
  (let* ((new-dir (make-temp-file
                   my-rw-process-dir-prefix my-rw-process-dir-suffix))
         (new-fifo
          (let ((default-directory new-dir))
            (make-named-pipe
             (expand-file-name my-rw-named-pipe-filename) user-rw-mode-bits))))
    (unless my-rw-process-only-me
      (set-file-modes new-dir all-rw-mode-bits)
      (set-file-modes new-fifo))
    (list :dir new-dir :fifo new-fifo)))

(defconst my-rw-process-dead-proc-base-name "*dead-rw-proc*")
(defconst my-rw-process-dead-buf-base-name "*dead-rw-proc-buf*")

(defun clear-old-rw-proc ()
  (let* ((old-proc (and (processp my-rw-process) my-rw-process))
         (old-buf (and old-proc (process-buffer old-proc)))
         (ret ))
    ;; TODO: make all literal strings into constants
    (list
     :new-buf (generate-new-buffer
               (format "*%s*" my-rw-process-buffer-base-name))
     :new-err-buf (generate-new-buffer
                   (format "*%s-errors*" my-rw-process-buffer-base-name))
     :old-proc (and (process-live-p old-proc) old-proc)
     :old-buf (and (buffer-live-p old-buf) old-buf))))

(defun read-pipe-filter (proc msg)
  (with-current-buffer (process-buffer proc)
    (let* ((win (get-buffer-window (current-buffer) t))
           (at-end (= (window-point win) (point-max))))
      (save-excursion
        (goto-char (point-max))
        (insert msg))
      (when at-end
        (with-selected-window win
          (goto-char (point-max)))))))

(defun read-from-pipe (fname)
  (let ((path (expand-file-name fname)))
    (unless (file-exists-p (expand-file-name fname))
      (throw 'pipe-does-not-exist
             (format "the named pipe at '%s' does not exist"
                     fname)))
    (let* ((buf (generate-new-buffer (format "pipe@%s" path)))
           (cat-proc (make-process
                      :name (format "pipe-read@%s" path)
                      :buffer buf
                      :command (list "cat" path)
                      :connection-type 'pipe
                      :filter #'read-pipe-filter
                      :sentinel #'ignore)))
      (switch-to-buffer buf))))

(defun my-rw-pipe-write (str &optional wait)
  (process-send-string my-rw-process str)
  (when wait
    (accept-process-output my-rw-process wait nil t)))

(defun my-rw-process-sentinel (proc ev)
  nil
  ;; (when (buffer-live-p my-rw-process-err-buf)
  ;;   (with-current-buffer))
  ;; (with-current-buffer
  ;;     (process-e))
  ;; (-when-let*
  ;;     ((code (and (process-live-p proc) (process-exit-status proc))))
  ;;   3)
  )

(defun make-rw-process (name buf err-buf inp)
  (make-process
   :name name
   :buffer buf
   :command (list "cat" inp)
   :connection-type 'pipe
   :noquery my-rw-process-query-on-exit
   :stderr err-buf))

;;; TODO: clean up least-recent/largest nonvisiting buffers once number gets too
;;; high! (keep track of nonvisiting buffers and remove duplicate bufs too)

(defun create-my-rw-process (&optional force-restart)
  "Set up `my-rw-process' communicating with `my-rw-process-fifo'. If
FORCE-RESTART is non-nil, then kill the current process if it already
exists. Returns (list :proc RW-PROCESS :fifo RW-PROC-FIFO :started WAS-STARTED),
where RW-PROCESS is a process object, RW-PROC-FIFO is a path, and WAS-STARTED
indicates whether this function created a new asynchronous process.

Note: do NOT rely on the name of the created process's buffer to stay the same!
Use (process-buffer `my-rw-process') instead."
  (pcase (append (clear-old-rw-proc)
                 (make-rw-proc-dir))
    (`(:new-buf ,new-buf :new-err-buf ,err-buf
       :old-proc ,old-proc :old-buf ,old-buf
       :dir ,dir :fifo ,fifo)
     (let* ((send-name (format "*send-%s*" my-rw-process-base-name))
            (recv-name (format "*recv-%s*" my-rw-process-base-name))
            (send-proc
             (make-process
              :name send-name
              :buffer nil
              :command (list "dd" (format "of=%s" fifo))
              :connection-type 'pipe
              :noquery my-rw-process-query-on-exit
              :stderr err-buf))
            (recv-proc
             (make-process
              :name recv-name
              :buffer new-buf
              :command (list "dd" (format "if=%s" fifo))
              :connection-type 'pipe
              :noquery my-rw-process-query-on-exit
              :stderr err-buf))))
     (async-start
      (lambda ()
        ()))
     )
    (_ (error "%s" "unknown error!"))))

(put 'upcase-region 'disabled nil)

(setq visible-bell nil)
