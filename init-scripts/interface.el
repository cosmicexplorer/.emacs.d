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
(add-hook
 'buffer-list-update-hook #'set-correct-trailing-whitespace-all-buffers)
(add-hook 'after-change-major-mode-hook #'set-correct-trailing-whitespace)

;; do backups well and put them into a separate folder
(defconst my-backup-dir (expand-file-name "backup-files" init-home-folder-dir))
(setq backup-directory-alist `(("." . ,my-backup-dir)))
(setq auto-save-file-name-transforms nil)

(defconst my-autosave-dir
  (expand-file-name "auto-save-files" init-home-folder-dir))

(defconst dir-sep
  (if (memq system-type '(windows-nt ms-dos)) "\\"
    "/"))

(defun make-auto-save-file-name ()
  (let* ((fname (buffer-file-name))
         (out-fname (replace-regexp-in-string dir-sep "!" fname)))
    (expand-file-name out-fname my-autosave-dir)))

(defun auto-save-file-name-p (file-basename)
  (file-exists-p (expand-file-name file-basename my-autosave-dir)))

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

(let ((local-bin "/usr/local/bin"))
  (when (file-directory-p local-bin)
    (add-to-list 'exec-path local-bin)))

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
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(setq auto-save-interval 200)
(setq auto-save-timeout 10)
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
(require 'rx)
;; (cl-defun rx-parse-constituent (k v  &key (from rx-constituents) (mut nil))
;;   (let* ((cur-val (alist-get k from))
;;          (to-match (if mut cur-val v)))
;;     (cl-assert (not (xor mut cur-val)))
;;     (msg-evals (v cur-val to-match))
;;     (pcase-exhaustive to-match
;;       ((or (and (pred stringp) defn)
;;            (and (pred symbolp)
;;                 (app
;;                  (lambda (x) (rx-parse-constituent
;;                               x to-match :from from :mut t))
;;                  defn))
;;            (and `(,(and (pred functionp))
;;                   ,(and (or `nil (and (pred integerp) (pred (<= 0)))) min-args)
;;                   ,(and (or `nil (and (pred integerp)
;;                                       (or (and (guard (integerp min-args))
;;                                                (pred (<= min-args)))
;;                                           (guard (null min-args)))))
;;                         (app (message "f2: %s") s))
;;                   ,(and (app (message "f3: %s") s) (or `nil (pred functionp)) (app (message "f4: %s") s)))
;;                 defn))
;;        (msg-evals (defn))
;;        t)
;;       (_ nil))))

;; (alist-get 'and rx-constituents)

;; (alist-get 'word-suffix rx-constituents)

(defconst warning-highlights-regexp
  (rx
   (: bow
      (: (| (: "to" (? (| space punct)) "do")
            (: "fix" (? (| space punct)) (? "me"))
            ;; (: (+? (: alpha (| space punct))) alpha)
            (: "dep" (? (: "end" (? (: "en" (| "t" "c"))))))
            (: "deprecate")
            (>= 3 "x")
            "hack"
            "iffy"
            "change"
            "modif"
            (: "opt" (? "imiz"))
            "broke"
            "break"
            "since"
            "should"
            (: "remov" (| "e" "al" "es" "ed" "er"))
            (: "delet" (| "ed" "es" "er"))
            "warn"
            (: "err" (? "or"))
            "only"
            "review"
            "must"
            "note"
            "need"
            "asap"
            (: "danger" (? "ous"))
            "strict"
            "race"
            "experiment"
            "arbitrary"
            "require"
            "consider"
            "actual"
            "usual"
            "instead"
            "expect"
            "me"
            "this"
            "spec"
            )
         (? (| "ing"
               "age"
               "ish"
               (: (? "ific") "ally")
               "ly"
               "y"
               "ic"
               "al"
               (: (? "e") "n")
               (: (? "a") (? "t") "ion")))
         (? (: (? "i") (? "e") (? (| "s" "d" "r")))))
      eow)))

(defconst warning-highlights-keywords
  `((,warning-highlights-regexp 0 font-lock-warning-face t))
  "Keywords to apply extra highlights to.")

(defun warning-highlights-turn-on ()
  "Turn on warning-highlights-mode."
  (font-lock-add-keywords nil warning-highlights-keywords t)
  (setq-local font-lock-keywords-case-fold-search t))

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
  (if pfx (helm-multi-swoop-all warning-highlights-regexp)
    (helm-swoop :$query warning-highlights-regexp)))

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
(defvar eshell-user-output-file (concat init-home-folder-dir "eshell-output")
  "File containing all eshell I/O from all eshell buffers.")
(add-hook 'eshell-pre-command-hook #'eshell-send-input-to-history)
(add-hook 'eshell-post-command-hook #'eshell-send-output-to-history)

(defun shell-record-history-filters ()
  (add-hook 'comint-input-filter-functions
            #'shell-send-input-to-history nil t)
  (add-hook 'comint-output-filter-functions
            #'shell-send-output-to-history nil t))

(defvar shell-user-output-file (concat init-home-folder-dir "shell-output"))
(add-hook 'shell-mode-hook #'shell-record-history-filters)

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

(add-hook 'help-mode-hook #'help-rename-buffer)

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

(defcustom clean-sampling-period 20
  "Number of times `buffer-list-clean-many-versions' will skip before attempting
to clean up.")
(defvar clean-sample-index 0)
(defun buffer-list-clean-many-versions ()
  (cond
   ((not (wholenump clean-sample-index))
    (setq clean-sample-index 0))
   ((< clean-sample-index clean-sampling-period)
    (incf clean-sample-index))
   ((>= clean-sample-index clean-sampling-period)
    (let* ((buffer-basename-parted
            (--partition-by
             (replace-regexp-in-string "<[0-9]+>\\'" "" (buffer-name it))
             (buffer-list)))
           (repeated-versions
            ;; only need one version of all these
            (--mapcat (-drop 1 it) buffer-basename-parted)))
      (-each repeated-versions #'kill-buffer)
      (setq clean-sample-index 0)))))

(add-hook 'buffer-list-update-hook #'buffer-list-clean-many-versions)

(defvar set-watched-help-buf nil)
(defvar watched-help-buf nil)

(defun buffer-list-update-watcher ()
  (let ((top (car (buffer-list))))
    (when (and set-watched-help-buf
               (not (eq top set-watched-help-buf)))
      (setq set-watched-help-buf nil
            watched-help-buf top))))

(add-hook 'buffer-list-update-hook #'buffer-list-update-watcher)

(defun help-do-button (pos &optional pfx)
  (interactive (list (point) current-prefix-arg))
  (let ((prefix-arg nil))
    (save-window-excursion
      (setq set-watched-help-buf (car (buffer-list)))
      (push-button pos)))
  (if pfx (pop-to-buffer watched-help-buf)
    (display-buffer-same-window watched-help-buf nil))
  (setq watched-help-buf nil))

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


;;; TODO: persist buffers not visiting files to disk as well because apparently
;;; sublime does this by default and we can't let sublime beat us; let's also do
;;; the same for tramp buffers (this requires logging in; "luckily" emacs is
;;; synchronous so that should still work. may get annoying for people who tramp
;;; around a lot. let's make that an option)

;;; see what i just killed
(add-hook 'find-file-hook (message-buffer-id "found"))
(add-hook 'kill-buffer-hook (message-buffer-id "killed"))

;;; don't like seeing wraparound
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'eww-after-render-hook #'refresh-visual-line-mode)
(add-hook 'litcoffee-mode-hook #'visual-line-mode)

;;; compilation-mode
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;; so i can manually git commit from within eshell on windows
(setenv "EDITOR" "emacsclient")

;;; setup submodules and make them
(defconst submodule-dirs
  '("emacs-helm-ag" "emacs-color-themes" "ESS"))

(defun setup-submodules-load ()
  (actual-setup-submodules)
  (cl-mapc
   (lambda (dir)
     (add-to-list
      'load-path (expand-file-name dir init-home-folder-dir)))
   submodule-dirs)
  (add-to-list 'load-path
               (expand-file-name "org-mode/lisp/" init-home-folder-dir))
  (autoload #'org-element-update-syntax "org-element.el")
  (autoload #'org-define-error "org-compat.el")
  (require 'org)
  (require 'helm-ag)
  (require 'color-theme-danny))

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

;;; TODO: figure out a workaround for this
;; (defadvice shell (around choose-shell activate)
;;   (let ((explicit-shell-file-name
;;          (if (string-match-p "^/ssh:" default-directory) "/bin/bash"
;;            explicit-shell-file-name)))
;;     ad-do-it))

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
(defvar orig-char-fold-table
  (let ((tab (make-char-table 'backup)))
    (set-char-table-parent tab char-fold-table)
    tab))

(cl-defun add-char-fold-chars
    (range equiv &optional (spread-range t)
           (target-tbl char-fold-table) (source-tbl target-tbl))
  (let* ((entry (char-table-range source-tbl range))
         (chars (cl-loop for i from 0 upto (- (length entry) 1)
                         for s = (substring entry i (1+ i))
                         when (string-match-p
                               (char-fold-to-regexp (char-to-string range))
                               s)
                         collect s))
         (opt-reg
          (regexp-opt (append chars (cl-mapcar #'char-to-string added)))))
    (set-char-table-range source-tbl range entry)
    (set-char-table-range target-tbl range opt-reg)))


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

;; (add-minus-eq-char-folds)
;; FIXME! make smart quotes equiv to quotes
;; (add-)

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


;;; TODO: make `my-rw-process'-* into a MELPA package!
;;; - create two processes implementing bidirectional communication over named
;;; pipe!
;;;   - why? who knows!
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

(defcustom ag-default-search-fn #'ag
  "Which function to use for searching by default when calling `ag'."
  :type 'function)
(defvar-local ag-local-search-fn ag-default-search-fn
  "Buffer-local binding for function to use to search with `ag'.")

(with-eval-after-spec helm-ag
  (defun my-helm-ag ()
    (interactive)
    (helm-do-ag default-directory)))

(defun stop-adaptive-fill ()
  (setq-local adaptive-fill-mode nil)
  (setq-local adaptive-fill-function nil))

(defconst no-adaptive-fill-modes
  '(lisp-mode-hook
    emacs-lisp-mode-hook))

(cl-mapc (l (add-hook _ #'stop-adaptive-fill)) no-adaptive-fill-modes)

(defun turn-on-set-mark-end-mode ()
  (set-mark-end-process-output-mode 1)
  (set-mark-end-eob))

(add-hook #'messages-buffer-mode-hook #'turn-on-set-mark-end-mode)
(with-current-buffer (messages-buffer)
  (turn-on-set-mark-end-mode))

(setq visible-bell nil)

(require 'book-txt-view)

(defun stop-book-txt-view-hook ()
  (when (and book-txt-view book-txt-view-underlying
             (not (eq book-txt-view-underlying (current-buffer))))
    (rainbow-mode -1)
    (linum-mode -1)))

(add-hook 'book-txt-view-hook #'stop-book-txt-view-hook)

(setq exec-path (get-exec-path))

(setq read-file-name-function #'ido-read-file-name)
