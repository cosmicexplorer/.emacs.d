;;; -*- lexical-binding: t -*-

(require 'package)
(require 'cl-lib)

(defcustom check-internet-connection t
  "Whether to check internet connection at init time, recorded in
`has-internet-connection' and exposed through `with-internet-connection'. See
`monitor-internet-connection'."
  :type 'boolean)

(defcustom monitor-internet-connection t
  "Whether to continuously monitor internet connection as described in
`check-internet-connection' with an asynchronous process."
  :type 'boolean)

(defcustom internet-connection-command-alist
  `(("nmcli"
     :pred ,(lambda () (executable-find "nmcli"))
     :check-cmd ("nmcli" "networking" "connectivity" "check")
     :check-rx ,(rx bos "full\n" eos)
     :monitor-cmd ("nmcli" "device" "monitor")
     :monitor-success-rx ,(rx bow "connected" eol)
     :monitor-failure-rx ,(rx bow "disconnected" eol)))
  "Alist of executable programs and listings of command lines to check internet
connectivity."
  :type '(alist :key-type string
                :value-type (list
                             (const :pred) function
                             (const :check-cmd) (repeat string)
                             (const :check-rx) regexp
                             (const :monitor-cmd) (repeat string)
                             (const :monitor-success-rx) regexp
                             (const :monitor-failure-rx) regexp)))

(defun get-internet-connection-command ()
  (cl-loop for entry in internet-connection-command-alist
           for pred = (plist-get (cdr entry) :pred)
           when (funcall pred)
           return entry
           finally return nil))

(defvar has-internet-connection nil
  "Used in macro `with-internet-connection'.")

(defconst has-internet-connection-proc-name "*internet-check-process*")
(defconst has-internet-connection-proc-buf "*internet-check-output*")

;;; TODO: make some wrapper around filters so that they always receive only
;;; complete lines of text as input (be careful about stalling on processes
;;; that DON'T produce that as output, though!!!)
(defun make-internet-connection-monitor-filter (success-rx failure-rx)
  (lambda (proc msg)
    (with-current-buffer (process-buffer proc)
      (insert msg))
    (cond
     ((string-match-p success-rx msg)
      (setq has-internet-connection t))
     ((string-match-p failure-rx msg)
      (setq has-internet-connection nil)))))

(defun internet-connection-monitor-sentinel (proc ev)
  (let ((proc-buf (process-buffer proc)))
    (when (buffer-live-p proc-buf)
      (with-current-buffer proc-buf
        (insert ev)))
    (cond
     ((process-live-p proc))
     ((not (string= ev sentinel-successful-exit-msg))
      (pop-to-buffer-on-error proc-buf 'error
        (list (format "error in the monitor process: '%s'" ev))))
     (monitor-internet-connection
      (setup-internet-connection-check 'both-test)))))

(defun do-check-internet-connection (check-cmd check-rx)
  (-let* (((ch-cmd . ch-args) check-cmd)
          (check-proc-anon
           (apply-partially #'call-process ch-cmd nil t nil))
          ((ch-code output)
           (with-temp-buffer
             (let ((c (apply check-proc-anon ch-args)))
               (list c (buffer-string)))))
          (connected (and (zerop ch-code)
                          (string-match-p check-rx output))))
    (setq has-internet-connection (not (not connected)))
    ch-code))

(defun do-monitor-internet-connection
    (monitor-cmd monitor-success-rx monitor-failure-rx)
  (when (process-live-p has-internet-connection-proc-name)
    (kill-process has-internet-connection-proc-name))
  (when (buffer-live-p has-internet-connection-proc-buf)
    (kill-buffer has-internet-connection-proc-buf))
  (make-process
   :name has-internet-connection-proc-name
   :buffer has-internet-connection-proc-buf
   :command monitor-cmd
   :noquery t
   :filter (make-internet-connection-monitor-filter
            monitor-success-rx monitor-failure-rx)
   :sentinel #'internet-connection-monitor-sentinel))

(defun setup-internet-connection-check (type)
  (let ((cmd (get-internet-connection-command)))
    (if (not cmd)
        (user-error
         "cannot check internet connectivity: %s"
         "no method in `internet-connection-command-alist' can be used")
      (cl-destructuring-bind
          (_ &key
             pred check-cmd check-rx
             monitor-cmd monitor-success-rx monitor-failure-rx)
          cmd
        (cl-ecase type
          ;; if specifically requested, check or monitor regardless of
          ;; defcustoms
          (check
           (do-check-internet-connection check-cmd check-rx))
          (monitor
           (do-monitor-internet-connection
            monitor-cmd monitor-success-rx monitor-failure-rx))
          (both
           (do-check-internet-connection check-cmd check-rx)
           (do-monitor-internet-connection
            monitor-cmd monitor-success-rx monitor-failure-rx))
          (both-test
           (when check-internet-connection
             (do-check-internet-connection check-cmd check-rx))
           (when monitor-internet-connection
             (do-monitor-internet-connection
              monitor-cmd monitor-success-rx monitor-failure-rx))))))))

(define-error 'no-internet-error "no internet connection is available"
  'my-errors)

;;; used in user customizations
(defmacro with-internet-connection (do-error &rest body)
  "Perform BODY only if we can grab a url in a short period of time."
  `(if has-internet-connection
       (progn ,@body)
     ,(when do-error
        (signal 'no-internet-error (list "no internet connection")))))

;;; add package lists
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(when (and (not package-archive-contents)
           (setup-internet-connection-check 'check))
  (package-refresh-contents))

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
    (let ((file (find-file-existing installed-packages-file)))
      (with-current-buffer file
        (let* ((names (split-string (buffer-string) "\n" t))
               (sorted (cl-sort names #'string-lessp))
               (syms (cl-mapcar #'intern sorted)))
          (cl-loop for p in syms
                   unless (cl-find p package-selected-packages :test #'eq)
                   do (package-install p))))
      (kill-buffer file)))
  (when (get-buffer "*Compile-Log*")
    (delete-windows-on "*Compile-Log*")))

(install-packages-from-file)

(add-hook 'kill-emacs-hook #'write-packages-to-file)
