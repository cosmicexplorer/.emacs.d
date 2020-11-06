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

;;;###autoload
(define-minor-mode warning-highlights-mode
  "Highlight words of warning."
  :lighter " !!"
  (if (not warning-highlights-mode)
      (warning-highlights-turn-off)
    (warning-highlights-turn-on)
    (font-lock-mode 1)))

;;;###autoload
(defun find-warning-words (pfx)
  (interactive "P")
  (if pfx (helm-multi-swoop-all warning-highlights-regexp)
    (helm-swoop :query warning-highlights-regexp)))

;;;###autoload
(defun find-warnings-in-dir (dir)
  (interactive "Mdirectory: ")
  (when (or current-prefix-arg (string-equal dir "")) (setq dir "."))
  (grep (concat init-home-folder-dir "switch-grep.sh" " -E "
                "\"(^|[^a-zA-Z])(" warning-words-grep-regex
                ")([^a-zA-Z]|$)\" \"" dir "\"")))

(provide 'warning-words)
