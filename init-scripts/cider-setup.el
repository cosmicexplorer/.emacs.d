;;; -* lexical-binding: t -*-

;;; cider sux (eh)
(defun setup-cider-stuff ()
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-mode-hook 'enable-paredit-mode)
  (add-hook 'cider-mode-hook 'subword-mode))

(eval-after-load "cider"
  '(let ((lein-binary (executable-find "lein"))
         (clojure-binary (executable-find "clojure"))
         (lein-profiles-file (concat (getenv "HOME") "/.lein/profiles.clj")))
     (if (and lein-binary clojure-binary (file-exists-p lein-profiles-file))
         (progn
           (with-temp-buffer
             (insert-file-contents lein-profiles-file)
             (goto-char (point-min))
             (re-search-forward "cider/cider-nrepl[[:space:]]+\"\\(.*\\)\"" nil t)
             (replace-match cider-version nil t nil 1)
             (write-region nil nil lein-profiles-file))
           (setup-cider-stuff))
       (when (and lein-binary clojure-binary)
         (with-temp-buffer
           (insert (concat "{:user {:plugins [[cider/cider-nrepl "
                           "\""
                           ;; hack for now, but should be fine
                           "LATEST"
                           ;; cider-version
                           "\"]]}}"))
           (write-region nil nil lein-profiles-file))
         (setup-cider-stuff)))))

(provide 'cider-setup)
