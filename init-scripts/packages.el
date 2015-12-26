;;; setup packages for emacs

(require 'package)

;;; add package lists

(if use-https
    ;; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
    (progn
      (eval-after-load 'tls '(setq tls-checktrust t))
      (setq package-archives nil)
      (add-to-list 'package-archives
                   '("melpa" . "https://melpa.org/packages/") t)
      (add-to-list 'package-archives
                   '("gnu" . "https://elpa.gnu.org/packages/") t)
      (if (not
           (or (zerop (shell-command "python -m pip show certifi"))
               (zerop (shell-command "python -m pip install --user certifi"))))
          (error "pip not installed, or some other error")
        (if (not (executable-find "gnutls-cli"))
            (error "gnutls-cli not installed!")
          (let ((trustfile
                 (replace-regexp-in-string
                  "\\\\" "/"
                  (replace-regexp-in-string
                   "\n" ""
                   (shell-command-to-string "python -m certifi")))))
            (setq tls-program
                  (list
                   (format
                    "gnutls-cli%s --x509cafile %s -p %%p %%h"
                    (if (memq system-type '(ms-dos windows-nt)) ".exe" "")
                    trustfile)))
            (eval-after-load 'gnutls
              `(progn
                 (setq gnutls-verify-error t)
                 (setq gnutls-trustfiles (list ,trustfile)))))
          ;; check to verify host checking is done correctly
          (let ((bad-hosts
                 (loop for bad
                       in `("https://wrong.host.badssl.com/"
                            "https://self-signed.badssl.com/")
                       if (condition-case e
                              (url-retrieve
                               bad (lambda (retrieved) t))
                            (error nil))
                       collect bad)))
            (if bad-hosts
                (error (format "tls misconfigured; retrieved %s ok"
                               bad-hosts))
              (url-retrieve "https://badssl.com"
                            (lambda (retrieved) t)))))))
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/") t))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages '(
                      2048-game
                      ag
                      async
                      auctex
                      auto-complete
                      better-defaults
                      bison-mode
                      cider
                      cloc
                      clojure-mode
                      coffee-mode
                      color-theme
                      company
                      company-ghc
                      company-ghci
                      csharp-mode
                      csv-mode
                      dash
                      ein
                      ensime
                      epl
                      epresent
                      espuds
                      evil
                      flycheck
                      flycheck-package
                      ggtags
                      ghc
                      git-gutter
                      gnuplot
                      gnuplot-mode
                      go-mode
                      haskell-mode
                      helm
                      helm-ag
                      helm-gtags
                      helm-swoop
                      highlight-parentheses
                      js2-mode
                      less-css-mode
                      linum
                      linum-relative
                      literate-coffee-mode
                      lua-mode
                      magit
                      markdown-mode
                      matlab-mode
                      minimap
                      misc-cmds
                      multiple-cursors
                      omnisharp
                      org
                      package-build
                      pacmacs
                      paredit
                      perl6-mode
                      php-mode
                      pkg-info
                      queue
                      racket-mode
                      rainbow-delimiters
                      rainbow-mode
                      s
                      scala-mode2
                      skewer-mode
                      slime
                      slime-company
                      smartrep
                      sml-mode
                      speech-tagger
                      undo-tree
                      w3m
                      wgrep
                      wgrep-ag
                      wgrep-helm
                      web-beautify
                      xterm-color
                      ))

;;; do the install (slow upon startup, but only for the first time)
(loop for p in my-packages
  do (unless (package-installed-p p)
      (package-install p)))

(when (get-buffer "*Compile-Log*") (delete-windows-on "*Compile-Log*"))
