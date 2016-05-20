;;; setup packages for emacs

(require 'package)

;;; add package lists
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))
(package-initialize)

(defvar my-packages
  '(
    2048-game
    ag
    async
    auctex
    auto-complete
    better-defaults
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
    font-lock-studio
    ggtags
    ghc
    git-gutter
    git-gutter-fringe
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
    mmm-mode
    multiple-cursors
    omnisharp
    org
    package-build
    pacmacs
    paredit
    pdf-tools
    perl6-mode
    php-mode
    pkg-info
    queue
    racket-mode
    rainbow-delimiters
    rainbow-mode
    s
    scala-mode2
    shut-up
    skewer-mode
    slime
    slime-company
    smartrep
    sml-mode
    sourcemap
    speech-tagger
    thrift
    undo-tree
    w3m
    web-beautify
    wgrep
    wgrep-ag
    wgrep-helm
    xterm-color
    yaml-mode
    ))

;;; do the install (slow upon startup, but only for the first time)
(loop for p in my-packages
  do (unless (package-installed-p p)
       (ignore-errors (package-install p))))

(when (get-buffer "*Compile-Log*") (delete-windows-on "*Compile-Log*"))
