;;; smart-compile.el --- an interface to `compile'

;; Copyright (C) 1998-2012  by Seiji Zenitani

;; Author: Seiji Zenitani <zenitani@mac.com>
;; $Id: smart-compile.el 764 2012-07-10 15:58:08Z zenitani $
;; Keywords: tools, unix
;; Created: 1998-12-27
;; Compatibility: Emacs 21 or later
;; URL(en): http://www.emacswiki.org/emacs/smart-compile.el
;; URL(jp): http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#smart-compile

;; Contributors: Sakito Hisakura, Greg Pfell

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package provides `smart-compile' function.
;; You can associates a particular file with a particular compile functions,
;; by editing `smart-compile-alist'.
;;
;; To use this package, add these lines to your .emacs file:
;;     (require 'smart-compile)
;;
;; Note that it requires emacs 21 or later.

;;; Code:

(eval-when-compile (require 'cl))
(require 'unix-find)
(require 'functions)
(require 'utilities)

(defgroup smart-compile nil
  "An interface to `compile'."
  :group 'processes
  :prefix "smart-compile")

(defun strip-minus-c (str)
  (replace-regexp-in-string
   (concat
    "[[:space:]]*\\-C\\'\\|"
    "[[:space:]]*\\-C[[:space:]]*./\\'\\|"
    "\\./\\([\\./]+\\)/")
   "\\1" str))

(defun add-compile-script-name (str)
  (concat
   (replace-regexp-in-string "\\`sh  " "sh " str)
   "compile.sh"))

(defun byte-compile-file-and-remove ()
  (interactive)
  (byte-compile-file (file-name-nondirectory (buffer-file-name)))
  (shell-command (concatenate 'string "rm " (buffer-file-name) ".elc")))

(defun go-fmt-file ()
  (interactive)
  (save-buffer)
  ;; makes temporary file with contents of gofmt
  (shell-command
   (concatenate
    'string "gofmt "
    (convert-standard-filename (buffer-file-name)) " > "
    (convert-standard-filename (concat (buffer-file-name) ".fmt"))))
  (shell-command (concatenate 'string
                              (if (eq system-type 'windows-nt)
                                  "del "
                                "rm ")
                              (convert-standard-filename (buffer-file-name))))
  (shell-command (concatenate 'string
                              (if (eq system-type 'windows-nt)
                                  "move "
                                "mv ")
                              (convert-standard-filename
                               (concat (buffer-file-name) ".fmt "))
                              (convert-standard-filename (buffer-file-name))))
  (revbufs))

(defun go-fmt-file-and-compile ()
  (interactive)
  (go-fmt-file)
  (compile (concatenate 'string "go build " (buffer-file-name))))

(defcustom smart-compile-alist '(
                                 (emacs-lisp-mode    . (emacs-lisp-byte-compile))
                                 (lisp-mode          . (byte-compile-file-and-remove)) ;; may want to take advantage of the formatting emacs does when byte-compiling files!
                                 ("\\.jl\\'"         . "julia %f") ;; for some reason detecting purely by "julia-mode" doesn't work
                                 (html-mode          . (browse-url-of-buffer))
                                 (nxhtml-mode        . (browse-url-of-buffer))
                                 (html-helper-mode   . (browse-url-of-buffer))
                                 (octave-mode        . (run-octave))
                                 (c-mode             . "clang -Wall -Wextra -Werror %f -o %n -lm")
                                 (c++-mode           . "clang++ -std=c++14 -Wall -Wextra -Werror %f -o %n")
                                 ("\\.m\\'"          . "gcc -O2 %f -lobjc -lpthread -o %n") ;; objective-c
                                 (java-mode          . "javac %f")
                                 ("\\.php\\'"        . "php -l %f")
                                 ;; using dragonegg for fortran and ada cause clang isn't available for them yet
                                 (fortran-mode       . "gcc-4.6 -fplugin=/usr/lib/dragonegg-3.3.src/dragonegg.so -Wall -Wextra -Werror %f -o %n")
                                 (ada-mode           . "gcc-4.6 -fplugin=/usr/lib/dragonegg-3.3.src/dragonegg.so -Wall -Wextra -Werror %f -o %n")
                                 ("\\.cron\\(tab\\)?\\'" . "crontab %f")
                                 ("\\.tex\\'"        . (tex-file))
                                 ("\\.texi\\'"       . "makeinfo %f")
                                 ("\\.mp\\'"         . "mptopdf %f")
                                 ("\\.cjsx\\'"       . "cjsx -bc --no-header %f")
                                 (perl-mode          . "perl -cw %f")
                                 (ruby-mode          . "ruby -cw %f")
                                 (haskell-mode       . "ghc -Wall -Werror %f")
                                 (python-mode        . "python %f")
                                 (js-mode            . "node %f")
                                 (csharp-mode        . "mcs %f -out:%n")
                                 ("\\.R\\'"          . "Rscript %f")
                                 ("\\.r\\'"          . "Rscript %f")
                                 (go-mode            . (go-fmt-file-and-compile))
                                 (coffee-mode        . "coffee -bc --no-header %f")
                                 (qmake-mode         . "qmake")
                                 (cmake-mode         . "cmake %d")
                                 (qmake-mode         . "qmake")
                                 )  "Alist of filename patterns vs corresponding format control strings.
Each element looks like (REGEXP . STRING) or (MAJOR-MODE . STRING).
Visiting a file whose name matches REGEXP specifies STRING as the
format control string.  Instead of REGEXP, MAJOR-MODE can also be used.
The compilation command will be generated from STRING.
The following %-sequences will be replaced by:

  %F  absolute pathname            ( /usr/local/bin/netscape.bin )
  %f  file name without directory  ( netscape.bin )
  %n  file name without extension  ( netscape )
  %e  extension of file name       ( bin )
  %d  directory without filename   ( /usr/local/bin )

  %o  value of `smart-compile-option-string'  ( \"user-defined\" ).

If the second item of the alist element is an emacs-lisp FUNCTION,
evaluate FUNCTION instead of running a compilation command.
"
                                    :type '(repeat
                                            (cons
                                             (choice
                                              (regexp :tag "Filename pattern")
                                              (function :tag "Major-mode"))
                                             (choice
                                              (string :tag "Compilation command")
                                              (sexp :tag "Lisp expression"))))
                                    :group 'smart-compile)
(put 'smart-compile-alist 'risky-local-variable t)

(defconst smart-compile-replace-alist '(
                                        ("%F" . (buffer-file-name))
                                        ("%f" . (file-name-nondirectory (buffer-file-name)))
                                        ("%n" . (file-name-sans-extension
                                                 (file-name-nondirectory (buffer-file-name))))
                                        ("%e" . (or (file-name-extension (buffer-file-name)) ""))
                                        ("%o" . smart-compile-option-string)
                                        ("%d" . (file-name-directory (buffer-file-name)))
                                        ("%U" . (user-login-name))
                                        ))
(put 'smart-compile-replace-alist 'risky-local-variable t)

;;; these are the things i added for automated build systems
(defvar-local smart-compile-check-pants t)
(defvar-local pants-build-file nil)
(defvar-local smart-compile-check-sbt t)
(defvar-local sbt-build-file nil)

(defcustom smart-compile-make-program "make "
  "The command by which to invoke the make program."
  :type 'string
  :group 'smart-compile)

(defcustom smart-compile-option-string ""
  "The option string that replaces %o.  The default is empty."
  :type 'string
  :group 'smart-compile)


(defun smart-compile--simplify-root-path (path)
  (cl-assert (file-directory-p path))
  (format "~/%s" (file-relative-name path (expand-file-name "~"))))

(defun smart-compile--simplify-root-file (file-path)
  (cl-assert (and (file-readable-p file-path)
                  (not (file-directory-p file-path))))
  (format "~/%s" (file-relative-name file-path (expand-file-name "~"))))

;;;###autoload
(defun smart-compile (&optional arg)
  "An interface to `compile'.
It calls `compile' or other compile function,
which is defined in `smart-compile-alist'."
  (interactive "p")
  (let ((name (buffer-file-name))
        (not-yet t))
    (if (not name)
        (error "cannot get filename."))
    (save-buffer)
    (macrolet
        ((add-build-system
          (cmd file-regexp depth use-dir case-matters dir-in-command
               &optional filter-command)
          (let ((build-file (gensym))
                (decided-against-it (gensym))
                (cmd-result (gensym))
                (filter-cmd (gensym)))
            (make-variable-buffer-local build-file)
            (make-variable-buffer-local decided-against-it)
            (set build-file nil)
            (set filter-cmd filter-command)
            (set-default build-file nil)
            (set decided-against-it t)
            (set-default decided-against-it t)
            `(when (setq
                    ,cmd-result ,cmd
                    ,build-file
                    (and ,decided-against-it
                         (or
                          ,@(loop
                             for i from 0 to depth
                             collect `(reduce
                                       #'or-fun
                                       (remove-if-not
                                        #'file-readable-p
                                        (unix-find
                                         ,(concat "./" (concat-n ".." i "/"))
                                         :maxdepth 1
                                         ,(if case-matters :regex :iregex)
                                         ,file-regexp))
                                       :initial-value nil)))))
               (if (y-or-n-p (format "%s found. Try %s?" ,build-file
                                     (if ,filter-cmd
                                         (funcall ,filter-cmd ,cmd-result)
                                       ,cmd-result)))
                   (progn
                     (set (make-local-variable 'compile-command)
                          (let ((res
                                 (format
                                  ,@(if dir-in-command
                                        `("%s %s" ,cmd-result
                                          ,(if use-dir
                                               `(if (file-directory-p
                                                     ,build-file)
                                                    ,build-file
                                                  (smart-compile--simplify-root-path (file-name-directory ,build-file)))
                                             (and (stringp build-file)
                                                  (file-readable-p build-file)
                                                  (smart-compile--simplify-root-file build-file))))
                                      `("%s" ,cmd-result)))))
                            (if ,filter-cmd (funcall ,filter-cmd res)
                              res)))
                     (call-interactively 'compile)
                     (setq not-yet nil)
                     t)
                 (setq ,decided-against-it nil))))))
      (cond
       ;; local command
       ;; The prefix 4 (C-u M-x smart-compile) skips this section
       ;; in order to re-generate the compile-command
       ((and (not (= arg 4)) ; C-u M-x smart-compile
             (local-variable-p 'compile-command)
             compile-command)
        (call-interactively 'compile)
        (setq not-yet nil))
       ((add-build-system "make -C" "^Makefile$" 4 t nil t strip-minus-c))
       ((add-build-system "node-gyp build" "^binding\\.gyp$" 4 nil nil nil))
       ((add-build-system "scons -C" "^SConstruct$" 4 t nil t strip-minus-c))
       ((add-build-system "cake build" "^Cakefile$" 4 nil nil nil))
       ((add-build-system
         "sh " "^compile\\.sh$" 4 t nil t add-compile-script-name))
       ((add-build-system
         (if (eq system-type 'windows-nt) "msbuild.exe" "xbuild")
         "^.*\\.sln$" 5 nil nil t))
       ((and smart-compile-check-pants
             (setq pants-build-file
                   (cl-find-if
                    #'file-exists-p
                    (append
                     '("./pants")
                     (cl-loop
                      for num from 1 to 15
                      collect (concat (repeat-string num "../") "pants"))))))
        (setq-local compile-command
                    (format "pushd %s && ./pants "
                            (smart-compile--simplify-root-path
                             (file-name-directory pants-build-file))))
        (call-interactively 'compile)
        (setq not-yet nil))
       ((and smart-compile-check-sbt
             (setq sbt-build-file
                   (cl-find-if
                    #'file-exists-p
                    (append
                     '("./build.sbt")
                     (cl-loop
                      for num from 1 to 15
                      collect
                      (concat (repeat-string num "../") "build.sbt"))))))
        (setq-local compile-command
                    (format "pushd %s >/dev/null && sbt "
                            (smart-compile--simplify-root-path
                             (file-name-directory sbt-build-file))))
        (call-interactively 'compile)
        (setq not-yet nil))))

    ;; compile
    (let( (alist smart-compile-alist)
          (case-fold-search nil)
          (function nil) )
      (while (and alist not-yet)
        (if (or
             (and (symbolp (caar alist))
                  (eq (caar alist) major-mode))
             (and (stringp (caar alist))
                  (string-match (caar alist) name))
             )
            (progn
              (setq function (cdar alist))
              (if (stringp function)
                  (progn
                    (set (make-local-variable 'compile-command)
                         (smart-compile-string function))
                    (call-interactively 'compile)
                    )
                (if (listp function)
                    (eval function)
                  ))
              (setq alist nil)
              (setq not-yet nil)
              )
          (setq alist (cdr alist)) )
        ))

    ;; If compile-command is not defined and the contents begins with "#!",
    ;; set compile-command to filename.
    (if (and not-yet
             (not (memq system-type '(windows-nt ms-dos)))
             (not (string-match "/\\.[^/]+$" name))
             (not
              (and (local-variable-p 'compile-command)
                   compile-command))
             )
        (save-restriction
          (widen)
          (if (equal "#!" (buffer-substring 1 (min 3 (point-max))))
              (set (make-local-variable 'compile-command) name)
            ))
      )

    ;; compile
    (when not-yet (call-interactively 'compile))))

(defun smart-compile-string (format-string)
  "Document forthcoming..."
  (if (and (boundp 'buffer-file-name)
           (stringp buffer-file-name))
      (let ((rlist smart-compile-replace-alist)
            (case-fold-search nil))
        (while rlist
          (while (string-match (caar rlist) format-string)
            (setq format-string
                  (replace-match
                   (eval (cdar rlist)) t nil format-string)))
          (setq rlist (cdr rlist))
          )
        ))
  format-string)

(provide 'smart-compile)

;;; smart-compile.el ends here
