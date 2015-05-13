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

(defgroup smart-compile nil
  "An interface to `compile'."
  :group 'processes
  :prefix "smart-compile")

(defun byte-compile-file-and-remove ()
  (interactive)
  (byte-compile-file (file-name-nondirectory (buffer-file-name)))
  (shell-command (concatenate 'string "rm " (buffer-file-name) ".elc")) ;; but re-adds it quickly so undo doesn't notice
  )

(defun go-fmt-file ()
  (interactive)
  (save-buffer)
  ;; makes temporary file with contents of gofmt
  (shell-command (concatenate 'string "gofmt " (buffer-file-name) " > " (buffer-file-name) ".fmt"))
  (shell-command (concatenate 'string "rm " (buffer-file-name))) ;; deletes file (!!!)
  ;; resuscitates file from the ashes with gofmt
  (shell-command (concatenate 'string "mv " (buffer-file-name) ".fmt " (buffer-file-name)))
  (revbufs)
  )

(defun go-fmt-file-and-compile ()
  (interactive)
  (go-fmt-file)
  (compile (concatenate 'string "go build " (buffer-file-name)))
  )

(defcustom smart-compile-alist '(
                                 (emacs-lisp-mode    . (emacs-lisp-byte-compile))
                                 (lisp-mode          . (byte-compile-file-and-remove)) ;; may want to take advantage of the formatting emacs does when byte-compiling files!
                                 ("\\.jl\\'"          . "julia %f") ;; for some reason detecting purely by "julia-mode" doesn't work
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
                                 (perl-mode          . "perl -cw %f")
                                 (ruby-mode          . "ruby -cw %f")
                                 (haskell-mode       . "ghc %f -o %n")
                                 (python-mode        . "python %f")
                                 (js-mode            . "node %f")
                                 ("\\.R\\'"          . "Rscript %f") ;; redirects to stdout
                                 ("\\.r\\'"          . "Rscript %f") ;; redirects to stdout
                                 (go-mode            . (go-fmt-file-and-compile))
                                 (coffee-mode        . "coffee -c %f")
                                 (qmake-mode         . "qmake")
                                 (cmake-mode	     . "cmake %d")
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
(defvar smart-compile-check-makefile t)
(make-variable-buffer-local 'smart-compile-check-makefile)
(defvar smart-compile-check-scons-files t)
(make-variable-buffer-local 'smart-compile-check-scons-files)
(defvar smart-compile-check-gyp t)
(make-variable-buffer-local 'smart-compile-check-gyp)
(defvar smart-compile-check-cakefile t)
(make-variable-buffer-local 'smart-compile-check-cakefile)

(defcustom smart-compile-make-program "make "
  "The command by which to invoke the make program."
  :type 'string
  :group 'smart-compile)

(defcustom smart-compile-option-string ""
  "The option string that replaces %o.  The default is empty."
  :type 'string
  :group 'smart-compile)


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

    (cond

     ;; local command
     ;; The prefix 4 (C-u M-x smart-compile) skips this section
     ;; in order to re-generate the compile-command
     ((and (not (= arg 4)) ; C-u M-x smart-compile
           (local-variable-p 'compile-command)
           compile-command)
      (call-interactively 'compile)
      (setq not-yet nil)
      )

     ;; make?
     ((and smart-compile-check-makefile
           (or (file-readable-p "Makefile")
               (file-readable-p "makefile")
               (file-readable-p "../Makefile")
               (file-readable-p "../makefile")
               (file-readable-p "../../Makefile")
               (file-readable-p "../../makefile")))
      (if (y-or-n-p "Makefile is found.  Try 'make'? ")
          (progn
            (cond
             ((or (file-readable-p "../../Makefile")
                  (file-readable-p "../../makefile"))
              (set
               (make-local-variable 'compile-command) "make -C ../.. -k "))
             ((or (file-readable-p "../Makefile")
                  (file-readable-p "../makefile"))
              (set
               (make-local-variable 'compile-command) "make -C .. -k "))
             (t
              (set (make-local-variable 'compile-command) "make -k ")))

            (call-interactively 'compile)
            (setq not-yet nil))
        (setq smart-compile-check-makefile nil)))

     ((and smart-compile-check-gyp
           (or (file-readable-p "binding.gyp")
               (file-readable-p "../binding.gyp")
               (file-readable-p "../../binding.gyp")))
      (if (y-or-n-p "binding.gyp found. Try 'npm build'?")
          (progn
            (cond
             ((file-readable-p "../../binding.gyp")
              (set
               (make-local-variable 'compile-command) "npm build ../.."))
             ((file-readable-p "../binding.gyp")
              (set
               (make-local-variable 'compile-command) "npm build .."))
             (t
              (set (make-local-variable 'compile-command) "npm build .")))
            (call-interactively 'compile)
            (setq not-yet nil))
        (setq smart-compile-check-gyp nil)))

     ((and smart-compile-check-scons-files
           (or (file-readable-p "SConstruct")
               (file-readable-p "../SConstruct")
               (file-readable-p "../../SConstruct"))) ;; if found in parent dir
      (if (y-or-n-p "scons files are found. Try 'scons'? ")
          (progn
            (cond
             ((file-readable-p "../../SConstruct")
              (set
               (make-local-variable
                'compile-command) "scons -C ../.. -k "))
             ((file-readable-p "../SConstruct")
              (set
               (make-local-variable 'compile-command) "scons -C .. -k "))
             (t (set (make-local-variable 'compile-command) "scons -k "))
             )
            (call-interactively 'compile)
            (setq not-yet nil))
        (setq smart-compile-check-scons-files nil)))

     ((and smart-compile-check-cakefile
           (or (file-readable-p "Cakefile")
               (file-readable-p "../Cakefile")
               (file-readable-p "../../Cakefile")))
      (if (y-or-n-p "Cakefile found. Try 'cake'?")
          (progn
            (set
             (make-local-variable
              'compile-command) "cake build ")
            (call-interactively 'compile)
            (setq not-yet nil))
        (setq smart-compile-check-cakefile nil)))

     ) ;; end of (cond ...)

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
    (if not-yet (call-interactively 'compile) )

    ))

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
