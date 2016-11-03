#!/bin/sh

\emacs -Q --batch --eval='(byte-recompile-directory (expand-file-name "~/.emacs.d/") 0)' &&
    \emacs
