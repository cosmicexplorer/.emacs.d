#!/bin/bash

if [ "$(pgrep emacs)" = "" ]; then
    emacs -l ~/.emacs.d/.emacs "$1"
else
    emacsclient -n "$1"
fi
