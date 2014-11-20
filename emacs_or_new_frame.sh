#!/bin/bash

if [ "$(pgrep emacs)" = "" ]; then
    emacs -l ~/.emacs.d/.emacs "$1"
else
    emacsclient -c "$1"
fi
