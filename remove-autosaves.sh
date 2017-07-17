#!/bin/sh

if [[ ! -d ./auto-save-files && \
    -d ./auto-save-list && \
    -d ./undo-tree-history ]]; then
  echo "not in emacs init directory (pwd=$(pwd))" >&2
  exit 1
fi

find auto-save-files -name "!*" -exec rm -v '{}' '+'
find auto-save-list -name ".saves*" -exec rm -v '{}' '+'
find undo-tree-history -name ".!*" -exec rm -v '{}' '+'

