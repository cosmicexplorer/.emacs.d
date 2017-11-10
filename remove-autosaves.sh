#!/bin/bash

set -e

for dir in auto-save-files auto-save-list undo-tree-history backup-files; do
  rm -rf "$dir"
  mkdir "$dir"
  echo -e "*\n!.gitignore" > "$dir/.gitignore"
done

rm -f .emacs.desktop
