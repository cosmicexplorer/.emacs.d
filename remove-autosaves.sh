#!/bin/bash

set -euxo pipefail

for dir in auto-save-files auto-save-list undo-tree-history backup-files; do
  find "$dir" -not -name '.gitignore' -type f -exec rm {} '+'
done

rm -f .emacs.desktop
