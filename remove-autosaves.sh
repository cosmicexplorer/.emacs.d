#!/bin/bash

set -euo pipefail

dir='(expand-file-name user-emacs-directory)'
sexp="(princ (format \"%s\\n\" $dir))"
emacs_dir="$(emacs --batch --eval "$sexp")"
cd "$emacs_dir"

git_dir="$(git rev-parse --git-dir)"
cd "$(dirname "$git_dir")"

git ls-files -o --exclude-standard | parallel rm
