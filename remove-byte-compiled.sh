#!/bin/bash

set -euxo pipefail

# This script removes compiled output *only for the scripts loaded in
# .emacs.d!* The emacs build itself will byte/native-compile the lisp
# code in emacs itself.

# Remove byte-compiled output.
find . \
  \( -name '*.elc' -or -name 'gmon.out' \) \
  -exec rm -v {} '+'

# Remove native-compiled output.
# NB: this shouldn't be necessary, as the eln cache is already segregated by
#     version, but currently when different emacs versions are used, they will
#     attempt to use native code from prior versions, which causes init to fail
#     with unrecognized function names e.g. `defvar-1'. Very strange.
rm -rfv ./eln-cache

