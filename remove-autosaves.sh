#!/bin/bash

set -euxo pipefail

find undo-tree-history/ \
     -not -name '.gitignore' \
     -type f \
     -exec rm -v {} '+'

./drop-opened-buffers.sh
