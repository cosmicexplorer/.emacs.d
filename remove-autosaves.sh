#!/bin/sh

WORKING_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "$WORKING_DIR"
rm -rf undo-tree-history
rm -rf autosaved-files
rm -rf auto-save-list
