#!/bin/bash

set -euxo pipefail

find . \
  -not -path '*elpa*' \
  -and \( -name '*.elc' -or -name 'gmon.out' \) \
  -exec rm {} '+'

