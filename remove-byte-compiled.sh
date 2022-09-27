#!/bin/bash

set -euxo pipefail

find . \
  \( -name '*.elc' -or -name 'gmon.out' \) \
  -exec rm -v {} '+'

rm -rfv ./eln-cache

