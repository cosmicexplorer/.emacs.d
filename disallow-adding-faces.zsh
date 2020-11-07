#!/bin/zsh

set -euxo pipefail

function get-added-faces-heuristic {
  git diff HEAD \
    | sed -Ene "s#^\\+ '\\(([^s]+) \\(\\(t \\(.*\$#\\0#gp"
}

get-added-faces-heuristic
get-added-faces-heuristic \
  | wc -l \
  | if [[ "$(cat)" -ne 0 ]]; then
  exit 1
fi
