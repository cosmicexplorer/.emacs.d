#!/bin/zsh

set -euxo pipefail

function die {
  local -r exit_code="$1"
  printf '%s\n' >&2 "Failed with code $exit_code:" "${@:2}"
  exit "$exit_code"
}

function diff-staged-files {
  git diff HEAD
}

# NB: dead code
function get-added-faces-heuristic {
  diff-staged-files \
    | grep -E "^\+ '\\(([^s]+) \\(\\(t \\(.*\$"
}

function filter-out-selected-packages {
  grep -vF 'package-selected-packages'
}

function any-added-content-heuristic {
  diff-staged-files \
    | grep -E "^\+ '\\(" \
    | filter-out-selected-packages
}

function is-empty-array {
  [[ "$#" -eq 1 && "$1" == '' ]]
}

local -a all_results=( "$(any-added-content-heuristic)" )
if is-empty-array "${all_results[@]}"; then
  all_results=( )
fi

if [[ "${#all_results[@]}" -ne 0 ]] ; then
  die "${#all_results[@]}" \
      "${#all_results[@]} faces or variables were added:" \
      "${all_results[@]}"
else
  exit 0
fi
