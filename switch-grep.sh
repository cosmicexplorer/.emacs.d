#!/bin/bash

# get last arg
for last; do true; done

# switches the locations of the last two grep arguments, and uses colorized
# output if allowed
useColorArg=$(grep --color 2>&1 | grep -- --color >/dev/null && \
                echo "" || echo "--color")
cmd="grep -rnH --binary-files=without-match $useColorArg"
if [ -d "$last" ]; then
  $cmd "${@}"
else
  $cmd "${@}" .
fi
