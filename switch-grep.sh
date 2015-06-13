#!/bin/bash

# switches the locations of the last two grep arguments, and adds a . if wanted



numNonOption=0
for arg in $@; do
  if [ "$(echo $arg | cut -b1)" != "-" ]; then
    ((++numNonOption))
  fi
done

useColorArg=$(grep --color 2>&1 | grep -- --color >/dev/null && \
                echo "" || echo "--color")

cmd="grep -rnH --binary-files=without-match $useColorArg"
if [ $numNonOption -gt 1 ]; then
  $cmd $@
else
  $cmd $@ .
fi
