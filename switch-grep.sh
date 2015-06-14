#!/bin/bash

# switches the locations of the last two grep arguments, and uses colorized
# output if allowed
useColorArg=$(grep --color 2>&1 | grep -- --color >/dev/null && \
                echo "" || echo "--color")
grep -rnH --binary-files=without-match $useColorArg $@ .
