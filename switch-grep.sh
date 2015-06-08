#!/bin/bash

# switches the locations of the last two grep arguments

grep -rnPH --binary-files=without-match "$@"
