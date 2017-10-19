#!/bin/bash

# Indent a SAS source file
# Uses emacs in batch mode

SASFILE=$1

emacs \
    -l "~/projects/sass-mode/sass-output-mode.el" \
    -l "~/projects/sass-mode/sass-indentation.el" \
    -l "~/projects/sass-mode/sass-mode.el" \
    -batch $1 \
    --eval "(let ((comment-start \"/*\") (comment-end   \"*/\"))
    (indent-region (point-min) (point-max))
    (save-buffer))" 2> /dev/null
