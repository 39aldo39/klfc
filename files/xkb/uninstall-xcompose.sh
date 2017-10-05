#!/bin/sh
set -eu

dir=$(dirname "$0")
layout=""
layout=${1:-$layout}
xcompose_file=${XCOMPOSEFILE:-$HOME/.XCompose}
include_klfc="include \"%H/.XCompose-klfc-$layout\""

if [ -f "$xcompose_file" ]; then
  { grep -Fxv "$include_klfc" "$xcompose_file" || [ "$?" = 1 ]; } > "$xcompose_file.tmp"
  mv "$xcompose_file.tmp" "$xcompose_file"
fi

rm "$HOME/.XCompose-klfc-$layout"
