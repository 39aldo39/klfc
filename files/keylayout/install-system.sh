#!/bin/sh
set -eu

dir_from=$(dirname "$0")
dir_to="/Library/Keyboard Layouts"
layout=""

OPTIND=1

while getopts "d:i:l:" opt; do
  case "$opt" in
    d|i) dir_from="$OPTARG";;
    o) dir_to="$OPTARG";;
    l) layout="$OPTARG";;
    *) exit 1;;
  esac
done

confirm () {
  # call with a prompt string or use a default
  printf "%s [y/N] " "${1:-Are you sure?}"
  read -r response
  case "${response:-${2:-}}" in
    [yY][eE][sS]|[yY]) true;;
    [nN][oO]|[nN]) false;;
    *) confirm "${1:-}" "${2:-}";;
  esac
}

copy_file () {
  file_from=$1
  file_to=$2

  if [ ! -e "$file_to" ] || confirm "$file_to already exists. Overwrite?"; then
    cp "$file_from" "$file_to"
  fi
}

copy_file "$dir_from/$layout"*.keylayout "$dir_to"
