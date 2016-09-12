#!/bin/sh
set -eu

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

add_extra_include () {
  file="$1"
  layout_name="$2"

  layout_include="    include \"$layout_name\""

  if [ ! -e "$file" ]; then
    echo "Warning: $file does not exist"
    return
  fi

  if grep -q "$layout_include" "$file"; then
    echo "$file already includes $layout_name"
    return
  fi

  first=$(head -n -1 "$file")
  last=$(tail -n 1 "$file")

  if [ "$last" = "};" ]; then
    mv "$file" "$file.bak"
    (echo "$first"; echo "$layout_include"; echo "$last") > "$file"
  else
    echo "Warning: could not add extra include to $file since it does not end on '};'"
    return
  fi
}

copy_xkb_file () {
  file_from="$1"
  file_to="$2"

  if [ ! -e "$file_to" ] || confirm "$file_to already exists. Overwrite?"; then
    cp "$file_from" "$file_to"
  fi
}

xkb_dir_from=$(dirname "$0")
xkb_dir_to="/usr/share/X11/xkb"
layout="colemak"

OPTIND=1

while getopts "i:o:l:" opt; do
  case "$opt" in
    i) xkb_dir_from="$OPTARG";;
    o) xkb_dir_to="$OPTARG";;
    l) layout="$OPTARG";;
    *) exit 1;;
  esac
done

if [ -z "$layout" ]; then
  echo "Empty layout"
  exit 2
fi

copy_xkb_file "$xkb_dir_from/symbols/$layout" "$xkb_dir_to/symbols/$layout"
copy_xkb_file "$xkb_dir_from/types/$layout" "$xkb_dir_to/types/$layout"
copy_xkb_file "$xkb_dir_from/keycodes/$layout" "$xkb_dir_to/keycodes/$layout"
#copy_xkb_file "$xkb_dir_from/compat/$layout" "$xkb_dir_to/compat/$layout"
add_extra_include "$xkb_dir_to/types/complete" "$layout"
#add_extra_include "$xkb_dir_to/compat/complete" "$layout"
