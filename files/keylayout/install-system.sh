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

cp "$dir_from/$layout"*.keylayout "$dir_to"
