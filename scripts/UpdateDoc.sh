#!/bin/sh
set -eu

dir=$(dirname "$0")
root="$dir/.."

# Add usage to README
preUsage=$(grep -B1000 "^-----$" "$root/README.md")
echo "$preUsage" > "$root/README.md"
"$root/dist/build/klfc/klfc" -h | "$dir/Usage.hs" >> "$root/README.md"

# Make new action list
"$dir/ActionToMd.hs" < "$root/src/Layout/Action.hs" > "$root/doc/lists/actions.md"

# Make new position list
"$dir/PosToMd.hs" < "$root/src/Layout/Pos.hs" > "$root/doc/lists/positions.md"
