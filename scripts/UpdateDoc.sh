#!/bin/sh
set -eu

dir=$(dirname "$0")
root="$dir/.."

# Add usage to README
preUsage=$(grep -B1000 "^-----$" "$root/README.md")
echo "$preUsage" > "$root/README.md"
"$root/dist/build/klfc/klfc" -h | "$dir/Usage.hs" >> "$root/README.md"

# Make new action list
cat "$root/src/Layout/Action.hs" | "$dir/ActionToMd.hs" > "$root/doc/lists/actions.md"

# Make new position list
cat "$root/src/Layout/Pos.hs" | "$dir/PosToMd.hs" > "$root/doc/lists/positions.md"
