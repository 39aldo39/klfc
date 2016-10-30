Keyboard Layout Files Creator
=============================

Keyboard Layout Files Creator is a tool to create keyboard layout files in various formats. Currently, exporting to XKB, PKL and KLC is supported. The layout is stored in a JSON file, which format is documented in [doc/layout.md](doc/layout.md), and can be imported from a XKB, PKL or KLC file.

Note on Windows
---------------

On Windows, a UTF-8 codepage is required to be able to output non-ASCII characters. This is done by executing `chcp 65001` in the command line.

Example
-------

To get the same output as in the [examples/output](examples/output) folder, execute

    path/to/klfc colemak.json altgr.json extend.json -o output

inside the examples folder.

Installation and building
-------------------------

The easiest way to get KLFC is to download a pre-built binary from the [releases page](https://github.com/39aldo39/klfc/releases).

KLFC can also be built from source. It is written in Haskell and can be installed with cabal-install. Cabal-Install can be installed by the instructions listed [here](https://wiki.haskell.org/Cabal-Install#Installation), or from the repositories of your distro, if you use Linux. With cabal-install installed, KLFC is built by executing `cabal install` inside the root directory of the source, which will create a binary `klfc` in `~/.cabal/bin/`.

Usage
-----

    klfc [IMPORT TYPE] FILE... [OUTPUTS] [OPTIONS]

### Available options ###

    -h,--help                Show this help text

#### Import types ####
    --from-json              Read from a JSON file
    --from-xkb               Import from a XKB symbols file
    --from-pkl               Import from a PKL layout file
    --from-klc               Import from a KLC file

#### Import files ####
    FILE...                  Files to read (‘-’ for stdin). If multiple files are
                             read, the corresponding layouts will be put together.
                             This is useful when a file only specifies a part of
                             the layout (e.g. only the letters at a few
                             shiftstates).

#### Output files ####
    --json FILE              Save to a JSON file (‘-’ for stdout)
    --xkb DIRECTORY          Export to a XKB directory
    --pkl DIRECTORY          Export to a PKL directory
    --klc DIRECTORY          Export to a KLC directory (‘-’ for printing the base
                             layout to stdout)
    -o,--output DIRECTORY    Export to all file types

#### Extra Options ####
    --remove-shiftstates INDEX
                             Remove one or more shiftstates with their letters.
                             The shiftstates are identified with their index
                             (starting with 0). Multiple indices are seperated
                             with a comma.
    --remove-empty-letters   Remove empty letters at the end of each key
    --combine-mods           Combine all the mods in the layout. For example, if
                             the layout has the mods ‘Wide’ and ‘Angle’, a new mod
                             ‘WideAngle’ will be created.
    --unify-shiftstates      Change the shiftstates of all keys such that all keys
                             have the same shiftstates
##### PKL #####
    --pkl-compact            Set PKL to compact mode
##### XKB #####
    --xkb-custom-shortcut-positions
                             Use the shortcut positions from the ‘shortcutPos’
                             attributes for shortcuts in XKB
    --xkb-redirect-all       Always use the ‘redirect’ action in XKB, if possible.
                             This may help some programs detect special actions on
                             different layers.
    --xkb-redirect-if-extend Always use the ‘redirect’ action in XKB if the extend
                             modifier (LevelFive) is active, if possible. This may
                             help some programs detect special actions on the
                             extend layer.
