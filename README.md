Keyboard Layout Files Creator
=============================

Keyboard Layout Files Creator is a tool to create advanced keyboard layouts in various formats.
Currently, exporting to XKB, PKL, KLC, keylayout, TMK and AHK is supported.
Layouts are stored in JSON files, of which examples can be found in the [examples](examples) folder.
The full format is documented in [doc/layout.md](doc/layout.md).
It is also possible to import an existing keyboard layout from a XKB, PKL or KLC file.

Example
-------

To get the same output as in the [examples/output](examples/output) folder, execute
```
path/to/klfc colemak.json altgr_colemak.json extend.json -o output
```
inside the examples folder.

Installation and building
-------------------------

The easiest way to get KLFC is to download a pre-built binary from the [releases page](https://github.com/39aldo39/klfc/releases).

KLFC can also be built from source.
It is written in Haskell and can be installed with the [Haskell toolchain](https://www.haskell.org/downloads).
With the Haskell toolchain installed, KLFC is built by executing `cabal install` inside the root directory of this project.
This will create a binary `klfc` in `~/.cabal/bin/`.

Usage
-----

```
klfc [IMPORT TYPE] FILE... [OUTPUTS] [OPTIONS]
```

### Available options ###
```
-h,--help                Show this help text
--version                Show version
```

#### Import types ####
```
--from-json              Read from a JSON file
--from-xkb               Import from a XKB symbols file. To read a variant,
                         append it in parenthesis (e.g. to read the Colemak
                         variant of the us symbols file, use "us(colemak)").
--from-pkl               Import from a PKL layout file
--from-klc               Import from a KLC file
```

#### Import files ####
```
FILE...                  Files to read (‘-’ for stdin). If multiple files are
                         read, the corresponding layouts will be put together.
                         This is useful when a file only specifies a part of
                         the layout (e.g. only the letters at a few
                         shiftstates).
```

#### Output files ####
```
--json FILE              Save to a JSON file (‘-’ for stdout)
--xkb DIRECTORY          Export to a XKB directory
--pkl DIRECTORY          Export to a PKL directory
--klc DIRECTORY          Export to a KLC directory (‘-’ for printing the base
                         layout to stdout)
--keylayout DIRECTORY    Export to a keylayout directory (‘-’ for printing the
                         base layout to stdout)
--tmk DIRECTORY          Export to a TMK directory (‘-’ for printing the base
                         layout to stdout)
--ahk DIRECTORY          Export to a AHK directory (‘-’ for printing the base
                         layout to stdout)
-o,--output DIRECTORY    Export to all file types
```

#### Extra Options ####
```
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
```
##### KLC #####
```
--klc-chained-deads      Use chained dead keys in KLC. This requires
                         alternative compilation, see
                         <http://archives.miloush.net/michkap/archive/2011/04/16/10154700.html>.
```
##### PKL #####
```
--pkl-compact            Set PKL to compact mode
```
##### XKB #####
```
--xkb-custom-shortcuts   Use the shortcut positions from the ‘shortcutPos’
                         attributes for shortcuts in XKB
--xkb-redirect-all       Always use the ‘redirect’ action in XKB, if possible.
                         This may help some programs detect special actions on
                         different layers.
--xkb-redirect-clears-extend
                         Clear the extend modifier (LevelFive) in redirect
                         actions. This may help some programs detect special
                         actions on the extend layer.
```
##### Keylayout #####
```
--keylayout-custom-shortcuts
                         Use the shortcut positions from the ‘shortcutPos’
                         attributes for shortcuts in keylayout
```
