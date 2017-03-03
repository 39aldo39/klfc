Layout
======

The JSON file denotes a (part of a) keyboard layout. The file has the following attributes:

* Miscellaneous information.
* QWERTY shortcuts (`qwertyShortcuts`).
* Filter (`filter`).
* Shiftlevels (`shiftlevels`).
* Keys (`keys`).
* Singleton keys (`singletonKeys`).
* Custom dead keys (`customDeadKeys`).
* Mods (`mods`).

It is possible to split a layout definition into multiple files. For example, a layout can be split into a `base.json` and a `altgr.json` file, where the `base.json` file contains the letters that are active when nothing or Shift is pressed and the `altgr.json` file the letters that are active when AltGr is pressed. This can make the layout more clear and modular. For example, the AltGr layer can easily be swapped for another one or can't be activated for certain output formats.

Miscellaneous information
-------------------------

Miscellaneous information about the keyboard layout. Most keyboard layout formats use some of them. The attributes are used at the top level.

* Full name of the layout (`fullName`), used when a human readable name is required.
* Short name of the layout (`name`), used when a short name of the layout is required, for example in file names.
* Copyright (`copyright`).
* Company (`company`).
* Locale ID (`localeId`).
* Version (`version`).
* Description (`description`).

QWERTY shortcuts
----------------

When the `qwertyShortcuts` attribute is set to `true`, all shortcut positions of the keys will be set to their QWERTY position.

By default, `qwertyShortcuts` will be `false`.

Filter
------

The `filter` attribute causes the current layout file to only be active at certain outputs. This is useful when a few outputs support a feature, and the absence of that feature at the other outputs causes undesired behavior.

A filter starts with `only` or `no`, followed by a list of outputs, separated with a comma or a space, for example `no KLC,PKL` A filter of the form `only outputs` causes the layout to only be active for the outputs in `outputs`. A filter of the form `no outputs` causes the layout to only be active for the outputs not in `outputs`. The supported outputs are `JSON`, `XKB`, `PKL`, `KLC` and `keylayout`.

Shiftlevels
-----------

The `shiftlevels` attribute determines which layer is activated when modifiers are active. It is a list of multiple modifiers, where the nth layer will be active if the nth modifiers are pressed. For example, `[ "None", "Shift", "AltGr", "Shift+AltGr" ]` will trigger the first layer when no modifiers are active, the second layer when shift is pressed, etc. Each element in the list are multiple modifiers separated with `+`, where `None` can be used to denote the absence of modifiers. It is also possible to separate multiple possibilities with z `|`. For example, `[ "None", "NumLock|Shift+AltGr" ]` will trigger the second layer when Num Lock is pressed or when shift and AltGr are both pressed.

The supported modifiers are:

* `Shift`
* `CapsLock`
* `Win`
* `Alt`
* `Control`
* `NumLock`
* `AltGr`
* `Extend`

For the Shift, Win, Alt and Control modifiers, also 'left' and 'right' versions are supported. These are denoted by appending a `_L` or `_R` (for example, `Shift_L` and `Shift_R`). When such modifier is used, only the modifier at the right side will activate the specified layer.

For the outputs for macOS, the Win modifier is equivalent to the Command key.

While Caps Lock is supported, it is rarely used, since the `capslock` attribute at keys is more useful and better supported.

Keys
----

The main part of the layout. The `keys` attribute contains a list of keys, which determine what happens when the keys on your keyboard are pressed. It is documented at [key.md](key.md).

Singleton keys
--------------

The `singletonKeys` attribute takes a list of tuples of a position and a letter, which will assign the letter to the position. This can be used for keys which always do the same, independent from the modifiers. The singleton key `[ "POS", "LETTER" ]` is roughly equivalent to the key

    {
        "pos": "POS",
        "letters": [ "LETTER" ],
        "shiftlevels": [ "None" ]
    }

But singleton keys are more useful, since the output formats handle them better than normal keys.

If multiple singleton keys are defined on the same position, only the last one is used.

Since JSON does not support tuples, a list of length 2 is used. For example, to let the Caps Lock key behave as extended modifier, use the 'tuple' `[ "CapsLock", "Extend" ]`.

Custom dead keys
----------------

The `customDeadKeys` attribute takes a list of custom dead keys. Each custom dead key is an object with a `name`, `baseChar` and `stringMap` attribute. The `name` is a string with the name of the dead key. The `baseChar` is a character (string of length one) which is the character of the dead key on his own. If no base character is applicable, the attribute can be omitted and a Unicode character from the private use area is used. The `stringMap` is a list of tuples, where the first component denotes the string which will be transformed by the dead key and the second component denotes the resulting string. Since JSON does not support tuples, a list of length 2 is used. For example, to put an `´` on an `e`, use the 'tuple' `[ "e", "é" ]`.

Mods
----

The `mods` attribute takes a list of so-called mods. A 'mod' is a permutation in the positions of the keys. For example, the 'Wide' mod moves all the keys of the right hand one position to the right, so that the right shift and enter key are easier to hit on a standard keyboard. Each mod is a JSON object with a `name` and a `permutation`. The `name` is a string with the name of the mod. The `permutation` is the permutation which the mod makes and is either an object with the original positions as keys and the new positions as values, or a list of permutation cycles (as described [here](https://en.wikipedia.org/wiki/Permutation#Cycle_notation)).

For example, the 'Wide' mod can be represented as

    {
        "name": "Wide",
        "permutation": [
            [ "Y", "U", "I", "O", "P", "[" ],
            [ "H", "J", "K", "L", ";", "'" ],
            [ "N", "M", ",", ".", "/", "]" ],
            [ "]", "'", "\\" ]
        ]
    }

or

    {
        "name": "Wide",
        "permutation": {
            "Y": "U",
            "U": "I",
            "I": "O",
            "O": "P",
            "P": "[",
            "[": "Y",
            "H": "J",
            "J": "K",
            "K", "L",
            "L", ";",
            ";", "'",
            "'", "\\",
            "\\", "N",
            "N", "M",
            "M", ",",
            ",", ".",
            ".", "/",
            "/", "]",
            "]", "H"
        }
    }
