{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Layout.Pos
    ( Pos(..)
    ) where

import BasePrelude hiding (Space, Down)
import Util (HumanReadable(..))

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

data Pos
    = Tilde | N1 | N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N0 | Minus | Plus | Backspace
    | Tab | Q | W | E | R | T | Y | U | I | O | P | Bracket_L | Bracket_R | Backslash
    | CapsLock | A | S | D | F | G | H | J | K | L | Semicolon | Apastrophe | Enter
    | Shift_L | Iso | Z | X | C | V | B | N | M | Comma | Period | Slash | Shift_R
    | Control_L | Win_L | Alt_L | Space | Alt_R | Win_R | Menu | Control_R

    | Esc | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12 | PrintScreen | ScrollLock | Pause
    | Insert | Delete | Home | End | PageUp | PageDown | Up | Left | Down | Right

    | NumLock | KP_Div | KP_Mult | KP_Min
    | KP_7    | KP_8   | KP_9    | KP_Plus
    | KP_4    | KP_5   | KP_6    | KP_Comma
    | KP_1    | KP_2   | KP_3    | KP_Enter
    |       KP_0       | KP_Dec  | KP_Eq

    | AudioPlay | AudioPause | PlayPause | Previous | Next | Stop | ToggleRepeat | ToggleRandom
    | AudioRewind | AudioForward
    | Mute | VolumeDown | VolumeUp
    | Eject

    | Browser_Back | Browser_Forward | Browser_Refresh | Browser_Stop | Browser_Search | Browser_Favorites

    | Calculator | MediaPlayer | Browser | Mail | Search | Explorer | WWW | MyComputer | Help
    | Launch0 | Launch1 | Launch2 | Launch3 | Launch4 | Launch5 | Launch6 | Launch7
    | Launch8 | Launch9 | LaunchA | LaunchB | LaunchC | LaunchD | LaunchE | LaunchF

    | Power | Sleep | Wake | BrightnessDown | BrightnessUp

    | F13 | F14 | F15 | F16 | F17 | F18 | F19 | F20 | F21 | F22 | F23 | F24
    | Hash | Ro | Yen | Muhenkan | Henkan | Katakana

    deriving (Eq, Ord, Show, Read, Enum, Bounded)

posAndString ∷ [(Pos, String)]
posAndString =
    -- Number row
    [ (Tilde, "~")
    , (Tilde, "`")
    , (Tilde, "Tilde")
    , (N1, "1")
    , (N2, "2")
    , (N3, "3")
    , (N4, "4")
    , (N5, "5")
    , (N6, "6")
    , (N7, "7")
    , (N8, "8")
    , (N9, "9")
    , (N0, "0")
    , (Minus, "-")
    , (Minus, "Minus")
    , (Plus, "+")
    , (Plus, "=")
    , (Plus, "Plus")
    , (Backspace, "Backspace")

    -- Top row
    , (Tab, "Tab")
    , (Q, "Q")
    , (W, "W")
    , (E, "E")
    , (R, "R")
    , (T, "T")
    , (Y, "Y")
    , (U, "U")
    , (I, "I")
    , (O, "O")
    , (P, "P")
    , (Bracket_L, "[")
    , (Bracket_R, "]")
    , (Backslash, "\\")

    -- Home row
    , (CapsLock, "CapsLock")
    , (A, "A")
    , (S, "S")
    , (D, "D")
    , (F, "F")
    , (G, "G")
    , (H, "H")
    , (J, "J")
    , (K, "K")
    , (L, "L")
    , (Semicolon, ";")
    , (Apastrophe, "'")
    , (Enter, "Enter")

    -- Buttom row
    , (Shift_L, "Shift_L")
    , (Iso, "Iso")
    , (Z, "Z")
    , (X, "X")
    , (C, "C")
    , (V, "V")
    , (B, "B")
    , (N, "N")
    , (M, "M")
    , (Comma, ",")
    , (Period, ".")
    , (Slash, "/")
    , (Shift_R, "Shift_R")

    -- Space row
    , (Control_L, "Control_L")
    , (Win_L, "Win_L")
    , (Alt_L, "Alt_L")
    , (Space, "Space")
    , (Alt_R, "Alt_R")
    , (Win_R, "Win_R")
    , (Menu, "Menu")
    , (Control_R, "Control_R")

    -- Function row
    , (Esc, "Escape")
    , (Esc, "Esc")
    , (F1, "F1")
    , (F2, "F2")
    , (F3, "F3")
    , (F4, "F4")
    , (F5, "F5")
    , (F6, "F6")
    , (F7, "F7")
    , (F8, "F8")
    , (F9, "F9")
    , (F10, "F10")
    , (F11, "F11")
    , (F12, "F12")
    , (PrintScreen, "PrintScreen")
    , (ScrollLock, "ScrollLock")
    , (Pause, "Pause")

    -- Movement
    , (Insert, "Insert")
    , (Delete, "Delete")
    , (Home, "Home")
    , (End, "End")
    , (PageUp, "PageUp")
    , (PageDown, "PageDown")
    , (Up, "Up")
    , (Layout.Pos.Left, "Left")
    , (Down, "Down")
    , (Layout.Pos.Right, "Right")

    -- Numpad
    , (NumLock, "NumLock")
    , (KP_Div, "KP_Div")
    , (KP_Mult, "KP_Mult")
    , (KP_Min, "KP_Min")
    , (KP_7, "KP_7")
    , (KP_8, "KP_8")
    , (KP_9, "KP_9")
    , (KP_Plus, "KP_Plus")
    , (KP_4, "KP_4")
    , (KP_5, "KP_5")
    , (KP_6, "KP_6")
    , (KP_Comma, "KP_Comma")
    , (KP_1, "KP_1")
    , (KP_2, "KP_2")
    , (KP_3, "KP_3")
    , (KP_Enter, "KP_Enter")
    , (KP_0, "KP_0")
    , (KP_Dec, "KP_Dec")
    , (KP_Eq, "KP_Eq")

    -- Miscellaneous keys

    -- Media control
    , (AudioPlay, "AudioPlay")
    , (AudioPause, "AudioPause")
    , (PlayPause, "PlayPause")
    , (Previous, "Previous")
    , (Next, "Next")
    , (Stop, "Stop")
    , (ToggleRepeat, "ToggleRepeat")
    , (ToggleRandom, "ToggleRandom")
    , (AudioRewind, "AudioRewind")
    , (AudioForward, "AudioForward")
    , (Mute, "Mute")
    , (VolumeDown, "VolumeDown")
    , (VolumeUp, "VolumeUp")
    , (Eject, "Eject")

    -- Browser control
    , (Browser_Back, "Browser_Back")
    , (Browser_Forward, "Browser_Forward")
    , (Browser_Refresh, "Browser_Refresh")
    , (Browser_Stop, "Browser_Stop")
    , (Browser_Search, "Browser_Search")
    , (Browser_Favorites, "Browser_Favorites")

    -- Applications
    , (Calculator, "Calculator")
    , (MediaPlayer, "MediaPlayer")
    , (Browser, "Browser")
    , (Mail, "Mail")
    , (Search, "Search")
    , (Explorer, "Explorer")
    , (WWW, "WWW")
    , (MyComputer, "MyComputer")
    , (Help, "Help")
    , (Launch0, "Launch0")
    , (Launch1, "Launch1")
    , (Launch2, "Launch2")
    , (Launch3, "Launch3")
    , (Launch4, "Launch4")
    , (Launch5, "Launch5")
    , (Launch6, "Launch6")
    , (Launch7, "Launch7")
    , (Launch8, "Launch8")
    , (Launch9, "Launch9")
    , (LaunchA, "LaunchA")
    , (LaunchB, "LaunchB")
    , (LaunchC, "LaunchC")
    , (LaunchD, "LaunchD")
    , (LaunchE, "LaunchE")
    , (LaunchF, "LaunchF")

    -- Power management
    , (Power, "Power")
    , (Sleep, "Sleep")
    , (Wake, "Wake")
    , (BrightnessDown, "BrightnessDown")
    , (BrightnessUp, "BrightnessUp")

    -- Extra function keys
    , (F13, "F13")
    , (F14, "F14")
    , (F15, "F15")
    , (F16, "F16")
    , (F17, "F17")
    , (F18, "F18")
    , (F19, "F19")
    , (F20, "F20")
    , (F21, "F21")
    , (F22, "F22")
    , (F23, "F23")
    , (F24, "F24")

    -- International keys
    , (Hash, "Hash")
    , (Ro, "Ro")
    , (Yen, "Yen")
    , (Muhenkan, "Muhenkan")
    , (Muhenkan, "Mhen")
    , (Henkan, "Henkan")
    , (Henkan, "Henk")
    , (Katakana, "Katakana")
    , (Katakana, "Kana")
    ]

instance HumanReadable Pos where
    typeName _ = "position"
    stringList = posAndString
instance ToJSON Pos where
    toJSON = hrToJSON
instance FromJSON Pos where
    parseJSON = hrParseJSON
