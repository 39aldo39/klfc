{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Layout.Pos
    ( Pos(..)
    , posAndXkbKeycode
    ) where

import BasePrelude hiding (Space, Down)
import Prelude.Unicode
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

posAndXkbKeycode ∷ [(Pos, String)]
posAndXkbKeycode =
    [ (Tilde, "TLDE")
    , (Tilde, "AE00")
    , (N1, "AE01")
    , (N2, "AE02")
    , (N3, "AE03")
    , (N4, "AE04")
    , (N5, "AE05")
    , (N6, "AE06")
    , (N7, "AE07")
    , (N8, "AE08")
    , (N9, "AE09")
    , (N0, "AE10")
    , (Minus, "AE11")
    , (Plus, "AE12")
    , (Backspace, "BKSP")

    , (Tab, "TAB")
    , (Q, "AD01")
    , (W, "AD02")
    , (E, "AD03")
    , (R, "AD04")
    , (T, "AD05")
    , (Y, "AD06")
    , (U, "AD07")
    , (I, "AD08")
    , (O, "AD09")
    , (P, "AD10")
    , (Bracket_L, "AD11")
    , (Bracket_R, "AD12")
    , (Backslash, "BKSL")
    , (Backslash, "AC12")

    , (CapsLock, "CAPS")
    , (A, "AC01")
    , (S, "AC02")
    , (D, "AC03")
    , (F, "AC04")
    , (G, "AC05")
    , (H, "AC06")
    , (J, "AC07")
    , (K, "AC08")
    , (L, "AC09")
    , (Semicolon, "AC10")
    , (Apastrophe, "AC11")
    , (Enter, "RTRN")

    , (Shift_L, "LFSH")
    , (Iso, "LSGT")
    , (Iso, "AB00")
    , (Z, "AB01")
    , (X, "AB02")
    , (C, "AB03")
    , (V, "AB04")
    , (B, "AB05")
    , (N, "AB06")
    , (M, "AB07")
    , (Comma, "AB08")
    , (Period, "AB09")
    , (Slash, "AB10")
    , (Shift_R, "RTSH")

    , (Control_L, "LCTL")
    , (Win_L, "LWIN")
    , (Alt_L, "LALT")
    , (Space, "SPCE")
    , (Alt_R, "RALT")
    , (Win_R, "RWIN")
    , (Menu, "MENU")
    , (Control_R, "RCTL")

    , (Esc, "ESC")
    , (F1, "FK01")
    , (F2, "FK02")
    , (F3, "FK03")
    , (F4, "FK04")
    , (F5, "FK05")
    , (F6, "FK06")
    , (F7, "FK07")
    , (F8, "FK08")
    , (F9, "FK09")
    , (F10, "FK10")
    , (F11, "FK11")
    , (F12, "FK12")
    , (PrintScreen, "PRSC")
    , (ScrollLock, "SCLK")
    , (Pause, "PAUS")

    , (Insert, "INS")
    , (Delete, "DELE")
    , (Home, "HOME")
    , (End, "END")
    , (PageUp, "PGUP")
    , (PageDown, "PGDN")
    , (Up, "UP")
    , (Layout.Pos.Left, "LEFT")
    , (Down, "DOWN")
    , (Layout.Pos.Right, "RGHT")

    , (NumLock, "NMLK")
    , (KP_Div, "KPDV")
    , (KP_Mult, "KPMU")
    , (KP_Min, "KPSU")
    , (KP_7, "KP7")
    , (KP_8, "KP8")
    , (KP_9, "KP9")
    , (KP_Plus, "KPAD")
    , (KP_4, "KP4")
    , (KP_5, "KP5")
    , (KP_6, "KP6")
    , (KP_Comma, "I129")
    , (KP_1, "KP1")
    , (KP_2, "KP2")
    , (KP_3, "KP3")
    , (KP_Enter, "KPEN")
    , (KP_0, "KP0")
    , (KP_Dec, "KPDL")
    , (KP_Eq, "KPEQ")

    , (AudioPlay, "I215")
    , (AudioPlay, "I208")
    , (AudioPause, "I209")
    , (PlayPause, "I172")
    , (Previous, "I173")
    , (Next, "I171")
    , (Stop, "I174")
    , (AudioRewind, "I176")
    , (AudioForward, "I216")
    , (Mute, "MUTE")
    , (VolumeDown, "VOL-")
    , (VolumeUp, "VOL+")
    , (Eject, "I169")

    , (Browser_Back, "I166")
    , (Browser_Forward, "I167")
    , (Browser_Refresh, "I181")
    , (Browser_Stop, "STOP")
    , (Browser_Search, "I225")
    , (Browser_Favorites, "I164")

    , (Calculator, "I148")
    , (MediaPlayer, "I234")
    , (Browser, "I180")
    , (Mail, "I163")
    , (Search, "I225")
    , (Explorer, "I152")
    , (WWW, "I158")
    , (MyComputer, "I165")
    , (Help, "HELP")
    , (LaunchA, "I128")
    , (Launch1, "I156")
    , (Launch2, "I157")
    , (LaunchA, "I192")
    , (LaunchC, "I193")
    , (LaunchD, "I194")
    , (LaunchE, "I195")
    , (LaunchF, "I196")
    , (Launch3, "I210")
    , (Launch4, "I211")
    , (LaunchB, "I212")
    , (Launch5, "FK14")
    , (Launch6, "FK15")
    , (Launch7, "FK16")
    , (Launch8, "FK17")
    , (Launch9, "FK18")

    , (Power, "POWR")
    , (Sleep, "I150")
    , (Wake, "I151")
    , (BrightnessDown, "I237")
    , (BrightnessUp, "I238")

    , (F13, "FK13")
    , (F14, "FK14")
    , (F15, "FK15")
    , (F16, "FK16")
    , (F17, "FK17")
    , (F18, "FK18")
    , (F19, "FK19")
    , (F20, "FK20")
    , (F21, "FK21")
    , (F22, "FK22")
    , (F23, "FK23")
    , (F24, "FK24")
--    , (Hash, "")
    , (Ro, "AB11")
    , (Yen, "AE13")
    , (Muhenkan, "MUHE")
    , (Henkan, "HENK")
    , (Katakana, "KATA")
    ]

instance HumanReadable Pos where
    typeName _ = "position"
    stringList = posAndString ⧺ posAndXkbKeycode
instance ToJSON Pos where
    toJSON = hrToJSON
instance FromJSON Pos where
    parseJSON = hrParseJSON
