{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Lookup.Tmk where

import BasePrelude
import Prelude.Unicode

import Data.Set (Set)
import qualified Data.Set as S

import qualified Layout.Action as A
import qualified Layout.Modifier as M
import qualified Layout.Pos as P
import Layout.Types

posAndTmkAction ∷ [(Pos, String)]
posAndTmkAction =
    [ (P.Tilde, "GRV")
    , (P.Tilde, "GRAVE")
    , (P.Tilde, "ZKHK")
    , (P.N1, "1")
    , (P.N2, "2")
    , (P.N3, "3")
    , (P.N4, "4")
    , (P.N5, "5")
    , (P.N6, "6")
    , (P.N7, "7")
    , (P.N8, "8")
    , (P.N9, "9")
    , (P.N0, "0")
    , (P.Minus, "MINS")
    , (P.Minus, "MINUS")
    , (P.Plus, "EQL")
    , (P.Plus, "EQUAL")
    , (P.Backspace, "BSPC")
    , (P.Backspace, "BSPACE")

    , (P.Tab, "TAB")
    , (P.Q, "Q")
    , (P.W, "W")
    , (P.E, "E")
    , (P.R, "R")
    , (P.T, "T")
    , (P.Y, "Y")
    , (P.U, "U")
    , (P.I, "I")
    , (P.O, "O")
    , (P.P, "P")
    , (P.Bracket_L, "LBRC")
    , (P.Bracket_L, "LBRACKET")
    , (P.Bracket_R, "RBRC")
    , (P.Bracket_R, "RBRACKET")
    , (P.Backslash, "BSLS")
    , (P.Backslash, "BSLASH")

    , (P.CapsLock, "CAPS")
    , (P.CapsLock, "CAPSLOCK")
    , (P.CapsLock, "CLCK")
    , (P.A, "A")
    , (P.S, "S")
    , (P.D, "D")
    , (P.F, "F")
    , (P.G, "G")
    , (P.H, "H")
    , (P.J, "J")
    , (P.K, "K")
    , (P.L, "L")
    , (P.Semicolon, "SCLN")
    , (P.Semicolon, "SCOLON")
    , (P.Apastrophe, "QUOT")
    , (P.Apastrophe, "QUOTE")
    , (P.Enter, "ENT")
    , (P.Enter, "ENTER")

    , (P.Shift_L, "LSFT")
    , (P.Shift_L, "LSHIFT")
    , (P.Iso, "NUBS")
    , (P.Iso, "NONUS_BSLASH")
    , (P.Z, "Z")
    , (P.X, "X")
    , (P.C, "C")
    , (P.V, "V")
    , (P.B, "B")
    , (P.N, "N")
    , (P.M, "M")
    , (P.Comma, "COMM")
    , (P.Comma, "COMMA")
    , (P.Period, "DOT")
    , (P.Slash, "SLSH")
    , (P.Slash, "SLASH")
    , (P.Shift_R, "RSFT")
    , (P.Shift_R, "RSHIFT")

    , (P.Control_L, "LCTL")
    , (P.Control_L, "LCTRL")
    , (P.Win_L, "LGUI")
    , (P.Alt_L, "LALT")
    , (P.Space, "SPC")
    , (P.Space, "SPACE")
    , (P.Alt_R, "RALT")
    , (P.Win_R, "RGUI")
--    , (P.Menu, "MENU")
    , (P.Menu, "APP")
    , (P.Menu, "APPLICATION")
    , (P.Control_R, "RCTL")
    , (P.Control_R, "RCTRL")

    , (P.Esc, "ESC")
    , (P.Esc, "ESCAPE")
    , (P.F1, "F1")
    , (P.F2, "F2")
    , (P.F3, "F3")
    , (P.F4, "F4")
    , (P.F5, "F5")
    , (P.F6, "F6")
    , (P.F7, "F7")
    , (P.F8, "F8")
    , (P.F9, "F9")
    , (P.F10, "F10")
    , (P.F11, "F11")
    , (P.F12, "F12")
    , (P.PrintScreen, "PSCR")
    , (P.PrintScreen, "PSCREEN")
    , (P.ScrollLock, "SLCK")
    , (P.ScrollLock, "SCROLLLOCK")
    , (P.Pause, "PAUS")
    , (P.Pause, "PAUSE")
    , (P.Pause, "BRK")

    , (P.Insert, "INS")
    , (P.Insert, "INSERT")
    , (P.Delete, "DEL")
    , (P.Delete, "DELETE")
    , (P.Home, "HOME")
    , (P.End, "END")
    , (P.PageUp, "PGUP")
    , (P.PageDown, "PGDN")
    , (P.PageDown, "PGDOWN")
    , (P.Up, "UP")
    , (P.Left, "LEFT")
    , (P.Down, "DOWN")
    , (P.Right, "RGHT")
    , (P.Right, "RIGHT")

    , (P.NumLock, "NLCK")
    , (P.NumLock, "NUMLOCK")
    , (P.KP_Div, "PSLS")
    , (P.KP_Div, "KP_SLASH")
    , (P.KP_Mult, "PAST")
    , (P.KP_Mult, "KP_ASTERISK")
    , (P.KP_Min, "PMNS")
    , (P.KP_Min, "KP_MINUS")
    , (P.KP_7, "P7")
    , (P.KP_7, "KP_7")
    , (P.KP_8, "P8")
    , (P.KP_8, "KP_8")
    , (P.KP_9, "P9")
    , (P.KP_9, "KP_9")
    , (P.KP_Plus, "PPLS")
    , (P.KP_Plus, "KP_PLUS")
    , (P.KP_4, "P4")
    , (P.KP_4, "KP_4")
    , (P.KP_5, "P5")
    , (P.KP_5, "KP_5")
    , (P.KP_6, "P6")
    , (P.KP_6, "KP_6")
    , (P.KP_Comma, "PCMM")
    , (P.KP_Comma, "KP_COMMA")
    , (P.KP_1, "P1")
    , (P.KP_1, "KP_1")
    , (P.KP_2, "P2")
    , (P.KP_2, "KP_2")
    , (P.KP_3, "P3")
    , (P.KP_3, "KP_3")
    , (P.KP_Enter, "PENT")
    , (P.KP_Enter, "KP_ENTER")
    , (P.KP_0, "P0")
    , (P.KP_0, "KP_0")
    , (P.KP_Dec, "PDOT")
    , (P.KP_Dec, "KP_DOT")
    , (P.KP_Eq, "PEQL")
    , (P.KP_Eq, "KP_EQUAL")

    , (P.PlayPause, "MPLY")
    , (P.PlayPause, "MEDIA_PLAY_PAUSE")
    , (P.AudioPlay, "MPLY")
    , (P.AudioPlay, "MEDIA_PLAY_PAUSE")
    , (P.Previous, "MPRV")
    , (P.Previous, "MEDIA_PREV_TRACK")
    , (P.Next, "MNXT")
    , (P.Next, "MEDIA_NEXT_TRACK")
    , (P.Stop, "MSTP")
    , (P.Stop, "MEDIA_STOP")
    , (P.AudioRewind, "MRWD")
    , (P.AudioRewind, "MEDIA_REWIND")
    , (P.AudioForward, "MFFD")
    , (P.AudioForward, "MEDIA_FAST_FORWARD")
    , (P.Mute, "MUTE")
    , (P.Mute, "AUDIO_MUTE")
    , (P.VolumeDown, "VOLD")
    , (P.VolumeDown, "AUDIO_VOL_DOWN")
    , (P.VolumeUp, "VOLU")
    , (P.VolumeUp, "AUDIO_VOL_UP")
    , (P.Eject, "EJCT")
    , (P.Eject, "MEDIA_EJECT")

    , (P.Browser_Back, "WBAK")
    , (P.Browser_Back, "WWW_BACK")
    , (P.Browser_Forward, "WFWD")
    , (P.Browser_Forward, "WWW_FORWARD")
    , (P.Browser_Refresh, "WREF")
    , (P.Browser_Refresh, "WWW_REFRESH")
    , (P.Browser_Stop, "WSTP")
    , (P.Browser_Stop, "WWW_STOP")
    , (P.Browser_Search, "WSCH")
    , (P.Browser_Search, "WWW_SEARCH")
    , (P.Browser_Favorites, "WFAV")
    , (P.Browser_Favorites, "WWW_FAVORITES")

    , (P.Calculator, "CALC")
    , (P.Calculator, "CALCULATOR")
    , (P.MediaPlayer, "MSEL")
    , (P.Browser, "WHOM")
    , (P.Browser, "WWW_HOME")
    , (P.Mail, "MAIL")
    , (P.Search, "FIND")
    , (P.MyComputer, "MYCM")
    , (P.MyComputer, "MY_COMPUTER")
    , (P.Help, "HELP")

    , (P.Power, "PWR")
    , (P.Power, "POWER")
    , (P.Power, "SYSTEM_POWER")
    , (P.Sleep, "SLEP")
    , (P.Sleep, "SYSTEM_SLEEP")
    , (P.Wake, "WAKE")
    , (P.Wake, "SYSTEM_WAKE")

    , (P.F13, "F13")
    , (P.F14, "F14")
    , (P.F15, "F15")
    , (P.F16, "F16")
    , (P.F17, "F17")
    , (P.F18, "F18")
    , (P.F19, "F19")
    , (P.F20, "F20")
    , (P.F21, "F21")
    , (P.F22, "F22")
    , (P.F23, "F23")
    , (P.F24, "F24")
    , (P.Hash, "NUHS")
    , (P.Hash, "NONUS_HASH")
    , (P.Ro, "RO")
    , (P.Yen, "JYEN")
    , (P.Muhenkan, "MHEN")
    , (P.Henkan, "HENK")
    , (P.Katakana, "KANA")
    ]

actionAndTmkAction ∷ [(Action, String)]
actionAndTmkAction =
    [ (A.MouseUp, "MS_U")
    , (A.MouseUp, "MS_UP")
    , (A.MouseDown, "MS_D")
    , (A.MouseDown, "MS_DOWN")
    , (A.MouseLeft, "MS_L")
    , (A.MouseLeft, "MS_LEFT")
    , (A.MouseRight, "MS_R")
    , (A.MouseRight, "MS_RIGHT")
    , (A.Button_L, "BTN1")
    , (A.Button_L, "MS_BTN1")
    , (A.Button_M, "BTN2")
    , (A.Button_M, "MS_BTN2")
    , (A.Button_R, "BTN3")
    , (A.Button_R, "MS_BTN3")
    , (A.WheelUp, "WH_U")
    , (A.WheelUp, "MS_WH_UP")
    , (A.WheelDown, "WH_D")
    , (A.WheelDown, "MS_WH_DOWN")
    , (A.WheelLeft, "WH_L")
    , (A.WheelLeft, "MS_WH_LEFT")
    , (A.WheelRight, "WH_R")
    , (A.WheelRight, "MS_WH_RIGHT")
    ]

isRealModifier ∷ Modifier → Bool
isRealModifier = (∈ map fst modifierAndKeycode)

modifierToRealModifiers ∷ Modifier → [Modifier]
modifierToRealModifiers M.Shift = [M.Shift_L, M.Shift_R]
modifierToRealModifiers M.Win = [M.Win_R, M.Win_R]
modifierToRealModifiers M.Alt = [M.Alt_L, M.Alt_R]
modifierToRealModifiers M.Control = [M.Control_L, M.Control_R]
modifierToRealModifiers modifier = [modifier]

modifierAndKeycode ∷ [(Modifier, String)]
modifierAndKeycode =
    [ (M.Shift, "LSFT")
    , (M.Shift_L, "LSFT")
    , (M.Shift_R, "RSFT")
    , (M.CapsLock, "CAPS")
    , (M.Win, "LGUI")
    , (M.Win_L, "LGUI")
    , (M.Win_R, "RGUI")
    , (M.Alt, "LALT")
    , (M.Alt_L, "LALT")
    , (M.Alt_R, "RALT")
    , (M.Control, "LCTL")
    , (M.Control_L, "LCTL")
    , (M.Control_R, "RCTL")
    , (M.NumLock, "NLCK")
    ]

data Keymap = Keymap
    { __usedPosses ∷ Set Pos
    , __name ∷ String
    , __posses ∷ [[Pos]]
    , __sizes ∷ [[Int]]
    }

keymap ∷ String → [[Pos]] → [[Int]] → Keymap
keymap name posses = Keymap (S.fromList (concat posses)) name posses

unimap ∷ Keymap
unimap = keymap "UNIMAP"
    [ [ P.F13, P.F14, P.F15, P.F16, P.F17, P.F18, P.F19, P.F20, P.F21, P.F22, P.F23, P.F24 ]
    , [ P.Esc, P.F1, P.F2, P.F3, P.F4, P.F5, P.F6, P.F7, P.F8, P.F9, P.F10, P.F11, P.F12, P.PrintScreen, P.ScrollLock, P.Pause, P.VolumeDown, P.VolumeUp, P.Mute ]
    , [ P.Tilde, P.N1, P.N2, P.N3, P.N4, P.N5, P.N6, P.N7, P.N8, P.N9, P.N0, P.Minus, P.Plus, P.Yen, P.Backspace, P.Insert, P.Home, P.PageUp, P.NumLock, P.KP_Div, P.KP_Mult, P.KP_Min ]
    , [ P.Tab, P.Q, P.W, P.E, P.R, P.T, P.Y, P.U, P.I, P.O, P.P, P.Bracket_L, P.Bracket_R, P.Backslash, P.Delete, P.End, P.PageDown, P.KP_7, P.KP_8, P.KP_9, P.KP_Plus ]
    , [ P.CapsLock, P.A, P.S, P.D, P.F, P.G, P.H, P.J, P.K, P.L, P.Semicolon, P.Apastrophe, P.Hash, P.Enter, P.KP_4, P.KP_5, P.KP_6, P.KP_Comma ]
    , [ P.Shift_L, P.Iso, P.Z, P.X, P.C, P.V, P.B, P.N, P.M, P.Comma, P.Period, P.Slash, P.Ro, P.Shift_R, P.Up, P.KP_1, P.KP_2, P.KP_3, P.KP_Enter ]
    , [ P.Control_L, P.Win_L, P.Alt_L, P.Muhenkan, P.Space, P.Henkan, P.Katakana, P.Alt_R, P.Win_R, P.Menu, P.Control_R, P.Left, P.Down, P.Right, P.KP_0, P.KP_Dec, P.KP_Eq ]
    ]
    [ [      14, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4 ]
    , [ 4,    9, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,      14, 4, 4,       13, 4, 4 ]
    , [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,    9, 4, 4,     8, 4, 4, 4 ]
    , [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,    9,    9, 4, 4,     8, 4, 4, 4 ]
    , [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,    9, 4,                28, 4, 4, 4 ]
    , [ 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,    9, 4,      14,       13, 4, 4, 4 ]
    , [ 4, 4, 4, 4,      14,      14, 4, 4, 4, 4, 4,    9, 4, 4,     8,    9, 4 ]
    ]
