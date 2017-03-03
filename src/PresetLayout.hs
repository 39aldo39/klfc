{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module PresetLayout
    ( defaultLayout
    , defaultFullLayout
    , defaultKeys
    , defaultFullKeys
    , defaultMacKeys
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅))

import qualified Layout.Action as A
import Layout.Key (Key(Key), filterKeyOnShiftstates)
import Layout.Layout (Layout(Layout))
import qualified Layout.Modifier as M
import qualified Layout.Pos as P
import Layout.Types

defaultLayout ∷ Layout
defaultLayout = Layout (∅) (∅) (∅) defaultKeys

defaultFullLayout ∷ Layout
defaultFullLayout = Layout (∅) (∅) (∅) (qwertyKeys ⧺ defaultFullKeys)

defaultKeys ∷ [Key]
defaultKeys = map (filterKeyOnShiftstates null) defaultFullKeys

defaultFullKeys ∷ [Key]
defaultFullKeys =
    [ Key P.Backspace Nothing [M.empty] [Action A.Backspace] Nothing
    , Key P.Tab Nothing [M.empty, M.singleton M.Shift] [Action A.Tab, Action A.LeftTab] Nothing
    , Key P.CapsLock Nothing [M.empty] [Modifiers Lock [M.CapsLock]] Nothing
    , Key P.Enter Nothing [M.empty] [Action A.Enter] Nothing
    , Key P.Shift_L Nothing [M.empty] [Modifiers Shift [M.Shift_L]] Nothing
    , Key P.Shift_R Nothing [M.empty] [Modifiers Shift [M.Shift_R]] Nothing

    , Key P.Control_L Nothing [M.empty] [Modifiers Shift [M.Control_L]] Nothing
    , Key P.Win_L Nothing [M.empty] [Modifiers Shift [M.Win_L]] Nothing
    , Key P.Alt_L Nothing [M.empty] [Modifiers Shift [M.Alt_L]] Nothing
    , Key P.Space Nothing [M.empty] [Char ' '] Nothing
    , Key P.Alt_R Nothing [M.empty] [Modifiers Shift [M.Alt_R]] Nothing
    , Key P.Win_R Nothing [M.empty] [Modifiers Shift [M.Win_R]] Nothing
    , Key P.Menu Nothing [M.empty] [Action A.Menu] Nothing
    , Key P.Control_R Nothing [M.empty] [Modifiers Shift [M.Control_R]] Nothing

    , Key P.Esc Nothing [M.empty] [Action A.Esc] Nothing
    , Key P.F1 Nothing [M.empty] [Action A.F1] Nothing
    , Key P.F2 Nothing [M.empty] [Action A.F2] Nothing
    , Key P.F3 Nothing [M.empty] [Action A.F3] Nothing
    , Key P.F4 Nothing [M.empty] [Action A.F4] Nothing
    , Key P.F5 Nothing [M.empty] [Action A.F5] Nothing
    , Key P.F6 Nothing [M.empty] [Action A.F6] Nothing
    , Key P.F7 Nothing [M.empty] [Action A.F7] Nothing
    , Key P.F8 Nothing [M.empty] [Action A.F8] Nothing
    , Key P.F9 Nothing [M.empty] [Action A.F9] Nothing
    , Key P.F10 Nothing [M.empty] [Action A.F10] Nothing
    , Key P.F11 Nothing [M.empty] [Action A.F11] Nothing
    , Key P.F12 Nothing [M.empty] [Action A.F12] Nothing
    , Key P.PrintScreen Nothing [M.empty, M.singleton M.Alt] [Action A.PrintScreen, Action A.SysRq] Nothing
    , Key P.ScrollLock Nothing [M.empty] [Action A.ScrollLock] Nothing
    , Key P.Pause Nothing [M.empty, M.singleton M.Control] [Action A.Pause, Action A.ControlBreak] Nothing

    , Key P.Insert Nothing [M.empty] [Action A.Insert] Nothing
    , Key P.Delete Nothing [M.empty] [Action A.Delete] Nothing
    , Key P.Home Nothing [M.empty] [Action A.Home] Nothing
    , Key P.End Nothing [M.empty] [Action A.End] Nothing
    , Key P.PageUp Nothing [M.empty] [Action A.PageUp] Nothing
    , Key P.PageDown Nothing [M.empty] [Action A.PageDown] Nothing
    , Key P.Up Nothing [M.empty] [Action A.Up] Nothing
    , Key P.Left Nothing [M.empty] [Action A.Left] Nothing
    , Key P.Down Nothing [M.empty] [Action A.Down] Nothing
    , Key P.Right Nothing [M.empty] [Action A.Right] Nothing

    , Key P.NumLock Nothing [M.empty] [Modifiers Lock [M.NumLock]] Nothing
    , Key P.KP_Div Nothing [M.empty] [Action A.KP_Div] Nothing
    , Key P.KP_Mult Nothing [M.empty] [Action A.KP_Mult] Nothing
    , Key P.KP_Min Nothing [M.empty] [Action A.KP_Min] Nothing
    , Key P.KP_7 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Home, Action A.KP_7] Nothing
    , Key P.KP_8 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Up, Action A.KP_8] Nothing
    , Key P.KP_9 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_PageUp, Action A.KP_9] Nothing
    , Key P.KP_Plus Nothing [M.empty] [Action A.KP_Plus] Nothing
    , Key P.KP_4 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Left, Action A.KP_4] Nothing
    , Key P.KP_5 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Begin, Action A.KP_5] Nothing
    , Key P.KP_6 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Right, Action A.KP_6] Nothing
    , Key P.KP_Comma Nothing [M.empty] [Action A.KP_Comma] Nothing
    , Key P.KP_1 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_End, Action A.KP_1] Nothing
    , Key P.KP_2 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Down, Action A.KP_2] Nothing
    , Key P.KP_3 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_PageDown, Action A.KP_3] Nothing
    , Key P.KP_Enter Nothing [M.empty] [Action A.KP_Enter] Nothing
    , Key P.KP_0 Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Insert, Action A.KP_0] Nothing
    , Key P.KP_Dec Nothing [M.empty, M.singleton M.NumLock] [Action A.KP_Delete, Action A.KP_Dec] Nothing
    , Key P.KP_Eq Nothing [M.empty] [Action A.KP_Eq] Nothing

    , Key P.AudioPlay Nothing [M.empty] [Action A.AudioPlay] Nothing
    , Key P.AudioPause Nothing [M.empty] [Action A.AudioPause] Nothing
    , Key P.PlayPause Nothing [M.empty] [Action A.PlayPause] Nothing
    , Key P.Previous Nothing [M.empty] [Action A.Previous] Nothing
    , Key P.Next Nothing [M.empty] [Action A.Next] Nothing
    , Key P.Stop Nothing [M.empty] [Action A.Stop] Nothing
    , Key P.ToggleRepeat Nothing [M.empty] [Action A.ToggleRepeat] Nothing
    , Key P.ToggleRandom Nothing [M.empty] [Action A.ToggleRandom] Nothing
    , Key P.AudioRewind Nothing [M.empty] [Action A.AudioRewind] Nothing
    , Key P.AudioForward Nothing [M.empty] [Action A.AudioForward] Nothing
    , Key P.Mute Nothing [M.empty] [Action A.Mute] Nothing
    , Key P.VolumeDown Nothing [M.empty] [Action A.VolumeDown] Nothing
    , Key P.VolumeUp Nothing [M.empty] [Action A.VolumeUp] Nothing
    , Key P.Eject Nothing [M.empty] [Action A.Eject] Nothing

    , Key P.Browser_Back Nothing [M.empty] [Action A.Browser_Back] Nothing
    , Key P.Browser_Forward Nothing [M.empty] [Action A.Browser_Forward] Nothing
    , Key P.Browser_Refresh Nothing [M.empty] [Action A.Browser_Refresh] Nothing
    , Key P.Browser_Stop Nothing [M.empty] [Action A.Browser_Stop] Nothing
    , Key P.Browser_Search Nothing [M.empty] [Action A.Browser_Search] Nothing
    , Key P.Browser_Favorites Nothing [M.empty] [Action A.Browser_Favorites] Nothing

    , Key P.Calculator Nothing [M.empty] [Action A.Calculator] Nothing
    , Key P.MediaPlayer Nothing [M.empty] [Action A.MediaPlayer] Nothing
    , Key P.Browser Nothing [M.empty] [Action A.Browser] Nothing
    , Key P.Mail Nothing [M.empty] [Action A.Mail] Nothing
    , Key P.Search Nothing [M.empty] [Action A.Search] Nothing
    , Key P.Explorer Nothing [M.empty] [Action A.Explorer] Nothing
    , Key P.WWW Nothing [M.empty] [Action A.WWW] Nothing
    , Key P.MyComputer Nothing [M.empty] [Action A.MyComputer] Nothing
    , Key P.Help Nothing [M.empty] [Action A.Help] Nothing

    , Key P.Launch0 Nothing [M.empty] [Action A.Launch0] Nothing
    , Key P.Launch1 Nothing [M.empty] [Action A.Launch1] Nothing
    , Key P.Launch2 Nothing [M.empty] [Action A.Launch2] Nothing
    , Key P.Launch3 Nothing [M.empty] [Action A.Launch3] Nothing
    , Key P.Launch4 Nothing [M.empty] [Action A.Launch4] Nothing
    , Key P.Launch5 Nothing [M.empty] [Action A.Launch5] Nothing
    , Key P.Launch6 Nothing [M.empty] [Action A.Launch6] Nothing
    , Key P.Launch7 Nothing [M.empty] [Action A.Launch7] Nothing
    , Key P.Launch8 Nothing [M.empty] [Action A.Launch8] Nothing
    , Key P.Launch9 Nothing [M.empty] [Action A.Launch9] Nothing
    , Key P.LaunchA Nothing [M.empty] [Action A.LaunchA] Nothing
    , Key P.LaunchB Nothing [M.empty] [Action A.LaunchB] Nothing
    , Key P.LaunchC Nothing [M.empty] [Action A.LaunchC] Nothing
    , Key P.LaunchD Nothing [M.empty] [Action A.LaunchD] Nothing
    , Key P.LaunchE Nothing [M.empty] [Action A.LaunchE] Nothing
    , Key P.LaunchF Nothing [M.empty] [Action A.LaunchF] Nothing

    , Key P.Power Nothing [M.empty] [Action A.Power] Nothing
    , Key P.Sleep Nothing [M.empty] [Action A.Sleep] Nothing
    , Key P.Wake Nothing [M.empty] [Action A.Wake] Nothing
    , Key P.BrightnessDown Nothing [M.empty] [Action A.BrightnessDown] Nothing
    , Key P.BrightnessUp Nothing [M.empty] [Action A.BrightnessUp] Nothing

    , Key P.F13 Nothing [M.empty] [Action A.F13] Nothing
    , Key P.F14 Nothing [M.empty] [Action A.F14] Nothing
    , Key P.F15 Nothing [M.empty] [Action A.F15] Nothing
    , Key P.F16 Nothing [M.empty] [Action A.F16] Nothing
    , Key P.F17 Nothing [M.empty] [Action A.F17] Nothing
    , Key P.F18 Nothing [M.empty] [Action A.F18] Nothing
    , Key P.F19 Nothing [M.empty] [Action A.F19] Nothing
    , Key P.F20 Nothing [M.empty] [Action A.F20] Nothing
    , Key P.F21 Nothing [M.empty] [Action A.F21] Nothing
    , Key P.F22 Nothing [M.empty] [Action A.F22] Nothing
    , Key P.F23 Nothing [M.empty] [Action A.F23] Nothing
    , Key P.F24 Nothing [M.empty] [Action A.F24] Nothing

    , Key P.Ro Nothing [M.empty] [Action A.Ro] Nothing
    , Key P.Yen Nothing [M.empty] [Char '¥'] Nothing
    , Key P.Muhenkan Nothing [M.empty] [Action A.Muhenkan] Nothing
    , Key P.Henkan Nothing [M.empty] [Action A.Henkan] Nothing
    , Key P.Katakana Nothing [M.empty] [Action A.Katakana] Nothing
    ]

defaultMacKeys ∷ [Key]
defaultMacKeys =
    [ Key P.Esc Nothing [M.empty] [Char '\ESC'] Nothing
    , Key P.F1 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F2 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F3 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F4 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F5 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F6 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F7 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F8 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F9 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F10 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F11 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.F12 Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.PrintScreen Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.ScrollLock Nothing [M.empty] [Char '\x10'] Nothing
    , Key P.Pause Nothing [M.empty] [Char '\x10'] Nothing

    , Key P.Insert Nothing [M.empty] [Char '\x05'] Nothing
    , Key P.Delete Nothing [M.empty] [Char '\DEL'] Nothing
    , Key P.Home Nothing [M.empty] [Char '\x01'] Nothing
    , Key P.End Nothing [M.empty] [Char '\x04'] Nothing
    , Key P.PageUp Nothing [M.empty] [Char '\x0B'] Nothing
    , Key P.PageDown Nothing [M.empty] [Char '\x0C'] Nothing
    , Key P.Up Nothing [M.empty] [Char '\x1E'] Nothing
    , Key P.Left Nothing [M.empty] [Char '\x1C'] Nothing
    , Key P.Down Nothing [M.empty] [Char '\x1F'] Nothing
    , Key P.Right Nothing [M.empty] [Char '\x1D'] Nothing

    , Key P.Backspace Nothing [M.empty] [Char '\b'] Nothing
    , Key P.Tab Nothing [M.empty] [Char '\t'] Nothing
    , Key P.Enter Nothing [M.empty] [Char '\r'] Nothing
    , Key P.Space Nothing [M.empty] [Char ' '] Nothing

    , Key P.NumLock Nothing [M.empty] [Char '\ESC'] Nothing
    , Key P.KP_Div Nothing [M.empty] [Char '/'] Nothing
    , Key P.KP_Mult Nothing [M.empty] [Char '*'] Nothing
    , Key P.KP_Min Nothing [M.empty] [Char '-'] Nothing
    , Key P.KP_7 Nothing [M.empty] [Char '7'] Nothing
    , Key P.KP_8 Nothing [M.empty] [Char '8'] Nothing
    , Key P.KP_9 Nothing [M.empty] [Char '9'] Nothing
    , Key P.KP_Plus Nothing [M.empty] [Char '+'] Nothing
    , Key P.KP_4 Nothing [M.empty] [Char '4'] Nothing
    , Key P.KP_5 Nothing [M.empty] [Char '5'] Nothing
    , Key P.KP_6 Nothing [M.empty] [Char '6'] Nothing
    , Key P.KP_1 Nothing [M.empty] [Char '1'] Nothing
    , Key P.KP_2 Nothing [M.empty] [Char '2'] Nothing
    , Key P.KP_3 Nothing [M.empty] [Char '3'] Nothing
    , Key P.KP_Enter Nothing [M.empty] [Char '\x03'] Nothing
    , Key P.KP_0 Nothing [M.empty] [Char '0'] Nothing
    , Key P.KP_Dec Nothing [M.empty] [Char '.'] Nothing
    , Key P.KP_Eq Nothing [M.empty] [Char '='] Nothing
    ]

qwertyKeys ∷ [Key]
qwertyKeys =
    [ Key P.Tilde Nothing [M.empty, M.singleton M.Shift] [Char '`', Char '~'] Nothing
    , Key P.N1 Nothing [M.empty, M.singleton M.Shift] [Char '1', Char '!'] Nothing
    , Key P.N2 Nothing [M.empty, M.singleton M.Shift] [Char '2', Char '@'] Nothing
    , Key P.N3 Nothing [M.empty, M.singleton M.Shift] [Char '3', Char '#'] Nothing
    , Key P.N4 Nothing [M.empty, M.singleton M.Shift] [Char '4', Char '$'] Nothing
    , Key P.N5 Nothing [M.empty, M.singleton M.Shift] [Char '5', Char '%'] Nothing
    , Key P.N6 Nothing [M.empty, M.singleton M.Shift] [Char '6', Char '^'] Nothing
    , Key P.N7 Nothing [M.empty, M.singleton M.Shift] [Char '7', Char '&'] Nothing
    , Key P.N8 Nothing [M.empty, M.singleton M.Shift] [Char '8', Char '*'] Nothing
    , Key P.N9 Nothing [M.empty, M.singleton M.Shift] [Char '9', Char '('] Nothing
    , Key P.N0 Nothing [M.empty, M.singleton M.Shift] [Char '0', Char ')'] Nothing
    , Key P.Minus Nothing [M.empty, M.singleton M.Shift] [Char '-', Char '_'] Nothing
    , Key P.Plus Nothing [M.empty, M.singleton M.Shift] [Char '=', Char '+'] Nothing

    , Key P.Q Nothing [M.empty, M.singleton M.Shift] [Char 'q', Char 'Q'] Nothing
    , Key P.W Nothing [M.empty, M.singleton M.Shift] [Char 'w', Char 'W'] Nothing
    , Key P.E Nothing [M.empty, M.singleton M.Shift] [Char 'e', Char 'E'] Nothing
    , Key P.R Nothing [M.empty, M.singleton M.Shift] [Char 'r', Char 'R'] Nothing
    , Key P.T Nothing [M.empty, M.singleton M.Shift] [Char 't', Char 'T'] Nothing
    , Key P.Y Nothing [M.empty, M.singleton M.Shift] [Char 'y', Char 'Y'] Nothing
    , Key P.U Nothing [M.empty, M.singleton M.Shift] [Char 'u', Char 'U'] Nothing
    , Key P.I Nothing [M.empty, M.singleton M.Shift] [Char 'i', Char 'I'] Nothing
    , Key P.O Nothing [M.empty, M.singleton M.Shift] [Char 'o', Char 'O'] Nothing
    , Key P.P Nothing [M.empty, M.singleton M.Shift] [Char 'p', Char 'P'] Nothing
    , Key P.Bracket_L Nothing [M.empty, M.singleton M.Shift] [Char '[', Char '{'] Nothing
    , Key P.Bracket_R Nothing [M.empty, M.singleton M.Shift] [Char ']', Char '}'] Nothing
    , Key P.Backslash Nothing [M.empty, M.singleton M.Shift] [Char '\\', Char '|'] Nothing

    , Key P.A Nothing [M.empty, M.singleton M.Shift] [Char 'a', Char 'A'] Nothing
    , Key P.S Nothing [M.empty, M.singleton M.Shift] [Char 's', Char 'S'] Nothing
    , Key P.D Nothing [M.empty, M.singleton M.Shift] [Char 'd', Char 'D'] Nothing
    , Key P.F Nothing [M.empty, M.singleton M.Shift] [Char 'f', Char 'F'] Nothing
    , Key P.G Nothing [M.empty, M.singleton M.Shift] [Char 'g', Char 'G'] Nothing
    , Key P.H Nothing [M.empty, M.singleton M.Shift] [Char 'h', Char 'H'] Nothing
    , Key P.J Nothing [M.empty, M.singleton M.Shift] [Char 'j', Char 'J'] Nothing
    , Key P.K Nothing [M.empty, M.singleton M.Shift] [Char 'k', Char 'K'] Nothing
    , Key P.L Nothing [M.empty, M.singleton M.Shift] [Char 'l', Char 'L'] Nothing
    , Key P.Semicolon Nothing [M.empty, M.singleton M.Shift] [Char ';', Char ':'] Nothing
    , Key P.Apastrophe Nothing [M.empty, M.singleton M.Shift] [Char '\'', Char '"'] Nothing

    , Key P.Z Nothing [M.empty, M.singleton M.Shift] [Char 'z', Char 'Z'] Nothing
    , Key P.X Nothing [M.empty, M.singleton M.Shift] [Char 'x', Char 'X'] Nothing
    , Key P.C Nothing [M.empty, M.singleton M.Shift] [Char 'c', Char 'C'] Nothing
    , Key P.V Nothing [M.empty, M.singleton M.Shift] [Char 'v', Char 'V'] Nothing
    , Key P.B Nothing [M.empty, M.singleton M.Shift] [Char 'b', Char 'B'] Nothing
    , Key P.N Nothing [M.empty, M.singleton M.Shift] [Char 'n', Char 'N'] Nothing
    , Key P.M Nothing [M.empty, M.singleton M.Shift] [Char 'm', Char 'M'] Nothing
    , Key P.Comma Nothing [M.empty, M.singleton M.Shift] [Char ',', Char '<'] Nothing
    , Key P.Period Nothing [M.empty, M.singleton M.Shift] [Char '.', Char '>'] Nothing
    , Key P.Slash Nothing [M.empty, M.singleton M.Shift] [Char '/', Char '?'] Nothing
    ]
