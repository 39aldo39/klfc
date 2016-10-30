{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module PresetLayout
    ( defaultLayout
    , defaultFullLayout
    , defaultKeys
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅))
import qualified WithPlus as WP (singleton)

import qualified Layout.Action as A
import Layout.Key (Key(Key), filterKeyOnShiftstates)
import Layout.Layout (Layout(Layout))
import qualified Layout.Modifier as M
import qualified Layout.Pos as P
import Layout.Types

defaultLayout ∷ Layout
defaultLayout = Layout (∅) (∅) (∅) defaultKeys

defaultFullLayout ∷ Layout
defaultFullLayout = Layout (∅) (∅) (∅) (qwertyKeys ⧺ defaultKeysFull)

defaultKeys ∷ [Key]
defaultKeys = map (filterKeyOnShiftstates null) defaultKeysFull

defaultKeysFull ∷ [Key]
defaultKeysFull =
    [ Key P.Esc Nothing [(∅)] [Action A.Esc] Nothing
    , Key P.F1 Nothing [(∅)] [Action A.F1] Nothing
    , Key P.F2 Nothing [(∅)] [Action A.F2] Nothing
    , Key P.F3 Nothing [(∅)] [Action A.F3] Nothing
    , Key P.F4 Nothing [(∅)] [Action A.F4] Nothing
    , Key P.F5 Nothing [(∅)] [Action A.F5] Nothing
    , Key P.F6 Nothing [(∅)] [Action A.F6] Nothing
    , Key P.F7 Nothing [(∅)] [Action A.F7] Nothing
    , Key P.F8 Nothing [(∅)] [Action A.F8] Nothing
    , Key P.F9 Nothing [(∅)] [Action A.F9] Nothing
    , Key P.F10 Nothing [(∅)] [Action A.F10] Nothing
    , Key P.F11 Nothing [(∅)] [Action A.F11] Nothing
    , Key P.F12 Nothing [(∅)] [Action A.F12] Nothing

    , Key P.PrintScreen Nothing [(∅), WP.singleton M.Alt] [Action A.PrintScreen, Action A.SysRq] Nothing
    , Key P.ScrollLock Nothing [(∅)] [Action A.ScrollLock] Nothing
    , Key P.Pause Nothing [(∅), WP.singleton M.Control] [Action A.Pause, Action A.ControlBreak] Nothing

    , Key P.Backspace Nothing [(∅)] [Action A.Backspace] Nothing
    , Key P.Tab Nothing [(∅), WP.singleton M.Shift] [Action A.Tab, Action A.LeftTab] Nothing
    , Key P.CapsLock Nothing [(∅)] [Action A.CapsLock] Nothing
    , Key P.Enter Nothing [(∅)] [Action A.Enter] Nothing
    , Key P.Shift_L Nothing [(∅)] [Action A.Shift_L] Nothing
    , Key P.Shift_R Nothing [(∅)] [Action A.Shift_R] Nothing

    , Key P.Control_L Nothing [(∅)] [Action A.Control_L] Nothing
    , Key P.Win_L Nothing [(∅)] [Action A.Win_L] Nothing
    , Key P.Alt_L Nothing [(∅)] [Action A.Alt_L] Nothing
    , Key P.Space Nothing [(∅)] [Char ' '] Nothing
    , Key P.Alt_R Nothing [(∅)] [Action A.Alt_R] Nothing
    , Key P.Win_R Nothing [(∅)] [Action A.Win_R] Nothing
    , Key P.Menu Nothing [(∅)] [Action A.Menu] Nothing
    , Key P.Control_R Nothing [(∅)] [Action A.Control_R] Nothing

    , Key P.Insert Nothing [(∅)] [Action A.Insert] Nothing
    , Key P.Delete Nothing [(∅)] [Action A.Delete] Nothing
    , Key P.Home Nothing [(∅)] [Action A.Home] Nothing
    , Key P.End Nothing [(∅)] [Action A.End] Nothing
    , Key P.PageUp Nothing [(∅)] [Action A.PageUp] Nothing
    , Key P.PageDown Nothing [(∅)] [Action A.PageDown] Nothing
    , Key P.Up Nothing [(∅)] [Action A.Up] Nothing
    , Key P.Left Nothing [(∅)] [Action A.Left] Nothing
    , Key P.Down Nothing [(∅)] [Action A.Down] Nothing
    , Key P.Right Nothing [(∅)] [Action A.Right] Nothing

    , Key P.NumLock Nothing [(∅)] [Action A.NumLock] Nothing
    , Key P.KP_Div Nothing [(∅)] [Action A.KP_Div] Nothing
    , Key P.KP_Mult Nothing [(∅)] [Action A.KP_Mult] Nothing
    , Key P.KP_Min Nothing [(∅)] [Action A.KP_Min] Nothing
    , Key P.KP_7 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Home, Action A.KP_7] Nothing
    , Key P.KP_8 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Up, Action A.KP_8] Nothing
    , Key P.KP_9 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_PageUp, Action A.KP_9] Nothing
    , Key P.KP_Plus Nothing [(∅)] [Action A.KP_Plus] Nothing
    , Key P.KP_4 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Left, Action A.KP_4] Nothing
    , Key P.KP_5 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Begin, Action A.KP_5] Nothing
    , Key P.KP_6 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Right, Action A.KP_6] Nothing
    , Key P.KP_1 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_End, Action A.KP_1] Nothing
    , Key P.KP_2 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Down, Action A.KP_2] Nothing
    , Key P.KP_3 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_PageDown, Action A.KP_3] Nothing
    , Key P.KP_Enter Nothing [(∅)] [Action A.KP_Enter] Nothing
    , Key P.KP_0 Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Insert, Action A.KP_0] Nothing
    , Key P.KP_Dec Nothing [(∅), WP.singleton M.NumLock] [Action A.KP_Delete, Action A.KP_Dec] Nothing
    , Key P.KP_Eq Nothing [(∅)] [Action A.KP_Eq] Nothing

    , Key P.Power Nothing [(∅)] [Action A.Power] Nothing
    , Key P.Sleep Nothing [(∅)] [Action A.Sleep] Nothing
    , Key P.Wake Nothing [(∅)] [Action A.Wake] Nothing

    , Key P.AudioPlay Nothing [(∅)] [Action A.AudioPlay] Nothing
    , Key P.AudioPause Nothing [(∅)] [Action A.AudioPause] Nothing
    , Key P.PlayPause Nothing [(∅)] [Action A.PlayPause] Nothing
    , Key P.Previous Nothing [(∅)] [Action A.Previous] Nothing
    , Key P.Next Nothing [(∅)] [Action A.Next] Nothing
    , Key P.Stop Nothing [(∅)] [Action A.Stop] Nothing
    , Key P.ToggleRepeat Nothing [(∅)] [Action A.ToggleRepeat] Nothing
    , Key P.ToggleRandom Nothing [(∅)] [Action A.ToggleRandom] Nothing
    , Key P.AudioRewind Nothing [(∅)] [Action A.AudioRewind] Nothing
    , Key P.AudioForward Nothing [(∅)] [Action A.AudioForward] Nothing
    , Key P.Mute Nothing [(∅)] [Action A.Mute] Nothing
    , Key P.VolumeDown Nothing [(∅)] [Action A.VolumeDown] Nothing
    , Key P.VolumeUp Nothing [(∅)] [Action A.VolumeUp] Nothing
    , Key P.BrightnessDown Nothing [(∅)] [Action A.BrightnessDown] Nothing
    , Key P.BrightnessUp Nothing [(∅)] [Action A.BrightnessUp] Nothing
    , Key P.Eject Nothing [(∅)] [Action A.Eject] Nothing
    , Key P.Browser_Back Nothing [(∅)] [Action A.Browser_Back] Nothing
    , Key P.Browser_Forward Nothing [(∅)] [Action A.Browser_Forward] Nothing
    , Key P.Browser_Refresh Nothing [(∅)] [Action A.Browser_Refresh] Nothing
    , Key P.Browser_Stop Nothing [(∅)] [Action A.Browser_Stop] Nothing
    , Key P.Browser_Search Nothing [(∅)] [Action A.Browser_Search] Nothing
    , Key P.Browser_Favorites Nothing [(∅)] [Action A.Browser_Favorites] Nothing
    , Key P.Calculator Nothing [(∅)] [Action A.Calculator] Nothing
    , Key P.MediaPlayer Nothing [(∅)] [Action A.MediaPlayer] Nothing
    , Key P.Browser Nothing [(∅)] [Action A.Browser] Nothing
    , Key P.Mail Nothing [(∅)] [Action A.Mail] Nothing
    , Key P.Search Nothing [(∅)] [Action A.Search] Nothing
    , Key P.Explorer Nothing [(∅)] [Action A.Explorer] Nothing
    , Key P.WWW Nothing [(∅)] [Action A.WWW] Nothing
    , Key P.MyComputer Nothing [(∅)] [Action A.MyComputer] Nothing
    , Key P.Launch0 Nothing [(∅)] [Action A.Launch0] Nothing
    , Key P.Launch1 Nothing [(∅)] [Action A.Launch1] Nothing
    , Key P.Launch2 Nothing [(∅)] [Action A.Launch2] Nothing
    , Key P.Launch3 Nothing [(∅)] [Action A.Launch3] Nothing
    , Key P.Launch4 Nothing [(∅)] [Action A.Launch4] Nothing
    , Key P.Launch5 Nothing [(∅)] [Action A.Launch5] Nothing
    , Key P.Launch6 Nothing [(∅)] [Action A.Launch6] Nothing
    , Key P.Launch7 Nothing [(∅)] [Action A.Launch7] Nothing
    , Key P.Launch8 Nothing [(∅)] [Action A.Launch8] Nothing
    , Key P.Launch9 Nothing [(∅)] [Action A.Launch9] Nothing
    , Key P.LaunchA Nothing [(∅)] [Action A.LaunchA] Nothing
    , Key P.LaunchB Nothing [(∅)] [Action A.LaunchB] Nothing
    , Key P.LaunchC Nothing [(∅)] [Action A.LaunchC] Nothing
    , Key P.LaunchD Nothing [(∅)] [Action A.LaunchD] Nothing
    , Key P.LaunchE Nothing [(∅)] [Action A.LaunchE] Nothing
    , Key P.LaunchF Nothing [(∅)] [Action A.LaunchF] Nothing
    ]


qwertyKeys ∷ [Key]
qwertyKeys =
    [ Key P.Tilde Nothing [(∅), WP.singleton M.Shift] [Char '`', Char '~'] Nothing
    , Key P.N1 Nothing [(∅), WP.singleton M.Shift] [Char '1', Char '!'] Nothing
    , Key P.N2 Nothing [(∅), WP.singleton M.Shift] [Char '2', Char '@'] Nothing
    , Key P.N3 Nothing [(∅), WP.singleton M.Shift] [Char '3', Char '#'] Nothing
    , Key P.N4 Nothing [(∅), WP.singleton M.Shift] [Char '4', Char '$'] Nothing
    , Key P.N5 Nothing [(∅), WP.singleton M.Shift] [Char '5', Char '%'] Nothing
    , Key P.N6 Nothing [(∅), WP.singleton M.Shift] [Char '6', Char '^'] Nothing
    , Key P.N7 Nothing [(∅), WP.singleton M.Shift] [Char '7', Char '&'] Nothing
    , Key P.N8 Nothing [(∅), WP.singleton M.Shift] [Char '8', Char '*'] Nothing
    , Key P.N9 Nothing [(∅), WP.singleton M.Shift] [Char '9', Char '('] Nothing
    , Key P.N0 Nothing [(∅), WP.singleton M.Shift] [Char '0', Char ')'] Nothing
    , Key P.Minus Nothing [(∅), WP.singleton M.Shift] [Char '-', Char '_'] Nothing
    , Key P.Plus Nothing [(∅), WP.singleton M.Shift] [Char '=', Char '+'] Nothing

    , Key P.Q Nothing [(∅), WP.singleton M.Shift] [Char 'q', Char 'Q'] Nothing
    , Key P.W Nothing [(∅), WP.singleton M.Shift] [Char 'w', Char 'W'] Nothing
    , Key P.E Nothing [(∅), WP.singleton M.Shift] [Char 'e', Char 'E'] Nothing
    , Key P.R Nothing [(∅), WP.singleton M.Shift] [Char 'r', Char 'R'] Nothing
    , Key P.T Nothing [(∅), WP.singleton M.Shift] [Char 't', Char 'T'] Nothing
    , Key P.Y Nothing [(∅), WP.singleton M.Shift] [Char 'y', Char 'Y'] Nothing
    , Key P.U Nothing [(∅), WP.singleton M.Shift] [Char 'u', Char 'U'] Nothing
    , Key P.I Nothing [(∅), WP.singleton M.Shift] [Char 'i', Char 'I'] Nothing
    , Key P.O Nothing [(∅), WP.singleton M.Shift] [Char 'o', Char 'O'] Nothing
    , Key P.P Nothing [(∅), WP.singleton M.Shift] [Char 'p', Char 'P'] Nothing
    , Key P.Bracket_L Nothing [(∅), WP.singleton M.Shift] [Char '[', Char '{'] Nothing
    , Key P.Bracket_R Nothing [(∅), WP.singleton M.Shift] [Char ']', Char '}'] Nothing
    , Key P.Backslash Nothing [(∅), WP.singleton M.Shift] [Char '\\', Char '|'] Nothing

    , Key P.A Nothing [(∅), WP.singleton M.Shift] [Char 'a', Char 'A'] Nothing
    , Key P.S Nothing [(∅), WP.singleton M.Shift] [Char 's', Char 'S'] Nothing
    , Key P.D Nothing [(∅), WP.singleton M.Shift] [Char 'd', Char 'D'] Nothing
    , Key P.F Nothing [(∅), WP.singleton M.Shift] [Char 'f', Char 'F'] Nothing
    , Key P.G Nothing [(∅), WP.singleton M.Shift] [Char 'g', Char 'G'] Nothing
    , Key P.H Nothing [(∅), WP.singleton M.Shift] [Char 'h', Char 'H'] Nothing
    , Key P.J Nothing [(∅), WP.singleton M.Shift] [Char 'j', Char 'J'] Nothing
    , Key P.K Nothing [(∅), WP.singleton M.Shift] [Char 'k', Char 'K'] Nothing
    , Key P.L Nothing [(∅), WP.singleton M.Shift] [Char 'l', Char 'L'] Nothing
    , Key P.Semicolon Nothing [(∅), WP.singleton M.Shift] [Char ';', Char ':'] Nothing
    , Key P.Apastrophe Nothing [(∅), WP.singleton M.Shift] [Char '\'', Char '"'] Nothing

    , Key P.Z Nothing [(∅), WP.singleton M.Shift] [Char 'z', Char 'Z'] Nothing
    , Key P.X Nothing [(∅), WP.singleton M.Shift] [Char 'x', Char 'X'] Nothing
    , Key P.C Nothing [(∅), WP.singleton M.Shift] [Char 'c', Char 'C'] Nothing
    , Key P.V Nothing [(∅), WP.singleton M.Shift] [Char 'v', Char 'V'] Nothing
    , Key P.B Nothing [(∅), WP.singleton M.Shift] [Char 'b', Char 'B'] Nothing
    , Key P.N Nothing [(∅), WP.singleton M.Shift] [Char 'n', Char 'N'] Nothing
    , Key P.M Nothing [(∅), WP.singleton M.Shift] [Char 'm', Char 'M'] Nothing
    , Key P.Comma Nothing [(∅), WP.singleton M.Shift] [Char ',', Char '<'] Nothing
    , Key P.Period Nothing [(∅), WP.singleton M.Shift] [Char '.', Char '>'] Nothing
    , Key P.Slash Nothing [(∅), WP.singleton M.Shift] [Char '/', Char '?'] Nothing
    ]
