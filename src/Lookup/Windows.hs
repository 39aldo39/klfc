{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Lookup.Windows
    ( WinShiftstate
    , modifierAndWinShiftstate
    , winShiftstateFromShiftstate
    , shiftstateFromWinShiftstate
    , isAltRToAltGr
    , altGrToControlAlt
    , posAndString
    , modifierAndChar
    , PklAction(..)
    , actionAndPklAction
    , modifierAndPklAction
    ) where

import BasePrelude
import Prelude.Unicode hiding ((∈))
import Data.Foldable.Unicode ((∈))
import qualified Data.Set.Unicode as S
import Util (lookupR, dec2bin)
import WithPlus (WithPlus(..))
import qualified WithPlus as WP (fromList)

import qualified Data.Set as S

import qualified Layout.Action as A
import qualified Layout.Modifier as M
import qualified Layout.Pos as P
import Layout.Types

type WinShiftstate = Int
modifierAndWinShiftstate ∷ [(Modifier, WinShiftstate)]
modifierAndWinShiftstate =
    [ (M.Shift, 1)
    , (M.Control, 2)
    , (M.Alt, 4)
    ]

winShiftstateFromShiftstate ∷ Shiftstate → WinShiftstate
winShiftstateFromShiftstate = sum ∘ mapMaybe (`lookup` modifierAndWinShiftstate) ∘ toList

shiftstateFromWinShiftstate ∷ WinShiftstate → Shiftstate
shiftstateFromWinShiftstate = WP.fromList ∘ catMaybes ∘ zipWith toModifier (iterate (⋅2) 1) ∘ dec2bin
    where toModifier _ 0 = Nothing
          toModifier n _ = lookupR n modifierAndWinShiftstate

isAltRToAltGr ∷ SingletonKey → Bool
isAltRToAltGr (P.Alt_R, Modifiers Shift [M.AltGr]) = True
isAltRToAltGr _ = False

altGrToControlAlt ∷ Shiftstate → Shiftstate
altGrToControlAlt xs@(WithPlus s)
  | M.AltGr ∈ xs = WithPlus (S.delete M.AltGr s S.∪ S.fromList [M.Control, M.Alt])
  | otherwise    = xs

posAndString ∷ [(Pos, String)]
posAndString =
    [ (P.Esc, "ESCAPE")
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
    , (P.PrintScreen, "SNAPSHOT")
    , (P.ScrollLock, "SCROLL")
    , (P.Pause, "PAUSE")

    , (P.Tilde, "OEM_3")
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
    , (P.Minus, "OEM_MINUS")
    , (P.Plus, "OEM_PLUS")
    , (P.Backspace, "BACK")

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
    , (P.Bracket_L, "OEM_4")
    , (P.Bracket_R, "OEM_6")
    , (P.Backslash, "OEM_5")

    , (P.CapsLock, "OEM_CAPITAL")
    , (P.A, "A")
    , (P.S, "S")
    , (P.D, "D")
    , (P.F, "F")
    , (P.G, "G")
    , (P.H, "H")
    , (P.J, "J")
    , (P.K, "K")
    , (P.L, "L")
    , (P.Semicolon, "OEM_1")
    , (P.Apastrophe, "OEM_7")
    , (P.Enter, "RETURN")

    , (P.Shift_L, "LSHIFT")
    , (P.Iso, "OEM_102")
    , (P.Z, "Z")
    , (P.X, "X")
    , (P.C, "C")
    , (P.V, "V")
    , (P.B, "B")
    , (P.N, "N")
    , (P.M, "M")
    , (P.Comma, "OEM_COMMA")
    , (P.Period, "OEM_PERIOD")
    , (P.Slash, "OEM_2")
    , (P.Shift_R, "RSHIFT")

    , (P.Control_L, "LCONTROL")
    , (P.Win_L, "LWIN")
    , (P.Alt_L, "LALT")
    , (P.Space, "SPACE")
    , (P.Alt_R, "RAlt")
    , (P.Win_R, "RWIN")
    , (P.Menu, "APPS")
    , (P.Control_R, "RCONTROL")

    , (P.Insert, "INSERT")
    , (P.Delete, "DELETE")
    , (P.Home, "HOME")
    , (P.End, "END")
    , (P.PageUp, "PRIOR")
    , (P.PageDown, "NEXT")
    , (P.Up, "UP")
    , (P.Left, "LEFT")
    , (P.Down, "DOWN")
    , (P.Right, "RIGHT")

    , (P.NumLock, "NUMLOCK")
    , (P.KP_Div, "DIVIDE")
    , (P.KP_Mult, "MULTIPLY")
    , (P.KP_Min, "SUBTRACT")
    , (P.KP_7, "NUMPAD7")
    , (P.KP_8, "NUMPAD8")
    , (P.KP_9, "NUMPAD9")
    , (P.KP_Plus, "ADD")
    , (P.KP_4, "NUMPAD4")
    , (P.KP_5, "NUMPAD5")
    , (P.KP_6, "NUMPAD6")
    , (P.KP_1, "NUMPAD1")
    , (P.KP_2, "NUMPAD2")
    , (P.KP_3, "NUMPAD3")
    , (P.KP_Enter, "RETURN")
    , (P.KP_0, "NUMPAD0")
    , (P.KP_Dec, "DECIMAL")
--    , (P.KP_Eq, "")

    , (P.PlayPause, "MEDIA_PLAY_PAUSE")
    , (P.Previous, "MEDIA_PREV_TRACK")
    , (P.Next, "MEDIA_NEXT_TRACK")
    , (P.Stop, "MEDIA_STOP")
    , (P.Mute, "VOLUME_MUTE")
    , (P.VolumeDown, "VOLUME_DOWN")
    , (P.VolumeUp, "VOLUME_UP")

    , (P.Browser_Back, "BROWSER_BACK")
    , (P.Browser_Forward, "BROWSER_FORWARD")
    , (P.Browser_Refresh, "BROWSER_REFRESH")
    , (P.Browser_Stop, "BROWSER_STOP")
    , (P.Browser_Search, "BROWSER_SEARCH")
    , (P.Browser_Favorites, "BROWSER_FAVORITES")

    , (P.Calculator, "LAUNCH_APP2")
    , (P.MediaPlayer, "LAUNCH_MEDIA_SELECT")
    , (P.Browser, "BROWSER_HOME")
    , (P.Mail, "LAUNCH_MAIL")
--    , (P.Search, "")
    , (P.Launch1, "LAUNCH_APP1")
    , (P.Launch2, "LAUNCH_APP2")

--    , (P.Power, "")
    , (P.Sleep, "SLEEP")
--    , (P.Wake, "")

    ]

modifierAndChar ∷ [(Modifier, Char)]
modifierAndChar =
    [ (M.Shift, '+')
    , (M.Control, '^')
    , (M.Alt, '!')
    , (M.Win, '#')
    ]

data PklAction
    = Simple String
    | RedirectLetter Letter [Modifier]
    deriving (Eq, Show, Read)

actionAndPklAction ∷ [(Action, PklAction)]
actionAndPklAction =
    [ (A.Esc, Simple "Esc")
    , (A.F1, Simple "F1")
    , (A.F2, Simple "F2")
    , (A.F3, Simple "F3")
    , (A.F4, Simple "F4")
    , (A.F5, Simple "F5")
    , (A.F6, Simple "F6")
    , (A.F7, Simple "F7")
    , (A.F8, Simple "F8")
    , (A.F9, Simple "F9")
    , (A.F10, Simple "F10")
    , (A.F11, Simple "F11")
    , (A.F12, Simple "F12")
    , (A.PrintScreen, Simple "PrintScreen")
--    , (A.SysRq, Simple "")
    , (A.ScrollLock, Simple "ScrollLock")
    , (A.Pause, Simple "Pause")
    , (A.ControlBreak, Simple "CtrlBreak")
    , (A.Insert, Simple "Ins")
    , (A.Delete, Simple "Del")
    , (A.Home, Simple "Home")
    , (A.End, Simple "End")
    , (A.PageUp, Simple "PgUp")
    , (A.PageDown, Simple "PgDn")
    , (A.Up, Simple "Up")
    , (A.Left, Simple "Left")
    , (A.Down, Simple "Down")
    , (A.Right, Simple "Right")
    , (A.Backspace, Simple "BackSpace")
    , (A.Tab, Simple "Tab")
    , (A.Enter, Simple "Enter")
    , (A.Menu, Simple "AppsKey")
    , (A.Power, Simple "Power")
    , (A.Sleep, Simple "Sleep")
    , (A.Wake, Simple "Wake")
    , (A.Undo, RedirectLetter (Char 'z') [M.Control])
    , (A.Redo, RedirectLetter (Char 'z') [M.Shift,M.Control])
    , (A.Cut, RedirectLetter (Char 'x') [M.Control])
    , (A.Copy, RedirectLetter (Char 'c') [M.Control])
    , (A.Paste, RedirectLetter (Char 'v') [M.Control])
    , (A.Save, RedirectLetter (Char 's') [M.Control])
    , (A.CloseTab, RedirectLetter (Char 'w') [M.Control])
    , (A.PlayPause, Simple "Media_Play_Pause")
    , (A.Previous, Simple "Media_Prev")
    , (A.Next, Simple "Media_Next")
    , (A.Stop, Simple "Media_Stop")
    , (A.Mute, Simple "Volume_Mute")
    , (A.VolumeDown, Simple "Volume_Down")
    , (A.VolumeUp, Simple "Volume_Up")
--    , (A.BrightnessDown, Simple "")
--    , (A.BrightnessUp, Simple "")
    , (A.Button_Default, Simple "LBotton")
    , (A.Button_L, Simple "LButton")
    , (A.Button_M, Simple "MButton")
    , (A.Button_R, Simple "RButton")
    , (A.WheelDown, Simple "WheelDown")
    , (A.WheelUp, Simple "WheelUp")
    , (A.WheelLeft, Simple "WheelLeft")
    , (A.WheelRight, Simple "WheelRight")
--    , (A.DoubleClick, Simple "")
    , (A.MouseLeft, Simple "Click Rel -17,0,0")
    , (A.MouseRight, Simple "Click Rel 17,0,0")
    , (A.MouseUp, Simple "Click Rel 0,-17,0")
    , (A.MouseDown, Simple "Click Rel 0,17,0")
    , (A.MouseUpLeft, Simple "Click Rel -17,-17,0")
    , (A.MouseUpRight, Simple "Click Rel 17,-17,0")
    , (A.MouseDownLeft, Simple "Click Rel -17,17,0")
    , (A.MouseDownRight, Simple "Click Rel 17,17,0")

    , (A.Browser_Back, Simple "Browser_Back")
    , (A.Browser_Forward, Simple "Browser_Forward")
    , (A.Browser_Refresh, Simple "Browser_Refresh")
    , (A.Browser_Stop, Simple "Browser_Stop")
    , (A.Browser_Search, Simple "Browser_Search")
    , (A.Browser_Favorites, Simple "Browser_Favorites")

    , (A.Calculator, Simple "Launch_App2")
    , (A.MediaPlayer, Simple "Launch_Media")
    , (A.Browser, Simple "Browser_Home")
    , (A.Mail, Simple "Launch_Mail")
    , (A.Launch1, Simple "Launch_App1")
    , (A.Launch2, Simple "Launch_App2")

    , (A.KP_Div, Simple "NumpadDiv")
    , (A.KP_Mult, Simple "NumpadMult")
    , (A.KP_Min, Simple "NumpadSub")
    , (A.KP_7, Simple "Numpad7")
    , (A.KP_8, Simple "Numpad8")
    , (A.KP_9, Simple "Numpad9")
    , (A.KP_Plus, Simple "NumpadAdd")
    , (A.KP_4, Simple "Numpad4")
    , (A.KP_5, Simple "Numpad5")
    , (A.KP_6, Simple "Numpad6")
    , (A.KP_1, Simple "Numpad1")
    , (A.KP_2, Simple "Numpad2")
    , (A.KP_3, Simple "Numpad3")
    , (A.KP_Enter, Simple "NumpadEnter")
    , (A.KP_0, Simple "Numpad0")
    , (A.KP_Dec, Simple "NumpadDot")

    , (A.KP_Home, Simple "NumpadHome")
    , (A.KP_Up, Simple "NumpadUp")
    , (A.KP_PageUp, Simple "NumpadPgUp")
    , (A.KP_Left, Simple "NumpadLeft")
    , (A.KP_Begin, Simple "NumpadClear")
    , (A.KP_Right, Simple "NumpadRight")
    , (A.KP_End, Simple "NumpadEnd")
    , (A.KP_Down, Simple "NumpadDown")
    , (A.KP_PageDown, Simple "NumpadPgDn")
    , (A.KP_Insert, Simple "NumpadIns")
    , (A.KP_Delete, Simple "NumpadDel")
    ]

modifierAndPklAction ∷ [(Modifier, PklAction)]
modifierAndPklAction =
    [ (M.Shift, Simple "Shift")
    , (M.Shift_L, Simple "LShift")
    , (M.Shift_R, Simple "RShift")
    , (M.CapsLock, Simple "CapsLock")
    , (M.Win, Simple "LWin")
    , (M.Win_L, Simple "LWin")
    , (M.Win_R, Simple "RWin")
    , (M.Alt, Simple "Alt")
    , (M.Alt_L, Simple "LAlt")
    , (M.Alt_R, Simple "RAlt")
    , (M.Control, Simple "Ctrl")
    , (M.Control_L, Simple "LCtrl")
    , (M.Control_R, Simple "RCtrl")
    , (M.AltGr, Simple "RAlt")
    ]
