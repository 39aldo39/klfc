{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Layout.Action
    ( Action(..)
    , isModifier
    ) where

import BasePrelude
import Prelude.Unicode
import Util (HumanReadable(..))

import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)

data Action
    -- Top row
    = Esc | F1 | F2 | F3 | F4 | F5 | F6 | F7 | F8 | F9 | F10 | F11 | F12
    | PrintScreen | SysRq | ScrollLock | Pause | ControlBreak

    -- Movement
    | Insert | Delete | Home | End | PageUp | PageDown
    | Up | Left | Down | Right

    -- Miscellaneous
    | Backspace | Tab | LeftTab | Enter | Compose | Menu
    | Undo | Redo | Cut | Copy | Paste | Save | CloseTab

    -- Media control
    | AudioPlay | AudioPause | PlayPause | Previous | Next | Stop | ToggleRepeat | ToggleRandom
    | AudioRewind | AudioForward
    | Mute | VolumeDown | VolumeUp
    | Eject

    -- Browser control
    | Browser_Back | Browser_Forward | Browser_Refresh | Browser_Stop | Browser_Search | Browser_Favorites

    -- Applications
    | Calculator | MediaPlayer | Browser | Mail | Search | Explorer | WWW | MyComputer
    | Launch0 | Launch1 | Launch2 | Launch3 | Launch4 | Launch5 | Launch6 | Launch7
    | Launch8 | Launch9 | LaunchA | LaunchB | LaunchC | LaunchD | LaunchE | LaunchF

    -- Virtual terminals (XKB only)
    | Switch_VT_1 | Switch_VT_2 | Switch_VT_3 | Switch_VT_4 | Switch_VT_5 | Switch_VT_6
    | Switch_VT_7 | Switch_VT_8 | Switch_VT_9 | Switch_VT_10 | Switch_VT_11 | Switch_VT_12

    -- Power management
    | Power | Sleep | Wake | BrightnessDown | BrightnessUp

    -- Modifiers
    | Shift | Shift_L | Shift_R | CapsLock
    | Win | Win_L | Win_R
    | Alt | Alt_L | Alt_R
    | Control | Control_L | Control_R
    | NumLock | AltGr | Extend

    -- Mouse keys
    | Button_Default | Button_L | Button_M | Button_R
    | Button_DoubleClick_Default | Button_DoubleClick_L | Button_DoubleClick_M | Button_DoubleClick_R
    | Button_Drag_Default | Button_Drag_L | Button_Drag_M | Button_Drag_R
    | Button_SetDefault_Next | Button_SetDefault_Prev | Button_SetDefault_L | Button_SetDefault_M | Button_SetDefault_R
    | WheelUp | WheelDown | WheelLeft | WheelRight
    | MouseEnable
    | MouseUp | MouseDown | MouseLeft | MouseRight
    | MouseUpLeft | MouseUpRight | MouseDownLeft | MouseDownRight

    -- Numpad
    | KP_Clear | KP_Div | KP_Mult | KP_Min
    | KP_7     | KP_8   | KP_9    | KP_Plus
    | KP_4     | KP_5   | KP_6
    | KP_1     | KP_2   | KP_3    | KP_Enter
    |        KP_0       | KP_Dec  | KP_Eq

    | KP_Home | KP_Up    | KP_PageUp
    | KP_Left | KP_Begin | KP_Right
    | KP_End  | KP_Down  | KP_PageDown
    |     KP_Insert      | KP_Delete

    deriving (Eq, Ord, Show, Read, Enum, Bounded)

instance HumanReadable Action where
    typeName _ = "action"
    toString = show
instance ToJSON Action where
    toJSON = hrToJSON
instance FromJSON Action where
    parseJSON = hrParseJSON

isModifier ∷ Action → Bool
isModifier = (∈ [Shift .. NumLock])
