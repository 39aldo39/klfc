{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module Xkb.Keycodes where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((⊕))
import Util (toString, show', concatMapM)

import Control.Monad.Writer (tell)
import Lens.Micro.Platform (view)

import Layout.Types
import Lookup.Linux (posToScancode, posAndKeycode)
import Permutation (assocs)

printKeycodes ∷ Layout → Logger String
printKeycodes = fmap unlines ∘ concatMapM printKeycode ∘ view _mods

printKeycode ∷ Mod → Logger [String]
printKeycode (Mod nameM per) = do
    let (logs, s) = partitionEithers (map (uncurry printKeycodePos) (assocs per))
    tell logs
    pure $
        [ "partial xkb_keycodes " ⊕ show nameM ⊕ " {"
        ] ⧺ map (replicate 4 ' ' ⊕) s ⧺
        [ "};"
        ]

printKeycodePos ∷ Pos → Pos → Either String String
printKeycodePos fromPos toPos =
    liftA2 (showKeycodePos fromPos toPos) fromPos' toPos'
  where
    fromPos' = maybe e (Right ∘ show) (posToScancode fromPos)
      where e = Left ("the position " ⊕ show' fromPos ⊕ " is not supported in XKB")
    toPos' = maybe e Right (lookup toPos posAndKeycode)
      where e = Left ("the position " ⊕ show' toPos ⊕ " is not supported in XKB")

showKeycodePos ∷ Pos → Pos → String → String → String
showKeycodePos fromPos toPos fromPosString toPosString =
    toPosString ⊕ " = " ⊕ fromPosString ⊕ ";\t// " ⊕ toString toPos ⊕ " ← " ⊕ toString fromPos
