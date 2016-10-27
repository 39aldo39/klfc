{-
Modified version of the aeson-pretty library by Falko Peters.

Copyright (c) 2016, Aldo Gunsing
Copyright (c) 2011, Falko Peters

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Falko Peters nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module JsonPretty
    ( Config(..)
    , defConfig
    , encodePretty
    , encodePretty'
    , encodePrettyToTextBuilder
    , encodePrettyToTextBuilder'
    , keyOrder
    , keyOrder'
    , delimsFromList
    ) where

import BasePrelude
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))

import Data.Aeson (Value(..), ToJSON(..))
#if MIN_VERSION_aeson(1,0,0)
import qualified Data.Aeson.Text as A (encodeToTextBuilder)
#else
import qualified Data.Aeson.Encode as A (encodeToTextBuilder)
#endif
import qualified Data.HashMap.Strict as H (toList)
import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as L (Text)
import Data.Text.Lazy.Builder (Builder, toLazyText)

data PState = PState
    { pstIndent ∷ Int
    , pstLevel  ∷ Int
    , pstSort   ∷ [(T.Text, Value)] → [(T.Text, Value)]
    , pstDelims ∷ T.Text → [Builder]
    }

data Config = Config
    { confIndent  ∷ Int
      -- ^ Indentation spaces per level of nesting
    , confCompare ∷ T.Text → T.Text → Ordering
      -- ^ Function used to sort keys in objects
    , confDelims ∷ T.Text → [Builder]
    }

-- |Sort keys by their order of appearance in the argument list.
--
--  Keys that are not present in the argument list are considered to be greater
--  than any key in the list and equal to all keys not in the list. I.e. keys
--  not in the argument list are moved to the end, while their order is
--  preserved.
keyOrder ∷ [T.Text] → T.Text → T.Text → Ordering
keyOrder ks = comparing $ \k → fromMaybe maxBound (elemIndex k ks)

keyOrder' ∷ Ord α ⇒ [T.Text] → (T.Text → α) → T.Text → T.Text → Ordering
keyOrder' ks f = keyOrder ks ⊕ comparing f

delimsFromList ∷ [(T.Text, [Builder])] → T.Text → [Builder]
delimsFromList ls = fromMaybe [] ∘ flip lookup ls

-- |The default configuration: indent by four spaces per level of nesting, do
--  not sort objects by key.
--
--  > defConfig = Config { confIndent = 4, confCompare = mempty }
defConfig ∷ Config
defConfig = Config { confIndent = 4, confCompare = (∅), confDelims = const [] }

-- |A drop-in replacement for aeson's 'Aeson.encode' function, producing
--  JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePretty ∷ ToJSON α ⇒ α → L.Text
encodePretty = encodePretty' defConfig

-- |A variant of 'encodePretty' that takes an additional configuration
--  parameter.
encodePretty' ∷ ToJSON α ⇒ Config → α → L.Text
encodePretty' conf = toLazyText ∘ encodePrettyToTextBuilder' conf

-- |A drop-in replacement for aeson's 'Aeson.encodeToTextBuilder' function,
--  producing JSON-ByteStrings for human readers.
--
--  Follows the default configuration in 'defConfig'.
encodePrettyToTextBuilder ∷ ToJSON α ⇒ α → Builder
encodePrettyToTextBuilder = encodePrettyToTextBuilder' defConfig

-- |A variant of 'encodeToTextBuilder' that takes an additional configuration
--  parameter.
encodePrettyToTextBuilder' ∷ ToJSON α ⇒ Config → α → Builder
encodePrettyToTextBuilder' Config{..} = (⊕ "\n") ∘ fromValue [] st ∘ toJSON
  where
    st       = PState confIndent 0 condSort confDelims
    condSort = sortBy (confCompare `on` fst)


fromValue ∷ [Builder] → PState → Value → Builder
fromValue [] st = fromValue ["\n"] st
fromValue (delim:delims') st@PState{..} = go
  where
    go (Array v)  = fromCompound st ("[","]") delim (fromValue delims') (toList v)
    go (Object m) = fromCompound st ("{","}") delim (fromPair delims') (pstSort (H.toList m))
    go v          = A.encodeToTextBuilder v

fromCompound ∷ PState
             → (Builder, Builder)
             → Builder
             → (PState → α → Builder)
             → [α]
             → Builder
fromCompound st@PState{..} (delimL,delimR) delim fromItem items = mconcat
    [ delimL
    , if null items
        then (∅)
        else delim ⊕ items' ⊕ delim ⊕ fromIndent delim st
    , delimR
    ]
  where
    items' = mconcat ∘ intersperse ("," ⊕ delim) $
                map (\item → fromIndent delim st' ⊕ fromItem st' item)
                    items
    st' = st { pstLevel = succ pstLevel }

fromPair ∷ [Builder] → PState → (T.Text, Value) → Builder
fromPair delims st (k,v) =
    A.encodeToTextBuilder (toJSON k) ⊕ ": " ⊕ fromValue delims' st v
  where
    delims' =
      case pstDelims st k of
        [] → delims
        xs → xs

fromIndent ∷ Builder → PState → Builder
fromIndent "\n" PState{..} = mconcat (replicate (pstIndent ⋅ pstLevel) " ")
fromIndent _ _ = (∅)
