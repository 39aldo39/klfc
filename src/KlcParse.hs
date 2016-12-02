{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module KlcParse
    ( parseKlcLayout
    ) where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, (>$>), lookupR)

import Control.Monad.Writer (tell)
import Data.Functor.Compose (Compose(..), getCompose)
import qualified Data.Text.Lazy as L (Text)
import Lens.Micro.Platform (ASetter, view, set, over, makeLenses, ix, _1, _Left)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Text.Lazy (Parser)

import Layout.Key (Key(Key))
import Layout.Layout (Layout(Layout))
import Layout.Types
import Lookup.Linux (posAndScancode)
import Lookup.Windows (shiftstateFromWinShiftstate, posAndString)

data KlcParseLayout = KlcParseLayout
    { __parseInformation ∷ Information
    , __parseShiftstates ∷ [Shiftstate]
    , __parseKeys ∷ [Key]
    , __parseLigatures ∷ [(Pos, Int, String)]
    , __parseDeadKeys ∷ [(Char, StringMap)]
    }
makeLenses ''KlcParseLayout
instance Monoid KlcParseLayout where
    mempty = KlcParseLayout (∅) (∅) (∅) (∅) (∅)
    KlcParseLayout a1 a2 a3 a4 a5 `mappend` KlcParseLayout b1 b2 b3 b4 b5 =
        KlcParseLayout (a1 ⊕ b1) (a2 ⊕ b2) (a3 ⊕ b3) (a4 ⊕ b4) (a5 ⊕ b5)

layout ∷ Parser (Logger Layout)
layout = flip fmap klcLayout $ \logger → do
    KlcParseLayout info states keys ligs deads ← logger
    ( map (set _shiftstates states) >>>
      Layout info (∅) (∅) >>>
      setDeads deads >$>
      setLigatures ligs
      ) keys

setDeads ∷ [(Char, StringMap)] → Layout → Logger Layout
setDeads = _keys ∘ traverse ∘ _letters ∘ traverse ∘ setDeadKey

setDeadKey ∷ [(Char, StringMap)] → Letter → Logger Letter
setDeadKey deads dead@(CustomDead i d) =
  case find ((≡) (__baseChar d) ∘ Just ∘ fst) deads of
    Just (_, m) → pure (CustomDead i d { __stringMap = m })
    Nothing → dead <$ tell ["dead key ‘" ⊕ c ⊕ "’ is not defined"]
  where
    c = maybe "unknown" (:[]) (__baseChar d)
setDeadKey _ l = pure l

setLigatures ∷ [(Pos, Int, String)] → Layout → Layout
setLigatures = over (_keys ∘ traverse) ∘ setLigatures'

setLigatures' ∷ [(Pos, Int, String)] → Key → Key
setLigatures' xs key = foldr setLigature key ligs
  where
    ligs = filter ((≡) (view _pos key) ∘ view _1) xs
    setLigature (_, i, s) = set (_letters ∘ ix i) (Ligature Nothing s)

klcLayout ∷ Parser (Logger KlcParseLayout)
klcLayout = fmap (fmap mconcat ∘ sequence) ∘ many $
        pure ∘ set' _parseInformation <$> try kbdField
    <|> pure ∘ set' _parseShiftstates <$> try shiftstates
    <|> fmap (set' _parseKeys) <$> klcKeys
    <|> fmap (set' _parseLigatures) <$> try ligatures
    <|> fmap (set' _parseDeadKeys) <$> try deadKey
    <|> pure (∅) <$ try keyName
    <|> pure (∅) <$ try endKbd
    <|> fmap (set' _parseInformation) ∘ uncurry field <$> try nameValue
    <|> (\xs → (∅) <$ tell ["uknown line ‘" ⊕ show xs ⊕ "’"]) <$> readLine
  where
    set' ∷ Monoid α ⇒ ASetter α α' β β' → β' → α'
    set' f = flip (set f) (∅)
    field ∷ String → String → Logger Information
    field "COPYRIGHT" = pure ∘ set' _copyright ∘ Just
    field "COMPANY" = pure ∘ set' _company ∘ Just
    field "LOCALEID" = pure ∘ set' _localeId ∘ Just
    field "VERSION" = pure ∘ set' _version ∘ Just
    field f = const $ (∅) <$ tell ["unknown field ‘" ⊕ f ⊕ "’"]

kbdField ∷ Parser Information
kbdField = do
    ["KBD", l1, l2] ← readLine
    pure ∘ set _name l1 ∘ set _fullName l2 $ (∅)

shiftstates ∷ Parser [Shiftstate]
shiftstates = do
    ["SHIFTSTATE"] ← readLine
    map shiftstateFromWinShiftstate <$> many (try shiftstate)

shiftstate ∷ Parser Int
shiftstate = do
    [i] ← readLine
    maybe (fail $ "‘" ⊕ show i ⊕ "’ is not an integer") pure (readMaybe i)

klcKeys ∷ Parser (Logger [Key])
klcKeys = do
    try $ spacing *> string "LAYOUT" *> endLine *> pure ()
    fmap catMaybes ∘ sequence <$> many (isHex *> klcKey)

klcKey ∷ Parser (Logger (Maybe Key))
klcKey = do
    sc:vk:caps:letters ← readLine
    pure ∘ getCompose $
      Key
        <$> parseScancode sc
        <*> (Just <$> parseShortcutPos vk)
        <*> pure []
        <*> (Compose $ Just <$> traverse parseLetter letters)
        <*> (Just <$> parseCapslock caps)

parseScancode ∷ String → Compose Logger Maybe Pos
parseScancode xs =
    let pos = readMaybe ('0':'x':xs) >>= (`lookupR` posAndScancode)
    in Compose $ pos <$ when (isNothing pos) (tell ["unknown position ‘" ⊕ xs ⊕ "’"])

parseShortcutPos ∷ String → Compose Logger Maybe Pos
parseShortcutPos xs =
    let pos = lookupR xs posAndString <|> parseString xs
    in Compose $ pos <$ when (isNothing pos) (tell ["unknown position ‘" ⊕ xs ⊕ "’"])

parseCapslock ∷ String → Compose Logger Maybe Bool
parseCapslock xs =
    let i = readMaybe xs
    in Compose $ toEnum <$> i <$ when (isNothing i) (tell ["‘" ⊕ xs ⊕ "’ is not a boolean value"])

parseLetter ∷ String → Logger Letter
parseLetter "" = pure LNothing
parseLetter "-1" = pure LNothing
parseLetter [x] = pure (Char x)
parseLetter "%%" = pure LNothing
parseLetter xs
    | last xs ≡ '@' =
        case chr <$> readMaybe ('0':'x':init xs) of
            Just c → pure (CustomDead Nothing (DeadKey [c] (Just c) (∅)))
            Nothing → LNothing <$ tell ["no number in dead key ‘" ⊕ xs ⊕ "’"]
    | otherwise =
        case chr <$> readMaybe ('0':'x':xs) of
          Just c → pure (Char c)
          Nothing → LNothing <$ tell ["unknown letter ‘" ⊕ xs ⊕ "’"]

ligatures ∷ Parser (Logger [(Pos, Int, String)])
ligatures = do
    ["LIGATURE"] ← readLine
    fmap catMaybes ∘ sequence <$> many (try ligature)

ligature ∷ Parser (Logger (Maybe (Pos, Int, String)))
ligature = do
    sc:i:chars ← readLine
    guard (not (null chars))
    pure $ do
        pos ← getCompose $ parseShortcutPos sc
        let i' = readMaybe ('0':'x':i)
        when (isNothing pos) (tell ["unknown index ‘" ⊕ i ⊕ "’"])
        s ← mapMaybe letterToChar <$> traverse parseLetter chars
        pure $ liftA3 (,,) pos i' (pure s)
  where
    letterToChar (Char c) = Just c
    letterToChar _ = Nothing

deadKey ∷ Parser (Logger [(Char, StringMap)])
deadKey = do
    ["DEADKEY", s] ← readLine
    let i = maybeToList (readMaybe ('0':'x':s))
    let c = chr <$> i <$ when (null i) (tell ["unknown dead key ‘" ⊕ s ⊕ "’"])
    m ← many (isHex *> deadPair)
    pure (liftA2 zip c (pure [m]))

deadPair ∷ Parser (String, String)
deadPair = do
    [x, y] ← map (\s → maybe '\0' chr (readMaybe ('0':'x':s))) <$> readLine
    pure ([x], [y])

keyName ∷ Parser [(String, String)]
keyName = do
    ['K':'E':'Y':'N':'A':'M':'E':_] ← readLine
    many (try nameValue)

endKbd ∷ Parser ()
endKbd = do
    ["ENDKBD"] ← readLine
    pure ()

nameValue ∷ Parser (String, String)
nameValue = do
    [name, value] ← readLine
    pure (name, value)

readLine ∷ Parser [String]
readLine = takeWhile (not ∘ isComment) <$> some (klcValue <* spacing) <* emptyOrCommentLines
  where
    isComment (';':_) = True
    isComment ('/':'/':_) = True
    isComment _ = False

klcValue ∷ Parser String
klcValue = try (char '"' *> manyTill anyChar (char '"')) <|> try (some (noneOf " \t\r\n")) <?> "klc value"

isHex ∷ Parser Char
isHex = (lookAhead ∘ try) (spacing *> satisfy ((∧) <$> isHexDigit <*> not ∘ isUpper))

spacing ∷ Parser String
spacing = many (oneOf " \t")

comment ∷ Parser String
comment = spacing *> (string ";" <|> string "//") *> manyTill anyChar (try eol)

endLine ∷ Parser String
endLine = manyTill anyChar (try eol) <* emptyOrCommentLines

emptyLine ∷ Parser String
emptyLine = spacing <* eol

emptyOrCommentLines ∷ Parser [String]
emptyOrCommentLines = many (try emptyLine <|> try comment)

parseKlcLayout ∷ String → L.Text → Either String (Logger Layout)
parseKlcLayout fname =
    parse (emptyOrCommentLines *> layout <* eof) fname >>>
    over _Left parseErrorPretty
