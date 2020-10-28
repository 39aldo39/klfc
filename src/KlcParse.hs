{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module KlcParse
    ( parseKlcLayout
    ) where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, (>$>), lookupR, tellMaybeT)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (runWriterT, writer, tell)
import qualified Data.Text.Lazy as L (Text)
import Lens.Micro.Platform (ASetter, view, set, over, makeLenses, ix, _1)
import Text.Megaparsec hiding (Pos, many, some)
import Text.Megaparsec.Char

import Layout.Key (Key(..), baseCharToChar)
import Layout.Layout (Layout(..))
import Layout.Types
import Lookup.Windows
import WithBar (WithBar(..))

type Parser = MonadParsec Void L.Text

data KlcParseLayout = KlcParseLayout
    { __parseInformation ∷ Information
    , __parseShiftstates ∷ [Shiftstate]
    , __parseKeys ∷ [Key]
    , __parseLigatures ∷ [(Pos, Int, String)]
    , __parseDeadKeys ∷ [(Char, ActionMap)]
    }
makeLenses ''KlcParseLayout

instance Semigroup KlcParseLayout where
    KlcParseLayout a1 a2 a3 a4 a5 <> KlcParseLayout b1 b2 b3 b4 b5 =
        KlcParseLayout (a1 ⊕ b1) (a2 ⊕ b2) (a3 ⊕ b3) (a4 ⊕ b4) (a5 ⊕ b5)

instance Monoid KlcParseLayout where
    mempty = KlcParseLayout (∅) (∅) (∅) (∅) (∅)
    mappend = (<>)

layout ∷ (Logger m, Parser m, MonadFail m) ⇒ m Layout
layout = do
    KlcParseLayout info states keys ligs deads ← klcLayout
    ($ keys) $
      map (set _shiftlevels (map (WithBar ∘ (:| [])) states)) >>>
      Layout info (∅) (∅) (∅) >>>
      setDeads deads >$>
      setLigatures ligs

setDeads ∷ Logger m ⇒ [(Char, ActionMap)] → Layout → m Layout
setDeads = _keys ∘ traverse ∘ _letters ∘ traverse ∘ setDeadKey

setDeadKey ∷ Logger m ⇒ [(Char, ActionMap)] → Letter → m Letter
setDeadKey deads dead@(CustomDead i d) =
  case find ((≡) (__baseChar d) ∘ BaseChar ∘ fst) deads of
    Just (_, m) → pure (CustomDead i d { __actionMap = m })
    Nothing → dead <$ tell ["dead key ‘" ⊕ c ⊕ "’ is not defined"]
  where
    c = maybe "unknown" (:[]) (baseCharToChar (__baseChar d))
setDeadKey _ l = pure l

setLigatures ∷ [(Pos, Int, String)] → Layout → Layout
setLigatures = over (_keys ∘ traverse) ∘ setLigatures'

setLigatures' ∷ [(Pos, Int, String)] → Key → Key
setLigatures' xs key = foldr setLigature key ligs
  where
    ligs = filter ((≡) (view _pos key) ∘ view _1) xs
    setLigature (_, i, s) = set (_letters ∘ ix i) (Ligature Nothing s)

klcLayout ∷ (Logger m, Parser m, MonadFail m) ⇒ m KlcParseLayout
klcLayout = many >$> mconcat $
        set' _parseInformation <$> try kbdField
    <|> set' _parseShiftstates <$> try shiftstates
    <|> set' _parseKeys <$> klcKeys
    <|> set' _parseLigatures <$> try ligatures
    <|> set' _parseDeadKeys <$> try deadKey
    <|> (∅) <$ try keyName
    <|> (∅) <$ try endKbd
    <|> (try nameValue >>= (uncurry field >$> set' _parseInformation))
    <|> (readLine >>= \xs → (∅) <$ tell ["uknown line ‘" ⊕ show xs ⊕ "’"])
  where
    set' ∷ Monoid α ⇒ ASetter α α' β β' → β' → α'
    set' f = flip (set f) (∅)
    field ∷ Logger m ⇒ String → String → m Information
    field "COPYRIGHT" = pure ∘ set' _copyright ∘ Just
    field "COMPANY" = pure ∘ set' _company ∘ Just
    field "LOCALEID" = pure ∘ set' _localeId ∘ Just
    field "VERSION" = pure ∘ set' _version ∘ Just
    field f = const $ (∅) <$ tell ["unknown field ‘" ⊕ f ⊕ "’"]

kbdField ∷ (Parser m, MonadFail m) ⇒ m Information
kbdField = do
    ["KBD", l1, l2] ← readLine
    pure ∘ set _name l1 ∘ set _fullName l2 $ (∅)

shiftstates ∷ (Parser m, MonadFail m) ⇒ m [Shiftstate]
shiftstates = do
    ["SHIFTSTATE"] ← readLine
    map shiftstateFromWinShiftstate <$> many (try shiftstate)

shiftstate ∷ (Parser m, MonadFail m) ⇒ m Int
shiftstate = do
    [i] ← readLine
    maybe (fail $ "‘" ⊕ show i ⊕ "’ is not an integer") pure (readMaybe i)

klcKeys ∷ (Logger m, Parser m) ⇒ m [Key]
klcKeys = do
    try $ spacing *> string "LAYOUT" *> endLine *> pure ()
    catMaybes <$> many (isHex *> klcKey)

klcKey ∷ (Logger m, Parser m) ⇒ m (Maybe Key)
klcKey = runMaybeT $ do
    sc:vk:caps:letters ← lift readLine
    Key
      <$> parseScancode sc
      <*> (Just <$> parseShortcutPos vk)
      <*> pure []
      <*> traverse parseLetter letters
      <*> (Just <$> parseCapslock caps)

parseScancode ∷ Logger m ⇒ String → MaybeT m Pos
parseScancode xs = maybe e pure (readMaybe ('0':'x':xs) >>= (`lookupR` posAndScancode))
  where e = tellMaybeT ["unknown position ‘" ⊕ xs ⊕ "’"]

parseShortcutPos ∷ Logger m ⇒ String → MaybeT m Pos
parseShortcutPos xs = maybe e pure (lookupR xs posAndVkString <|> parseString xs)
  where e = tellMaybeT ["unknown position ‘" ⊕ xs ⊕ "’"]

parseCapslock ∷ Logger m ⇒ String → MaybeT m Bool
parseCapslock xs = maybe e (pure ∘ flip testBit 0) (readMaybe xs ∷ Maybe Int)
  where e = tellMaybeT ["‘" ⊕ xs ⊕ "’ is not a boolean value"]

parseLetter ∷ Logger m ⇒ String → m Letter
parseLetter "" = pure LNothing
parseLetter "-1" = pure LNothing
parseLetter [x] = pure (Char x)
parseLetter "%%" = pure LNothing
parseLetter xs
    | last xs ≡ '@' =
        case chr <$> readMaybe ('0':'x':init xs) of
            Just c → pure (CustomDead Nothing (DeadKey [c] (BaseChar c) (∅)))
            Nothing → LNothing <$ tell ["no number in dead key ‘" ⊕ xs ⊕ "’"]
    | otherwise =
        case chr <$> readMaybe ('0':'x':xs) of
          Just c → pure (Char c)
          Nothing → LNothing <$ tell ["unknown letter ‘" ⊕ xs ⊕ "’"]

ligatures ∷ (Logger m, Parser m, MonadFail m) ⇒ m [(Pos, Int, String)]
ligatures = do
    ["LIGATURE"] ← readLine
    catMaybes <$> many (try ligature)

ligature ∷ (Logger m, Parser m) ⇒ m (Maybe (Pos, Int, String))
ligature = runMaybeT $ do
    sc:i:chars ← lift readLine
    guard (not (null chars))
    pos ← parseShortcutPos sc
    i' ← maybe (tellMaybeT ["unknown index ‘" ⊕ i ⊕ "’"]) pure $ readMaybe ('0':'x':i)
    s ← mapMaybe letterToChar <$> traverse parseLetter chars
    pure (pos, i', s)
  where
    letterToChar (Char c) = Just c
    letterToChar _ = Nothing

deadKey ∷ (Logger m, Parser m, MonadFail m) ⇒ m [(Char, ActionMap)]
deadKey = do
    ["DEADKEY", s] ← readLine
    let i = maybeToList (readMaybe ('0':'x':s))
    c ← chr <$> i <$ when (null i) (tell ["unknown dead key ‘" ⊕ s ⊕ "’"])
    m ← many (isHex *> deadPair)
    pure (zip c [m])

deadPair ∷ (Parser m, MonadFail m) ⇒ m (Letter, ActionResult)
deadPair = do
    [x, y] ← map (\s → maybe '\0' chr (readMaybe ('0':'x':s))) <$> readLine
    pure (Char x, OutString [y])

keyName ∷ (Parser m, MonadFail m) ⇒ m [(String, String)]
keyName = do
    ['K':'E':'Y':'N':'A':'M':'E':_] ← readLine
    many (try nameValue)

endKbd ∷ (Parser m, MonadFail m) ⇒ m ()
endKbd = do
    ["ENDKBD"] ← readLine
    pure ()

nameValue ∷ (Parser m, MonadFail m) ⇒ m (String, String)
nameValue = do
    [name, value] ← readLine
    pure (name, value)

readLine ∷ Parser m ⇒ m [String]
readLine = takeWhile (not ∘ isComment) <$> some (klcValue <* spacing) <* emptyOrCommentLines
  where
    isComment (';':_) = True
    isComment ('/':'/':_) = True
    isComment _ = False

klcValue ∷ Parser m ⇒ m String
klcValue = try (char '"' *> manyTill anySingle (char '"')) <|> try (some (noneOf [' ','\t','\r','\n'])) <?> "klc value"

isHex ∷ Parser m ⇒ m Char
isHex = (lookAhead ∘ try) (spacing *> satisfy ((∧) <$> isHexDigit <*> not ∘ isUpper))

spacing ∷ Parser m ⇒ m String
spacing = many (oneOf [' ','\t'])

comment ∷ Parser m ⇒ m String
comment = spacing *> (string ";" <|> string "//") *> manyTill anySingle (try eol)

endLine ∷ Parser m ⇒ m String
endLine = manyTill anySingle (try eol) <* emptyOrCommentLines

emptyLine ∷ Parser m ⇒ m String
emptyLine = spacing <* eol

emptyOrCommentLines ∷ Parser m ⇒ m [String]
emptyOrCommentLines = many (try emptyLine <|> try comment)

parseKlcLayout ∷ Logger m ⇒ String → L.Text → Either String (m Layout)
parseKlcLayout fname =
    parse (runWriterT (emptyOrCommentLines *> layout <* eof)) fname >>>
    bimap errorBundlePretty writer
