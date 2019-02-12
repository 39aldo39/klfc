{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PklParse
    ( parsePklLayout
    ) where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR, stripSuffix, tellMaybeT)
import qualified WithPlus as WP (singleton)

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (runWriterT, writer, tell)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Text.Lazy as L (Text)
import Data.Void (Void)
import Lens.Micro.Platform (set, (<&>))
import Text.Megaparsec hiding (Pos, many, some)
import Text.Megaparsec.Char

import Layout.Key (Key(Key))
import Layout.Layout (Layout(Layout))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Windows
import WithBar (WithBar(..))

type Parser = MonadParsec Void L.Text

layout ∷ (Logger m, Parser m) ⇒ m Layout
layout = do
    PklParseLayout info states keys specialKeys deads ← pklLayout
    ($ keys) $
      map (set _shiftlevels (map (WithBar ∘ (:| [])) states)) >>>
      Layout info specialKeys (∅) (∅) >>>
      setDeads deads

setDeads ∷ Logger m ⇒ [(Int, DeadKey)] → Layout → m Layout
setDeads = _keys ∘ traverse ∘ _letters ∘ traverse ∘ setDeadKey

setDeadKey ∷ Logger m ⇒ [(Int, DeadKey)] → Letter → m Letter
setDeadKey deads (CustomDead (Just i) _) =
  case find ((≡) i ∘ fst) deads of
    Just (_, d) → pure (CustomDead (Just i) d)
    Nothing → CustomDead (Just i) (DeadKey "" BaseNo []) <$ tell ["deadkey" ⊕ show i ⊕ " is not defined"]
setDeadKey _ l = pure l

data PklParseLayout = PklParseLayout
    { parseInformation ∷ Information
    , parseShiftstates ∷ [Shiftstate]
    , parseKeys ∷ [Key]
    , parseSingletonKeys ∷ [SingletonKey]
    , parseDeadKeys ∷ [(Int, DeadKey)]
    }

instance Semigroup PklParseLayout where
    PklParseLayout a1 a2 a3 a4 a5 <> PklParseLayout b1 b2 b3 b4 b5 =
        PklParseLayout (a1 ⊕ b1) (a2 ⊕ b2) (a3 ⊕ b3) (a4 ⊕ b4) (a5 ⊕ b5)

instance Monoid PklParseLayout where
    mempty = PklParseLayout (∅) (∅) (∅) (∅) (∅)
    mappend = (<>)

pklLayout ∷ (Logger m, Parser m) ⇒ m PklParseLayout
pklLayout = mconcat <$> many ((sectionName >>= section) <* many nonSectionLine)
  where
    sectionName = char '[' *> manyTill anySingle (char ']') <* endLine
    section ∷ (Logger m, Parser m) ⇒ String → m PklParseLayout
    section "informations" = (\l → (∅) { parseInformation = l }) <$> informationsSection
    section "global" = globalSection
    section "layout" = (\ks → (∅) { parseKeys = ks }) <$> layoutSection
    section "extend" = extendSection
    section xs
        | "deadkey" `isPrefixOf` xs ∧ isJust num =
              (\d → (∅) { parseDeadKeys = [(fromJust num, d)] }) <$> deadkeySection
        | otherwise = (∅) <$ tell ["unknown section ‘" ⊕ xs ⊕ "’"]
        where num = readMaybe (drop 7 xs) ∷ Maybe Int
    nonSectionLine = lookAhead (noneOf ['[']) *> many (noneOf ['\r','\n']) *> eol

informationsSection ∷ (Logger m, Parser m) ⇒ m Information
informationsSection = mconcat <$> (many nameValue >>= traverse (uncurry field))
  where
    field ∷ Logger m ⇒ String → String → m Information
    field "layoutname" = pure ∘ flip (set _fullName) (∅)
    field "layoutcode" = pure ∘ flip (set _name) (∅)
    field "localeid" = pure ∘ flip (set _localeId) (∅) ∘ Just
    field "copyright" = pure ∘ flip (set _copyright) (∅) ∘ Just
    field "company" = pure ∘ flip (set _company) (∅) ∘ Just
    field "version" = pure ∘ flip (set _version) (∅) ∘ Just
    field "homepage" = pure ∘ const (∅)
    field "generated_at" = pure ∘ const (∅)
    field "generated_from" = pure ∘ const (∅)
    field "modified_after_generate" = pure ∘ const (∅)
    field name = const $ (∅) <$ tell ["unknown information ‘" ⊕ name ⊕ "’"]

nameValue ∷ Parser m ⇒ m (String, String)
nameValue = liftA2 (,)
            (lookAhead (noneOf ['[']) *> some (noneOf ['=',' ','\t']))
            (spacing *> char '=' *> spacing *> endLine)

globalSection ∷ Parser m ⇒ m PklParseLayout
globalSection = mconcat ∘ map (uncurry field) <$> many nameValue
  where
    field ∷ String → String → PklParseLayout
    field "extend_key" = maybe (∅) ((\ks → (∅) { parseSingletonKeys = [ks]}) ∘ flip SingletonKey (Modifiers Shift [M.Extend])) ∘ parseString
    field "shiftstates" = (\ms → (∅) { parseShiftstates = ms }) ∘ map shiftstateFromWinShiftstate ∘ catMaybes ∘ map readMaybe ∘ shiftstates
    field _ = const (∅)

shiftstates ∷ String → [String]
shiftstates "" = []
shiftstates xs =
  case break (≡':') xs of
    (s, [])   → [s]
    (s, _:ss) → s : shiftstates ss

layoutSection ∷ (Logger m, Parser m) ⇒ m [Key]
layoutSection = catMaybes <$> many (try key <|> Nothing <$ unknownLine)
  where
    unknownLine = lookAhead (noneOf ['[']) *> eol

key ∷ (Logger m, Parser m) ⇒ m (Maybe Key)
key = runMaybeT $ do
    pos ← parseScancode =<< lift (oneOf ['s','S'] *> oneOf ['c','C'] *> some hexDigitChar)
    void ∘ lift $ spacing *> char '=' *> spacing
    shortcutPos ← parseShortcutPos =<< lift (some (noneOf ['\t','\r','\n']) <* tab)
    capslock ← parseCapslock =<< lift (some (noneOf ['\t','\r','\n']) <* tab)
    letters ← traverse parseLetter ∘ takeWhile (not ∘ isComment) =<< lift (some (noneOf ['\t','\r','\n']) `sepBy` tab)
    void ∘ lift $ eol *> emptyOrCommentLines
    pure (Key pos (Just shortcutPos) [] letters (Just capslock))
  where
    isComment (';':_:_) = True
    isComment _         = False

parseScancode ∷ Logger m ⇒ String → MaybeT m Pos
parseScancode xs =
    maybe e1 pure (readMaybe ('0':'x':xs)) >>=
    maybe e2 pure ∘ (`lookupR` posAndScancode)
  where
    e1 = tellMaybeT ["‘" ⊕ xs ⊕ "’ is not an number"]
    e2 = tellMaybeT ["unknown position ‘" ⊕ xs ⊕ "’"]

parseShortcutPos ∷ Logger m ⇒ String → MaybeT m Pos
parseShortcutPos xs = maybe e pure (lookupR xs posAndVkString <|> parseString xs)
  where e = tellMaybeT ["unknown position ‘" ⊕ xs ⊕ "’"]

parseCapslock ∷ Logger m ⇒ String → MaybeT m Bool
parseCapslock xs = maybe e pure (readMaybe xs >>= intToCapsBool)
  where
    e = tellMaybeT ["‘" ⊕ xs ⊕ "’ is not a boolean value"]
    intToCapsBool ∷ Int → Maybe Bool
    intToCapsBool 0 = Just False
    intToCapsBool 1 = Just True
    intToCapsBool 4 = Just False
    intToCapsBool 5 = Just True
    intToCapsBool _ = Nothing

parseLetter ∷ Logger m ⇒ String → m Letter
parseLetter "--" = pure LNothing
parseLetter s' = maybe (LNothing <$ tell ["unknown letter ‘" ⊕ s' ⊕ "’"]) pure $ asum
    [ parseSpace s
    , parseDead s
    , parseAction s
    , parseChars s
    ]
  where
    s = fromMaybe s' (drop 1 >>> stripPrefix "{" >=> stripSuffix "}" $ s')
    parseSpace "space" = Just (Char ' ')
    parseSpace "Space" = Just (Char ' ')
    parseSpace _       = Nothing
    parseChars []  = Just LNothing
    parseChars [x] = Just (Char x)
    parseChars xs  = Just (Ligature Nothing xs)
    parseDead ('d':'k':xs) = CustomDead ∘ Just <$> readMaybe xs <*> pure (DeadKey "" BaseNo [])
    parseDead _ = Nothing
    parseAction = fmap Action ∘ asum ∘ map (`lookupR` actionAndPklAction) ∘ ap [Simple] ∘ pure

deadkeySection ∷ Parser m ⇒ m DeadKey
deadkeySection = do
    deadkeyMap ← many deadkeyValue
    let name = fromMaybe "" (listToMaybe deadkeyMap >>= resultToString ∘ snd)
    let c = maybe BaseNo BaseChar (listToMaybe name)
    pure (DeadKey name c deadkeyMap)
  where
    resultToString (OutString s) = Just s
    resultToString _ = Nothing

deadkeyValue ∷ Parser m ⇒ m (Letter, ActionResult)
deadkeyValue = do
    from ← readMaybe <$> many digitChar
    void $ spacing >> char '=' >> spacing
    to ← readMaybe <$> many digitChar
    void endLine
    maybe (fail "could not parse deadkey") pure $
        (,) <$> (Char ∘ chr <$> from) <*> ((OutString ∘ (:[]) ∘ chr) <$> to)

extendSection ∷ (Logger m, Parser m) ⇒ m PklParseLayout
extendSection = many nameValue >>= traverse (uncurry field) <&> (\ks → (∅)
                    { parseShiftstates = [WP.singleton M.Extend]
                    , parseKeys = catMaybes ks
                    })
  where
    field (s:c:code) l
        | s ∈ "sS" ∧ c ∈ "cC" = runMaybeT $
              Key
                <$> parseScancode code
                <*> pure Nothing
                <*> pure []
                <*> ((:[]) <$> parseLetter l)
                <*> pure Nothing
    field _ xs = Nothing <$ tell ["unknown letter ‘" ⊕ xs ⊕ "’"]

spacing ∷ Parser m ⇒ m String
spacing = many (oneOf [' ','\t'])

endLine ∷ Parser m ⇒ m String
endLine = manyTill anySingle (try eol) <* emptyOrCommentLines

comment ∷ Parser m ⇒ m String
comment = spacing *> char ';' *> manyTill anySingle (try eol)

emptyLine ∷ Parser m ⇒ m String
emptyLine = spacing <* eol

emptyOrCommentLines ∷ Parser m ⇒ m [String]
emptyOrCommentLines = many (try emptyLine <|> comment)

parsePklLayout ∷ Logger m ⇒ String → L.Text → Either String (m Layout)
parsePklLayout fname =
    parse (runWriterT (emptyOrCommentLines *> layout <* eof)) fname >>>
    bimap errorBundlePretty writer
