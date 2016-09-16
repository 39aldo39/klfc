{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}

module PklParse
    ( parsePklLayout
    ) where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR, stripSuffix)
import qualified WithPlus as WP (singleton)

import Control.Monad.Writer (tell)
import qualified Data.Text.Lazy as L (Text)
import Lens.Micro.Platform (set)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Text.Lazy (Parser)

import FileType (FileType)
import qualified Layout.Action as A
import Layout.Key (Key(Key))
import Layout.Layout (Layout(Layout))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux (posAndScancode)
import Lookup.Windows

layout ∷ Parser (Logger Layout)
layout = flip fmap pklLayout $ \logger → do
    PklParseLayout info states keys specialKeys deads ← logger
    keys &
      ( map (set _shiftstates states) >>>
        Layout info specialKeys (∅) >>>
        setDeads deads
      )

setDeads ∷ [(Int, DeadKey)] → Layout → Logger Layout
setDeads = _keys ∘ traverse ∘ _letters ∘ traverse ∘ setDeadKey

setDeadKey ∷ [(Int, DeadKey)] → Letter → Logger Letter
setDeadKey deads (CustomDead (Just i) _) =
  case find ((≡) i ∘ fst) deads of
    Just (_, d) → pure (CustomDead (Just i) d)
    Nothing → CustomDead (Just i) (DeadKey "" Nothing []) <$ tell ["deadkey" ⊕ show i ⊕ " is not defined"]
setDeadKey _ l = pure l

data PklParseLayout = PklParseLayout
    { parseInformation ∷ Information
    , parseShiftstates ∷ [Shiftstate]
    , parseKeys ∷ [Key]
    , parseSingletonKeys ∷ [SingletonKey]
    , parseDeadKeys ∷ [(Int, DeadKey)]
    }
instance Monoid PklParseLayout where
    mempty = PklParseLayout (∅) (∅) (∅) (∅) (∅)
    PklParseLayout a1 a2 a3 a4 a5 `mappend` PklParseLayout b1 b2 b3 b4 b5 =
        PklParseLayout (a1 ⊕ b1) (a2 ⊕ b2) (a3 ⊕ b3) (a4 ⊕ b4) (a5 ⊕ b5)

pklLayout ∷ Parser (Logger PklParseLayout)
pklLayout = fmap mconcat ∘ sequence <$> many ((sectionName >>= section) <* many nonSectionLine)
  where
    sectionName = char '[' *> manyTill anyChar (char ']') <* endLine
    section ∷ String → Parser (Logger PklParseLayout)
    section "informations" = fmap (\l → (∅) { parseInformation = l }) <$> informationsSection
    section "global" = pure <$> globalSection
    section "layout" = pure ∘ (\ks → (∅) { parseKeys = ks }) <$> layoutSection
    section "extend" = pure <$> extendSection
    section xs
        | "deadkey" `isPrefixOf` xs ∧ isJust num =
              pure ∘ (\d → (∅) { parseDeadKeys = [(fromJust num, d)] }) <$> deadkeySection
        | otherwise = pure ((∅) <$ tell ["unknown section ‘" ⊕ xs ⊕ "’"])
        where num = readMaybe (drop 7 xs) ∷ Maybe Int
    nonSectionLine = lookAhead (noneOf "[") *> many (noneOf "\r\n") *> eol

informationsSection ∷ Parser (Logger Information)
informationsSection = fmap mconcat ∘ traverse (uncurry field) <$> many nameValue
  where
    field ∷ String → String → Logger Information
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

nameValue ∷ Parser (String, String)
nameValue = liftA2 (,)
            (lookAhead (noneOf "[") *> some (noneOf "= \t"))
            (spacing *> char '=' *> spacing *> endLine)

globalSection ∷ Parser PklParseLayout
globalSection = mconcat ∘ map (uncurry field) <$> many nameValue
  where
    field ∷ String → String → PklParseLayout
    field "extend_key" = maybe (∅) ((\ks → (∅) { parseSingletonKeys = [ks]}) ∘ flip (,) (Action A.Extend)) ∘ parseString
    field "shiftstates" = (\ms → (∅) { parseShiftstates = ms }) ∘ map shiftstateFromWinShiftstate ∘ catMaybes ∘ map readMaybe ∘ shiftstates
    field _ = const (∅)

shiftstates ∷ String → [String]
shiftstates "" = []
shiftstates xs =
  case break (≡':') xs of
    (s, [])   → [s]
    (s, _:ss) → s : shiftstates ss

layoutSection ∷ Parser [Key]
layoutSection = catMaybes <$> many (Just <$> try key <|> Nothing <$ unknownLine)
  where
    unknownLine = lookAhead (noneOf "[") *> eol

key ∷ Parser Key
key = do
    void $ oneOf "sS" *> oneOf "cC"
    pos ← some hexDigitChar >>= either fail pure ∘ parseScancode
    void $ spacing *> char '=' *> spacing
    shortcutPos ← Just <$> valueWith parseShortcutPos
    capslock ← Just <$> valueWith parseCapslock
    letters ← takeWhile (not ∘ isComment) <$> some (noneOf "\t\r\n") `sepBy` tab >>=
              either fail pure ∘ traverse parseLetter
    void $ eol *> emptyOrCommentLines
    pure (Key pos shortcutPos [] letters capslock)
  where
    valueWith ∷ (String → Either String a) → Parser a
    valueWith f = some (noneOf "\t\r\n") <* tab >>= either fail pure ∘ f
    isComment (';':_:_) = True
    isComment _         = False

parseScancode ∷ String → Either String Pos
parseScancode xs =
    maybe (Left ("‘" ⊕ xs ⊕ "’ is not an number")) Right (readMaybe ('0':'x':xs)) >>=
    maybe (Left ("the position at ‘" ⊕ xs ⊕ "’ is not known")) Right ∘ (`lookupR` posAndScancode)

parseShortcutPos ∷ String → Either String Pos
parseShortcutPos xs =
    maybe (Left ("the position at ‘" ⊕ xs ⊕ "’ is not known")) Right $
    (mplus <$> (`lookupR` posAndString) <*> parseString) xs

parseCapslock ∷ String → Either String Bool
parseCapslock xs = maybe (Left ("‘" ⊕ xs ⊕ "’ is not a boolean value")) pure (readMaybe xs >>= intToCapsBool)
  where
    intToCapsBool ∷ Int → Maybe Bool
    intToCapsBool 0 = Just False
    intToCapsBool 1 = Just True
    intToCapsBool 4 = Just False
    intToCapsBool 5 = Just True
    intToCapsBool _ = Nothing

parseLetter ∷ String → Either String Letter
parseLetter "--" = Right LNothing
parseLetter s' = maybe (Left ("unknown letter ‘" ⊕ s' ⊕ "’")) Right $ asum
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
    parseDead ('d':'k':xs) = CustomDead ∘ Just <$> readMaybe xs <*> pure (DeadKey "" Nothing [])
    parseDead _ = Nothing
    parseAction = fmap Action ∘ asum ∘ map (`lookupR` actionAndPklAction) ∘ ap [Simple] ∘ pure

deadkeySection ∷ Parser DeadKey
deadkeySection = do
    deadkeyMap ← many deadkeyValue
    let name = maybe "" snd (listToMaybe deadkeyMap)
    let c = listToMaybe name
    pure (DeadKey name c deadkeyMap)

deadkeyValue ∷ Parser (String, String)
deadkeyValue = do
    from ← readMaybe <$> many digitChar
    void $ spacing >> char '=' >> spacing
    to ← readMaybe <$> many digitChar
    void endLine
    maybe (fail "could not parse deadkey") pure $
        (,) <$> fmap (pure ∘ chr) from <*> fmap (pure ∘ chr) to

extendSection ∷ Parser PklParseLayout
extendSection = (\ks → (∅)
                    { parseShiftstates = [WP.singleton M.Extend]
                    , parseKeys = ks
                    }) ∘ rights ∘ map (uncurry field)
                <$> many nameValue
  where
    field (s:c:code) l
        | s ∈ "sS" ∧ c ∈ "cC" =
              Key
                <$> parseScancode code
                <*> pure Nothing
                <*> pure []
                <*> ((:[]) <$> parseLetter l)
                <*> pure Nothing
    field _ xs = Left ("unknown letter ‘" ⊕ xs ⊕ "’")

spacing ∷ Parser String
spacing = many (oneOf " \t")

endLine ∷ Parser String
endLine = manyTill anyChar (try eol) <* emptyOrCommentLines

comment ∷ Parser String
comment = spacing *> char ';' *> manyTill anyChar (try eol)

emptyLine ∷ Parser String
emptyLine = spacing <* eol

emptyOrCommentLines ∷ Parser [String]
emptyOrCommentLines = many (try emptyLine <|> comment)

parsePklLayout ∷ String → L.Text → Either String (Logger (FileType → Layout))
parsePklLayout fname =
    parse (emptyOrCommentLines *> layout <* eof) fname >>>
    bimap parseErrorPretty (fmap const)
