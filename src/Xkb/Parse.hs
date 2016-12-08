{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Xkb.Parse where

import BasePrelude hiding (try)
import Prelude.Unicode
import Data.Monoid.Unicode ((∅), (⊕))
import Util (parseString, lookupR, whenNothing, (>$>))
import qualified WithPlus as WP (fromList, singleton)

import Control.Monad.Trans (liftIO)
import Control.Monad.Writer (MonadWriter, writer, runWriterT, tell)
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Lens.Micro.Platform (set, over, _2, _Left)
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory, splitFileName)
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Prim (MonadParsec)
import qualified Text.Megaparsec.String as S (Parser)
import Text.Megaparsec.Text.Lazy (Parser)

import Layout.Key (Key(Key))
import qualified Layout.Modifier as M
import Layout.Types
import Lookup.Linux

layout ∷ FilePath → Parser (String, LoggerT IO Layout)
layout dir = liftA2 (,)
    (manyTill (void xkbName <|> ws1) (char '"') *> xkbName <* char '"')
    (ws *> char '{' *> ws *> xkbLines dir <* char '}' <* ws <* char ';' <* ws)

xkbLines ∷ FilePath → Parser (LoggerT IO Layout)
xkbLines dir = fmap mconcat ∘ sequence <$> many (xkbLine dir)

xkbLine ∷ FilePath → Parser (LoggerT IO Layout)
xkbLine dir = pure (∅) <$ keySetting
    <|> fmap (flip (set _keys) (∅) ∘ maybeToList) ∘ writer <$> runWriterT key
    <|> include dir
    <|> pure (∅) <$ groupName
    <|> pure (∅) <$ modifierMap
    <|> pure (∅) <$ virtualModifiers

groupName ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m String
groupName = keyDescription (string' "name") *> char '"' *> manyTill anyChar (char '"') <* ws <* char ';' <* ws

key ∷ LoggerT Parser (Maybe Key)
key = do
    void $ optional (string' "replace" *> ws) *>
           optional (string' "override" *> ws) *>
           string' "key" *> ws
    pos ← position
    void $ char '{' *> ws
    letters ← fromMaybe [] ∘ asum <$> xkbLetters `sepBy` comma
    let shiftstates' = shiftstates letters
    let letters' = catMaybes letters
    void $ char '}' *> ws *> char ';' *> ws
    case pos of
        Left  s → Nothing <$ tell [s]
        Right p → pure ∘ Just $ Key p Nothing shiftstates' letters' Nothing

shiftstates ∷ [Maybe Letter] → [Shiftstate]
shiftstates = catMaybes ∘ zipWith (<$)
    [ (∅)
    , WP.singleton M.Shift
    , WP.singleton M.AltGr
    , WP.fromList [M.AltGr, M.Shift]
    , WP.singleton M.Extend
    , WP.fromList [M.Extend, M.Shift]
    , WP.fromList [M.Extend, M.AltGr]
    , WP.fromList [M.Extend, M.AltGr, M.Shift]
    ]

xkbLetters ∷ LoggerT Parser (Maybe [Maybe Letter])
xkbLetters = asum
  [ Just <$> symbols
  , Just <$> (keyDescription (string' "symbols") *> symbols)
  , Nothing <$ (keyDescription (string' "actions") *> actions)
  , Nothing <$ (keyDescription (string' "type") *>
        char '"' *> many (noneOf "\"") <* char '"' <* ws)
  , Nothing <$ (keyDescription (string' "locks" <|> string' "locking" <?> "\"locks\"") *>
        boolean <* ws)
  , Nothing <$ (keyDescription (string' "repeat" <|> string' "repeats" <|> string' "repeating" <?> "repeat") *>
        (void boolean <|> void (string' "default")) <* ws)
  , Nothing <$ (keyDescription (string' "vmods" <|> string' "virtualmods" <|> string' "virtualmodifiers" <?> "\"vmods\"") *>
        xkbName)
  , Nothing <$ (keyDescription (string' "overlay" *> some digitChar) *>
        xkbName)
  ]

keyDescription ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m α → m α
keyDescription s = s <* ws <*
                   optional (char '[' *> ws *> xkbName <* ws <* char ']') <*
                   ws <* char '=' <* ws

symbols ∷ LoggerT Parser [Maybe Letter]
symbols =
    char '[' *> ws *> keysym `sepBy` comma <* char ']' <* ws

actions ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m [(String, String)]
actions = char '[' *> ws *> action `sepBy` comma <* char ']' <* ws

action ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m (String, String)
action = liftA2 (,)
         (some alphaNumChar)
         (char '(' *> many (alphaNumChar <|> oneOf " _,+-=<>!") <* char ')' <* ws)

comma ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m ()
comma = char ',' *> ws

position ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m (Either String Pos)
position = parsePos <$> xkbName <?> ""

parsePos ∷ String → Either String Pos
parsePos s = maybe (Left ("unknown position ‘" ⊕ s ⊕ "’")) Right $
    lookupR s posAndKeycode

keysym ∷ LoggerT Parser (Maybe Letter)
keysym = xkbName >>= parseLetter

parseLetter ∷ MonadWriter [String] m ⇒ String → m (Maybe Letter)
parseLetter "NoSymbol" = pure Nothing
parseLetter "VoidSymbol" = pure (Just LNothing)
parseLetter [x] = pure ∘ Just $ Char x
parseLetter ('U':xs)
    | Just c ← readMaybe ('0':'x':xs)
    = pure ∘ Just $ Char (chr c)
parseLetter ('0':'x':xs)
    | Just c ← readMaybe ('0':'x':xs)
    , c' ← fromMaybe c $ mfilter (≥ 0) (Just (c - 0x1000000))
    = pure ∘ Just $ Char (chr c')
parseLetter xs = whenNothing (tell ["unknown letter ‘" ⊕ xs ⊕ "’"]) $
    asum [ Char <$> lookupR xs charAndString
         , Dead <$> lookupR xs deadKeysAndLinuxDeadKeys
         , Action <$> lookupR xs (map (over _2 __symbol) actionAndLinuxAction)
         , parseString xs
         ]

include ∷ FilePath → Parser (LoggerT IO Layout)
include dir = getInclude dir <$>
    (string' "include" *> ws *> char '"' *> some (noneOf "\"") <* char '"' <* ws)

getInclude ∷ FilePath → String → LoggerT IO Layout
getInclude dir name
  | Just singletonKeys ← parseSingletonKey name = pure $
        set _singletonKeys singletonKeys (∅)
  | Just (file, variant) ← parseXkbFilePath name = do
        fnameMaybe ← liftIO $ getFilePath file
        case fnameMaybe of
          Just fname → do
            text ← liftIO $ L.readFile fname
            either (\s → (∅) <$ tell [s]) id $ parseXkbLayoutVariant variant fname text
          Nothing → (∅) <$ tell ["could not find the symbols file ‘" ⊕ file ⊕ "’"]
  | otherwise =
        (∅) <$ tell ["could not parse include string ‘" ⊕ name ⊕ "’"]
  where
    getFilePath file = listToMaybe <$> filterM doesFileExist (dirs file)
    dirs file = map (</> file) [dir, "/usr/share/X11/xkb/symbols"]

parseSingletonKey ∷ String → Maybe [SingletonKey]
parseSingletonKey xs = asum
    [ pure <$> lookupR xs' singleIncludes
    , (\(x,y) → [x,y]) <$> lookupR xs' doubleIncludes
    , [] <$ lookupR xs' modMappingIncludes
    ]
  where xs' = "include \"" ⊕ xs ⊕ "\""

keySetting ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m ()
keySetting = string' "key." *> manyTill anyChar (char ';') *> ws *> pure ()

modifierMap ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m (String, [String])
modifierMap = liftA2 (,)
              (string' "modifier_map" *> ws *> xkbName <* ws)
              (char '{' *> ws *> xkbName `sepBy` comma <* char '}' <* ws <* char ';' <* ws)

virtualModifiers ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m [String]
virtualModifiers = string' "virtual_modifiers" *> ws *> xkbName `sepBy` comma <* char ';' <* ws

xkbName ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m String
xkbName = some (alphaNumChar <|> oneOf "_+-<>()") <* ws

ws ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m ()
ws = void $ many singleSpace

ws1 ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m ()
ws1 = void (some singleSpace) <?> "white space"

comment ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m ()
comment = void $ (string "//" <|> string "#") *> manyTill anyChar eol

singleSpace ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m ()
singleSpace = void (some spaceChar) <|> comment <?> ""

boolean ∷ (Token s ~ Char, MonadParsec e s m) ⇒ m Bool
boolean = True <$ (string' "true" <|> string' "yes" <|> string' "on")
   <|> False <$ (string' "false" <|> string' "no" <|> string' "off")

parseXkbFilePath ∷ String → Maybe (String, String)
parseXkbFilePath = parseMaybe $ liftA2 (,)
    (many (noneOf "()") ∷ S.Parser String)
    (fromMaybe "basic" <$> optional (char '(' *> many (noneOf "()") <* char ')'))

getFileAndVariant ∷ FilePath → (FilePath, String)
getFileAndVariant fname = (dir </> file, variant)
  where
    (dir, name) = splitFileName fname
    (file, variant) = fromMaybe (fname, "basic") $ parseXkbFilePath name

parseXkbLayoutVariant ∷ String → FilePath → L.Text → Either String (LoggerT IO Layout)
parseXkbLayoutVariant variant fname =
    parse (many (layout (takeDirectory fname)) <* eof) fname >>>
    over _Left parseErrorPretty >$>
    fromMaybe unknownVariant ∘ lookup variant
  where
    unknownVariant = (∅) <$ tell [fname ⊕ ": unknown layout variant ‘" ⊕ variant ⊕ "’"]
